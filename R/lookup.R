# DuckDB-backed lookup over the nlmixr2lib canonical-name registers.
#
# The 1.14 MB covariate register (~284k tokens) must never enter an agent's
# context. build_register_index() parses the three registers into a DuckDB file
# with a full-text index; lookup_canonical() returns the few relevant rows.
# A source-freshness check rebuilds the index only when a register file actually
# changes, with an in-session TTL so the file stat is not repeated every call.

# Per-session record of when each db_path was last validated against source.
.regValidated <- new.env(parent = emptyenv())

.registerTtl <- function() getOption("nlmixr2libingest.register_ttl", 86400)

.connect <- function(db_path) {
  d <- dirname(db_path)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
}

# Is the on-disk index in sync with the current register files? Compares the
# cheap size+mtime first; only hashes files whose size/mtime moved.
.indexIsFresh <- function(db_path, dir) {
  if (!file.exists(db_path)) return(FALSE)
  cur <- .registerSignature(.registerPaths(dir))
  fresh <- tryCatch({
    con <- .connect(db_path)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    if (!DBI::dbExistsTable(con, "register_meta")) return(FALSE)
    stored <- DBI::dbReadTable(con, "register_meta")
    stored <- stored[match(cur$kind, stored$kind), , drop = FALSE]
    if (anyNA(stored$kind)) return(FALSE)
    fast <- isTRUE(all.equal(cur$size, stored$size)) &&
      isTRUE(all.equal(cur$mtime, stored$mtime))
    if (fast) return(TRUE)
    # size/mtime moved on at least one file: fall back to the content hash.
    isTRUE(all.equal(cur$md5, stored$md5))
  }, error = function(e) FALSE)
  isTRUE(fresh)
}

# Build the index unless it is already fresh (TTL-gated to skip the stat).
.ensureIndex <- function(db_path, dir) {
  key <- normalizePath(db_path, mustWork = FALSE)
  last <- .regValidated[[key]]
  if (!is.null(last) && file.exists(db_path) &&
      as.numeric(Sys.time() - last, units = "secs") < .registerTtl()) {
    return(invisible(FALSE))
  }
  if (.indexIsFresh(db_path, dir)) {
    .regValidated[[key]] <- Sys.time()
    return(invisible(FALSE))
  }
  build_register_index(db_path = db_path, dir = dir, quiet = TRUE)
  invisible(TRUE)
}

#' Build (or rebuild) the canonical-name register index
#'
#' Parses the three `nlmixr2lib` registers into a DuckDB file with a full-text
#' index plus a source signature (size, mtime, md5) used for freshness checks.
#' If the DuckDB `fts` extension cannot be loaded (e.g. offline), the index is
#' still built and lookups fall back to token `LIKE` matching.
#'
#' @param db_path DuckDB file to write. Defaults to [register_db_path()].
#' @param dir Register directory. Defaults to [registers_dir()].
#' @param quiet Suppress the FTS-unavailable warning.
#' @return Invisibly, the parsed register data frame.
#' @export
build_register_index <- function(db_path = register_db_path(),
                                 dir = registers_dir(), quiet = FALSE) {
  df <- parse_registers(dir)
  sig <- .registerSignature(.registerPaths(dir))
  unlink(c(db_path, paste0(db_path, ".wal")))
  con <- .connect(db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "registers", df, overwrite = TRUE)
  DBI::dbWriteTable(con, "register_meta", sig, overwrite = TRUE)
  fts_ok <- tryCatch({
    DBI::dbExecute(con, "INSTALL fts;")
    DBI::dbExecute(con, "LOAD fts;")
    DBI::dbExecute(con, paste(
      "PRAGMA create_fts_index('registers', 'id', 'text',",
      "stemmer='porter', stopwords='english', overwrite=1);"))
    TRUE
  }, error = function(e) {
    if (!quiet) {
      cli::cli_warn(c("DuckDB FTS unavailable; using LIKE fallback.",
                      i = conditionMessage(e)))
    }
    FALSE
  })
  DBI::dbWriteTable(con, "register_build", data.frame(
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    fts_available = as.integer(fts_ok),
    n_entries = nrow(df),
    stringsAsFactors = FALSE
  ), overwrite = TRUE)
  .regValidated[[normalizePath(db_path, mustWork = FALSE)]] <- Sys.time()
  invisible(df)
}

.kindClause <- function(con, kind) {
  if (is.null(kind) || !nzchar(kind)) return("")
  paste("AND kind =", DBI::dbQuoteString(con, as.character(kind)))
}

# Exact name and alias-substring matches, scored so exact name > prefix/substr >
# alias/description. For multi-word concept queries ("body weight") a full-phrase
# match in `description` is also a first-tier hit, so the canonical whose
# definition contains the phrase outranks bm25 noise. Single-word queries skip
# this (a common word like "clearance" would flood the tier).
.exactAlias <- function(con, term, kind, include_desc = FALSE) {
  tq <- DBI::dbQuoteString(con, term)
  lq <- DBI::dbQuoteString(con, paste0("%", term, "%"))
  desc_where <- if (include_desc)
    paste0(" OR lower(coalesce(description, '')) LIKE lower(", lq, ")") else ""
  desc_score <- if (include_desc)
    paste0("WHEN lower(coalesce(description, '')) LIKE lower(", lq, ") THEN 2 ") else ""
  sql <- paste0(
    "SELECT *, CASE WHEN lower(name) = lower(", tq, ") THEN 4 ",
    "WHEN lower(name) LIKE lower(", lq, ") THEN 3 ", desc_score, "ELSE 1 END AS score ",
    "FROM registers WHERE (lower(name) = lower(", tq, ") ",
    "OR lower(name) LIKE lower(", lq, ") ",
    "OR lower(coalesce(source_aliases, '')) LIKE lower(", lq, ")", desc_where, ") ",
    .kindClause(con, kind))
  DBI::dbGetQuery(con, sql)
}

# BM25 full-text ranking over the `text` document.
.ftsQuery <- function(con, term, kind, top_k) {
  DBI::dbExecute(con, "LOAD fts;")
  sql <- paste0(
    "SELECT * FROM (SELECT *, fts_main_registers.match_bm25(id, ",
    DBI::dbQuoteString(con, term), ") AS score FROM registers) ",
    "WHERE score IS NOT NULL ", .kindClause(con, kind),
    " ORDER BY score DESC LIMIT ", as.integer(top_k))
  DBI::dbGetQuery(con, sql)
}

# Token LIKE fallback when FTS is unavailable: score = # query tokens hit.
.likeQuery <- function(con, term, kind, top_k) {
  toks <- unique(tolower(unlist(strsplit(trimws(term), "\\s+"))))
  toks <- toks[nchar(toks) >= 2L]
  if (!length(toks)) toks <- tolower(trimws(term))
  parts <- vapply(toks, function(t) paste0(
    "(CASE WHEN lower(text) LIKE ",
    DBI::dbQuoteString(con, paste0("%", t, "%")), " THEN 1 ELSE 0 END)"), "")
  score_expr <- paste(parts, collapse = " + ")
  sql <- paste0("SELECT *, (", score_expr, ") AS score FROM registers WHERE (",
                score_expr, ") > 0 ", .kindClause(con, kind),
                " ORDER BY score DESC LIMIT ", as.integer(top_k))
  DBI::dbGetQuery(con, sql)
}

#' Look up canonical names in the nlmixr2lib registers
#'
#' Returns the handful of register entries most relevant to `term` without ever
#' loading the multi-hundred-thousand-token register files into context. Exact
#' name and alias matches are surfaced first, then full-text (BM25) or token
#' matches. The long `example_models` list is summarised to a count by default.
#'
#' @param term Search term: a canonical name, an alias, or a free-text concept
#'   (e.g. `"body weight"`).
#' @param kind Optionally restrict to `"covariate"`, `"compartment"`, or
#'   `"parameter"`.
#' @param top_k Maximum rows to return.
#' @param full If `TRUE`, include the full `example_models` list and `text`.
#' @param db_path,dir Index location and register directory.
#' @return A data frame of matching entries (0 rows if none match), ordered by
#'   relevance.
#' @export
lookup_canonical <- function(term, kind = NULL, top_k = 5, full = FALSE,
                             db_path = register_db_path(), dir = registers_dir()) {
  stopifnot(is.character(term), length(term) == 1L, nzchar(term))
  .ensureIndex(db_path, dir)
  con <- .connect(db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  fts_ok <- tryCatch(
    isTRUE(DBI::dbReadTable(con, "register_build")$fts_available[[1L]] == 1L),
    error = function(e) FALSE)
  exact <- .exactAlias(con, term, kind, include_desc = grepl("\\s", trimws(term)))
  fuzzy <- NULL
  if (fts_ok) fuzzy <- tryCatch(.ftsQuery(con, term, kind, top_k), error = function(e) NULL)
  if (is.null(fuzzy)) fuzzy <- .likeQuery(con, term, kind, top_k)
  # Use rep(value, nrow): a scalar assigned to a 0-row frame errors ("replacement
  # has 1 row, data has 0"), but a length-nrow vector is fine. This keeps the
  # rbind() column sets matching even when one side has no matches.
  exact$.tier <- rep(0L, nrow(exact))
  fuzzy$.tier <- rep(1L, nrow(fuzzy))
  combined <- rbind(exact, fuzzy)
  if (!nrow(combined)) {
    return(combined[, setdiff(names(combined), c("text", ".tier", "score")), drop = FALSE])
  }
  combined <- combined[order(combined$.tier, -combined$score), , drop = FALSE]
  combined <- combined[!duplicated(combined$id), , drop = FALSE]
  combined <- utils::head(combined, top_k)
  combined$n_example_models <-
    vapply(combined$example_models, function(x) {
      if (is.na(x)) 0L else lengths(regmatches(x, gregexpr("\\.R\\b", x)))
    }, integer(1L))
  drop <- c("id", ".tier", "score")
  if (!full) drop <- c(drop, "text", "example_models")
  combined[, setdiff(names(combined), drop), drop = FALSE]
}

#' Render lookup results as compact markdown (for CLI / agent consumption)
#'
#' @param df A data frame from [lookup_canonical()].
#' @return The markdown string, invisibly; also printed.
#' @export
render_lookup <- function(df) {
  if (!nrow(df)) {
    out <- "_No matching canonical entries._\n"
    cat(out)
    return(invisible(out))
  }
  blocks <- vapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    tags <- paste(stats::na.omit(c(r$kind, r$type, r$scope)), collapse = ", ")
    lines <- sprintf("### %s  [%s]", r$name, tags)
    if (!is.na(r$description)) lines <- c(lines, r$description)
    kv <- character()
    if (!is.na(r$units)) kv <- c(kv, paste0("units: ", r$units))
    if (!is.null(r$reference_category) && !is.na(r$reference_category)) {
      kv <- c(kv, paste0("ref: ", r$reference_category))
    }
    if (length(kv)) lines <- c(lines, paste(kv, collapse = " | "))
    if (!is.null(r$role) && !is.na(r$role)) lines <- c(lines, paste0("role: ", r$role))
    if (!is.na(r$source_aliases)) lines <- c(lines, paste0("aliases: ", r$source_aliases))
    if (!is.na(r$notes)) lines <- c(lines, paste0("notes: ", r$notes))
    if (!is.null(r$n_example_models)) {
      lines <- c(lines, sprintf("_(%d example model%s)_",
                                r$n_example_models, ifelse(r$n_example_models == 1L, "", "s")))
    }
    paste(lines, collapse = "\n")
  }, character(1L))
  out <- paste0(paste(blocks, collapse = "\n\n"), "\n")
  cat(out)
  invisible(out)
}
