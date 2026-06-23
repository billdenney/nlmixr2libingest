# Model-specific naming pre-brief: resolve the covariates/parameters/compartments
# a paper actually uses to their canonical nlmixr2lib names ONCE, up front, so the
# agent neither loads the ~136k-token covariate register nor spends a turn per
# lookup. The agent ingests one small report (~1-2k tokens) and proceeds.
#
# Candidate terms come from two independent sources, unioned for recall:
#   - deterministic: scan the paper for any register canonical name / source
#     alias that appears (no LLM; high precision).
#   - LLM (optional): distill_paper() names paper-phrased covariates the
#     deterministic scan misses (e.g. "creatinine clearance" -> CRCL).
# Each candidate is resolved with lookup_canonical(). Unmatched candidates are
# surfaced explicitly -- they may be genuinely new covariates to ratify.
#
# This is a PRIOR, not a gate (quality firewall): the agent still source-traces
# every covariate VALUE against the paper; the pre-brief only fixes the naming.

.pbReadText <- function(paper) {
  if (length(paper) == 1L && !grepl("\n", paper) && file.exists(paper)) {
    return(paste(readLines(paper, warn = FALSE), collapse = " "))
  }
  paste(paper, collapse = " ")
}

.pbEsc <- function(x) gsub("([.\\\\+*?\\[\\]^$(){}|-])", "\\\\\\1", x, perl = TRUE)

# Split a "source aliases" field into individual, parenthetical-stripped terms.
.pbAliasTerms <- function(s) {
  if (is.na(s) || !nzchar(s)) return(character())
  s <- gsub("\\(.*?\\)", "", s)
  terms <- trimws(strsplit(s, "[,;/]")[[1L]])
  terms[nzchar(terms)]
}

# Canonical names whose name or an alias appears as a whole word in the paper.
.pbDeterministic <- function(txt, kind, dir) {
  reg <- tryCatch(parse_registers(dir), error = function(e) NULL)
  if (is.null(reg)) return(character())
  reg <- reg[reg$kind == kind, , drop = FALSE]
  if (!nrow(reg)) return(character())
  low <- tolower(txt)
  hit <- logical(nrow(reg))
  for (i in seq_len(nrow(reg))) {
    terms <- c(reg$name[i], .pbAliasTerms(reg$source_aliases[i]))
    terms <- unique(terms[nchar(terms) >= 2L])
    for (t in terms) {
      if (grepl(paste0("\\b", .pbEsc(tolower(t)), "\\b"), low, perl = TRUE)) {
        hit[i] <- TRUE; break
      }
    }
  }
  reg$name[hit]
}

.pbCol <- function(x, col) {
  if (is.data.frame(x) && col %in% names(x)) return(as.character(x[[col]]))
  if (is.list(x) && length(x)) {
    return(as.character(unlist(lapply(x, function(e)
      if (is.list(e)) e[[col]] else NULL))))
  }
  character()
}

# Paper-phrased candidate terms from a distillation sheet (LLM-optional).
.pbLLM <- function(txt, kind, backend) {
  sheet <- tryCatch(distill_paper(txt, backend = backend), error = function(e) NULL)
  if (is.null(sheet)) return(character())
  terms <- switch(kind,
    covariate = .pbCol(sheet$covariate_effects, "covariate"),
    parameter = .pbCol(sheet$parameters, "name"),
    compartment = {
      st <- sheet$structure
      if (!is.null(st) && !is.null(st$compartments)) as.character(st$compartments)
      else character()
    },
    character())
  unique(trimws(terms[nzchar(terms)]))
}

# Resolve one kind: union the candidate sources, look each up, split matched vs
# unmatched.
.pbResolveKind <- function(txt, kind, backend, top_k, dir, db_path) {
  det <- .pbDeterministic(txt, kind, dir)
  llm <- if (identical(backend, "none")) character() else .pbLLM(txt, kind, backend)
  src <- c(stats::setNames(rep("register", length(det)), det),
           stats::setNames(rep("llm", length(llm)), llm))
  terms <- unique(names(src))
  matched <- list(); unmatched <- character()
  for (t in terms) {
    if (!nzchar(t)) next
    hit <- tryCatch(
      lookup_canonical(t, kind = kind, top_k = top_k, db_path = db_path, dir = dir),
      error = function(e) NULL)
    if (is.null(hit) || !nrow(hit)) { unmatched <- c(unmatched, t); next }
    r <- hit[1L, ]
    matched[[length(matched) + 1L]] <- data.frame(
      paper_term = t, source = src[[t]], canonical = r$name,
      units = if (is.na(r$units)) NA_character_ else r$units,
      scope = if (is.null(r$scope) || is.na(r$scope)) NA_character_ else r$scope,
      role = if (is.null(r$role) || is.na(r$role)) NA_character_ else r$role,
      notes = if (is.na(r$notes)) NA_character_ else r$notes,
      stringsAsFactors = FALSE)
  }
  md <- if (length(matched)) do.call(rbind, matched) else
    data.frame(paper_term = character(), source = character(),
               canonical = character(), units = character(), scope = character(),
               role = character(), notes = character(), stringsAsFactors = FALSE)
  # Collapse rows that resolved to the same canonical name from both sources.
  if (nrow(md)) {
    md <- md[order(md$canonical, md$paper_term), , drop = FALSE]
    dup <- duplicated(md$canonical)
    md$source[md$canonical %in% md$canonical[dup]] <- "both"
    md <- md[!duplicated(md$canonical), , drop = FALSE]
    rownames(md) <- NULL
  }
  list(matched = md, unmatched = unique(unmatched))
}

#' Build a model-specific naming pre-brief from a paper
#'
#' Resolves the covariates (and optionally parameters/compartments) a paper uses
#' to their canonical `nlmixr2lib` names up front, so the agent ingests one small
#' report instead of loading the register or looking terms up turn-by-turn.
#' Candidates come from a deterministic register scan plus, when a local LLM is
#' configured, [distill_paper()]; each is resolved with [lookup_canonical()].
#' Unmatched candidates (possible new canonical names) are flagged. This is a
#' prior, not a gate: the agent still source-traces every value against the paper.
#'
#' @param paper Paper text, or a path to a (trimmed) paper file.
#' @param kinds Which register kinds to resolve: any of `"covariate"`,
#'   `"parameter"`, `"compartment"`.
#' @param backend LLM backend for the candidate-augmenting distillation
#'   (`"ollama"`/`"none"`/`"auto"`); `"none"` uses the deterministic scan only.
#' @param top_k Rows to consider per lookup (the best is used).
#' @param dir Register directory (defaults to [registers_dir()]).
#' @param db_path Lookup index location (defaults to [register_db_path()]).
#' @return An `nli_prebrief` list: one entry per kind (`matched`/`unmatched`),
#'   plus `backend` and `n` (matched count). Render with [render_prebrief()].
#' @export
naming_prebrief <- function(paper, kinds = "covariate",
                            backend = .llmBackend(), top_k = 3L,
                            dir = registers_dir(), db_path = register_db_path()) {
  kinds <- match.arg(kinds, c("covariate", "parameter", "compartment"),
                     several.ok = TRUE)
  txt <- .pbReadText(paper)
  out <- list()
  n <- 0L
  for (k in kinds) {
    res <- .pbResolveKind(txt, k, backend, top_k, dir, db_path)
    out[[k]] <- res
    n <- n + nrow(res$matched)
  }
  out$backend <- backend
  out$n <- n
  structure(out, class = "nli_prebrief")
}

#' Render a naming pre-brief as compact markdown
#'
#' @param x An `nli_prebrief` from [naming_prebrief()].
#' @return The markdown string, invisibly; also printed.
#' @export
render_prebrief <- function(x) {
  stopifnot(inherits(x, "nli_prebrief"))
  kinds <- setdiff(names(x), c("backend", "n"))
  lines <- c("# Naming pre-brief (prior only -- source-trace every value)")
  for (k in kinds) {
    md <- x[[k]]$matched
    un <- x[[k]]$unmatched
    lines <- c(lines, "", sprintf("## %ss (%d resolved)", k, nrow(md)))
    if (nrow(md)) {
      for (i in seq_len(nrow(md))) {
        r <- md[i, ]
        tag <- paste(stats::na.omit(c(r$units, r$scope, r$role)), collapse = ", ")
        tag <- if (nzchar(tag)) paste0(" [", tag, "]") else ""
        arrow <- if (identical(tolower(r$paper_term), tolower(r$canonical)))
          r$canonical else sprintf("%s -> %s", r$paper_term, r$canonical)
        lines <- c(lines, sprintf("- %s%s", arrow, tag))
      }
    } else {
      lines <- c(lines, "_none detected_")
    }
    if (length(un)) {
      lines <- c(lines,
                 sprintf("- **unmatched (verify; may need a new canonical name):** %s",
                         paste(un, collapse = ", ")))
    }
  }
  out <- paste(lines, collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

#' @export
print.nli_prebrief <- function(x, ...) {
  kinds <- setdiff(names(x), c("backend", "n"))
  cli::cli_h2("Naming pre-brief ({x$n} resolved; backend: {x$backend})")
  for (k in kinds) {
    md <- x[[k]]$matched
    un <- length(x[[k]]$unmatched)
    cli::cli_text("{k}: {nrow(md)} resolved{if (un) paste0(', ', un, ' unmatched') else ''}")
  }
  cli::cli_text("{.emph Prior only -- the agent still source-traces every value.}")
  invisible(x)
}
