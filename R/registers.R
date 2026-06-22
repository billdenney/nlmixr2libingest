# Parser for the nlmixr2lib canonical-name registers.
#
# All three registers (covariate-columns.md, compartment-names.md,
# parameter-names.md) share one structure:
#   ## H2            -- category grouping (ignored for lookup)
#   ### name (**description**)   -- one canonical entry; canonical name is the
#                                   first whitespace-separated token after '###'
#   - **Field:** value           -- bold-labelled fields; a field value may span
#                                   continuation lines until the next field/heading
#
# Fields seen across the three files: Description, Units, Type, Scope,
# Reference category, Role, Source aliases, Example models, Notes. They are
# normalised to lower_snake column names. Missing fields are NA.

# Normalise a bold field label ("Reference category") to a column name
# ("reference_category").
.normField <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

# Parse a single register file into a list of per-entry named lists.
.parseRegisterFile <- function(path, kind) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  h3 <- grepl("^###[ \t]+", lines)
  starts <- which(h3)
  if (!length(starts)) return(list())
  ends <- c(starts[-1L] - 1L, length(lines))
  out <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    block <- lines[starts[i]:ends[i]]
    heading <- block[[1L]]
    name <- sub("^###[ \t]+", "", heading)
    name <- trimws(sub("[ \t]*\\(.*$", "", name))
    paren <- sub("^###[ \t]+\\S+[ \t]*\\((.*)\\)[ \t]*$", "\\1", heading)
    if (identical(paren, heading)) paren <- "" else paren <- gsub("\\*\\*", "", paren)
    fields <- list()
    cur <- NULL
    for (ln in block[-1L]) {
      m <- regmatches(ln, regexec("^[ \t]*-[ \t]+\\*\\*([^:*]+):\\*\\*[ \t]*(.*)$", ln))[[1L]]
      if (length(m) == 3L) {
        cur <- .normField(m[[2L]])
        fields[[cur]] <- trimws(m[[3L]])
      } else if (!is.null(cur) && nzchar(trimws(ln))) {
        fields[[cur]] <- trimws(paste(fields[[cur]], trimws(ln)))
      }
    }
    fields$.name <- name
    fields$.paren <- paren
    fields$.kind <- kind
    out[[i]] <- fields
  }
  out
}

# Bind a list of per-entry named lists into a tidy, fixed-column data.frame.
.bindRegisterEntries <- function(entries) {
  cols <- c("kind", "name", "description", "units", "type", "scope",
            "reference_category", "role", "source_aliases",
            "example_models", "notes")
  pull <- function(e, f) {
    v <- e[[f]]
    if (is.null(v) || !nzchar(v)) NA_character_ else v
  }
  rows <- lapply(entries, function(e) {
    desc <- pull(e, "description")
    if (is.na(desc)) desc <- if (nzchar(e$.paren)) e$.paren else NA_character_
    data.frame(
      kind = e$.kind,
      name = e$.name,
      description = desc,
      units = pull(e, "units"),
      type = pull(e, "type"),
      scope = pull(e, "scope"),
      reference_category = pull(e, "reference_category"),
      role = pull(e, "role"),
      source_aliases = pull(e, "source_aliases"),
      example_models = pull(e, "example_models"),
      notes = pull(e, "notes"),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df <- df[, cols, drop = FALSE]
  # FTS document: everything a search might want to hit, minus the long
  # example-model lists (which are noise for keyword search).
  df$text <- trimws(apply(
    df[, c("name", "description", "type", "scope", "role",
           "source_aliases", "notes")],
    1L,
    function(r) paste(stats::na.omit(r), collapse = " ")
  ))
  df$id <- seq_len(nrow(df))
  df
}

#' Parse the nlmixr2lib canonical-name registers into a tidy data frame
#'
#' Reads the covariate, compartment, and parameter registers shipped by
#' `nlmixr2lib` and returns one row per canonical entry. This is the
#' deterministic substrate for [build_register_index()] / [lookup_canonical()];
#' it does no validation (that stays in `nlmixr2lib::checkModelConventions()`).
#'
#' @param dir Directory holding the register markdown files. Defaults to
#'   [registers_dir()].
#' @return A data frame with columns `kind`, `name`, `description`, `units`,
#'   `type`, `scope`, `reference_category`, `role`, `source_aliases`,
#'   `example_models`, `notes`, plus `text` (FTS document) and `id`.
#' @export
parse_registers <- function(dir = registers_dir()) {
  paths <- .registerPaths(dir)
  entries <- unlist(
    Map(.parseRegisterFile, paths, names(paths)),
    recursive = FALSE, use.names = FALSE
  )
  .bindRegisterEntries(entries)
}
