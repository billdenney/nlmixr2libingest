# Deterministic filtering of R CMD check / devtools::check and vignette-render
# logs. The build->check->fix loop is the dominant token sink (cache_read x
# turns); a multi-thousand-line check log re-read every turn is the worst case.
#
# These filters are DETERMINISTIC on purpose: an LLM summary that silently drops
# a real ERROR is a quality regression. We keep every non-OK check step in full
# plus the run status, and discard only the "... OK" noise.

# Read input that may be a path or already-split/whole text.
.asLines <- function(x) {
  if (length(x) == 1L && !grepl("\n", x) && file.exists(x)) {
    return(readLines(x, warn = FALSE))
  }
  if (length(x) == 1L) return(strsplit(x, "\n", fixed = TRUE)[[1L]])
  x
}

#' Filter an R CMD check / devtools::check log to the parts that matter
#'
#' Keeps every check step that is not `OK` (NOTE/WARNING/ERROR), in full,
#' together with its indented detail block, plus the run-status summary and any
#' hard build/install failure lines. Drops the `... OK` steps. Nothing that
#' signals a problem is removed.
#'
#' @param x A path to a log file, or the log text.
#' @param keep_notes Keep `NOTE`-level steps (default `TRUE`).
#' @return Invisibly, the filtered lines (character). Printed by default.
#' @export
filter_check_log <- function(x, keep_notes = TRUE) {
  lines <- .asLines(x)
  n <- length(lines)
  if (!n) return(invisible(character()))
  # A check step starts with "* checking" (R CMD check), a cli rule (U+2500), or
  # a devtools problem header (U+276F). Status / results-summary lines also act
  # as boundaries so a passing step is not pulled in by an adjacent summary. The
  # non-ASCII markers are built with intToUtf8() to keep the R source ASCII-only.
  cli_rule <- intToUtf8(0x2500L)
  dt_hdr <- intToUtf8(0x276FL)
  step_start <- grepl("^\\*+ ", lines) | grepl(paste0("^", cli_rule), lines) |
    grepl("^-- ", lines) | grepl(paste0("^", dt_hdr), lines) |
    grepl("^Status:", lines) | grepl("R CMD check results", lines)
  step_idx <- which(step_start)
  keep <- logical(n)
  bad <- c("ERROR", "WARNING", if (keep_notes) "NOTE")
  bad_re <- paste0("\\b(", paste(bad, collapse = "|"), ")\\b")
  # Hard-error lines (any case) also trigger keeping their whole step block, so a
  # devtools problem-header step is retained alongside a lowercase "Error:" body.
  hard <- grepl("^(Error|Execution halted)", lines) | grepl("^\\s*Error(:| in )", lines)
  if (length(step_idx)) {
    bounds <- c(step_idx, n + 1L)
    for (j in seq_along(step_idx)) {
      head_line <- lines[step_idx[j]]
      blk <- step_idx[j]:(bounds[j + 1L] - 1L)
      # Keep the whole block when the step header flags a problem, OR when the
      # block body contains an error/warning token or a hard-error line.
      if (grepl(bad_re, head_line) || any(grepl(bad_re, lines[blk])) || any(hard[blk])) {
        keep[blk] <- TRUE
      }
    }
  }
  # Always keep status / results summary and hard failures, wherever they sit.
  # Count summary is matched case-insensitively to cover the devtools cli form
  # ("0 errors | 1 warning | 0 notes") as well as raw "1 ERROR, 1 NOTE".
  always <- grepl("^Status:", lines) |
    grepl("R CMD check results", lines) |
    grepl("^(Error|Execution halted)", lines) |
    grepl("^\\s*Error(:| in )", lines) |
    grepl("[0-9]+\\s+(error|warning|note)s?\\b", lines, ignore.case = TRUE)
  keep <- keep | always
  out <- lines[keep]
  if (!length(out)) out <- "[rfilter] no ERROR/WARNING/NOTE found (check passed cleanly)."
  cat(out, sep = "\n"); cat("\n")
  invisible(out)
}

#' Filter an rmarkdown / knitr render log to the failure and its context
#'
#' Vignette renders fail with a `Quitting from lines ...` / `Error ...` /
#' `Execution halted` cluster. This returns a window around each such marker so
#' the failing chunk is visible without the full render transcript.
#'
#' @param x A path to a log file, or the log text.
#' @param context Lines of context to keep on each side of a failure marker.
#' @return Invisibly, the filtered lines (character). Printed by default.
#' @export
filter_render_log <- function(x, context = 20L) {
  lines <- .asLines(x)
  n <- length(lines)
  if (!n) return(invisible(character()))
  marker <- grepl("^Quitting from", lines) |
    grepl("^Error", lines) |
    grepl("Execution halted", lines) |
    grepl("^\\s*Error in ", lines) |
    grepl("^Warning message", lines)
  idx <- which(marker)
  if (!length(idx)) {
    out <- "[rfilter] no render error found (vignette rendered cleanly)."
    cat(out, "\n"); return(invisible(out))
  }
  windows <- unique(unlist(lapply(idx, function(i)
    seq.int(max(1L, i - context), min(n, i + context)))))
  windows <- sort(windows)
  # Insert ellipses where the kept windows are non-contiguous.
  out <- character()
  prev <- NA_integer_
  for (i in windows) {
    if (!is.na(prev) && i > prev + 1L) out <- c(out, "  ...")
    out <- c(out, lines[i]); prev <- i
  }
  cat(out, sep = "\n"); cat("\n")
  invisible(out)
}
