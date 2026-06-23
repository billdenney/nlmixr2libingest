# Static, pre-render vignette linter.
#
# Rendering is the single most expensive / most-iterated operation in an
# extraction (each failed render adds a turn and a large render log to the
# re-read-every-turn context). This linter catches the most common, regex- and
# rxUi-detectable render-killers WITHOUT a render round-trip, so they are fixed
# before the first (expensive) render rather than discovered by it. It is a
# best-effort PRE-check -- it does not replace the render gate, and a clean lint
# does not guarantee a clean render.

# Extract the R code from ```{r ...} chunks of an Rmd.
.lintRcode <- function(rmd) {
  lines <- readLines(rmd, warn = FALSE)
  inchunk <- FALSE
  code <- character()
  for (ln in lines) {
    if (grepl("^```\\{[rR]", ln)) { inchunk <- TRUE; next }
    if (inchunk && grepl("^```\\s*$", ln)) { inchunk <- FALSE; next }
    if (inchunk) code <- c(code, ln)
  }
  code
}

# Algebraic observables: prediction variables that are not ODE compartment
# states (e.g. `Cc <- central / vc; Cc ~ prop(.)`). Best-effort via rxUi.
.lintObservables <- function(model) {
  if (is.null(model)) return(character())
  ui <- tryCatch(.stAsUi(model), error = function(e) NULL)
  if (is.null(ui)) return(character())
  tryCatch({
    pd <- ui$predDf
    nm <- if (is.data.frame(pd) && "var" %in% names(pd)) pd$var
          else if (is.data.frame(pd) && "cond" %in% names(pd)) pd$cond
          else character()
    st <- tryCatch(as.character(ui$state), error = function(e) character())
    unique(setdiff(as.character(nm), st))
  }, error = function(e) character())
}

.lintIssue <- function(check, severity, message, fix) {
  data.frame(check = check, severity = severity, message = message, fix = fix,
             stringsAsFactors = FALSE)
}

#' Statically pre-lint a validation vignette for common render-killers
#'
#' Scans the vignette's R chunks (and, when supplied, the model) for the
#' recurring failures that otherwise only surface during an expensive render:
#' an oversized simulation cohort, a missing `cmt =` for an algebraic
#' observable (the most common slot-renumbering failure), a named-vector scalar
#' passed to `amt =`, and a PKNCA input filter that drops the `time = 0` row.
#'
#' @param rmd Path to the vignette `.Rmd`.
#' @param model Optional `rxUi` / model function / `nlmixr2lib` name; enables the
#'   algebraic-observable `cmt =` check.
#' @param max_per_arm Maximum simulated participants per arm (default 200).
#' @return An `nli_vignette_lint` list with `issues` (data frame) and `n`.
#' @export
lint_vignette <- function(rmd, model = NULL, max_per_arm = 200L) {
  if (!file.exists(rmd)) {
    return(structure(list(
      issues = .lintIssue("file", "error", paste0("vignette not found: ", rmd),
                          "check the path"), n = 1L),
      class = "nli_vignette_lint"))
  }
  code <- .lintRcode(rmd)
  blob <- paste(code, collapse = "\n")
  iss <- list()

  # 1. Event table referencing an algebraic observable as a compartment.
  # Per the nlmixr2 convention, event-table `cmt=` must name an ODE STATE, never
  # an algebraic observable: referencing the observable makes rxode2 inject a
  # `cmt()` slot for it AFTER the ODE states, renumbering every compartment --
  # the most common slot-renumbering render failure. rxode2 computes the
  # observable (e.g. `Cc <- central / vc`) at the state's observation rows.
  obs <- .lintObservables(model)
  if (length(obs)) {
    used <- obs[vapply(obs, function(o)
      grepl(sprintf("cmt\\s*=\\s*[\"']%s[\"']", o), blob), logical(1L))]
    if (length(used)) {
      states <- tryCatch(as.character(.stAsUi(model)$state),
                         error = function(e) character())
      rec <- if (length(states))
        paste0("use an ODE state name instead (e.g. cmt='", states[[length(states)]], "')")
      else "use the ODE state name instead"
      iss[[length(iss) + 1L]] <- .lintIssue(
        "cmt-observable", "warning",
        paste0("event table references algebraic observable(s) ",
               paste(used, collapse = ", "), " as a compartment via cmt="),
        paste0(rec, " -- referencing an observable injects a cmt slot and renumbers compartments"))
    }
  }

  # 2. Named-vector scalar passed to amt = (single-bracket subscript of a
  #    *character* key -> a named length-1 vector). A numeric index (`x[1]`)
  #    yields an unnamed scalar and is fine, so require a quoted key.
  if (grepl("amt\\s*=\\s*[A-Za-z._][\\w.$]*\\[\\s*[\"']", blob, perl = TRUE)) {
    iss[[length(iss) + 1L]] <- .lintIssue(
      "amt-named-scalar", "warning",
      "`amt =` is given a single-bracket character subscript of a named vector",
      "use `[[ ]]` (rxode2 rejects names on amt: 'Assertion on amt failed: May not have names')")
  }

  # 3. PKNCA input filter that can drop the time = 0 row -- only flagged when the
  #    vignette actually uses PKNCA (otherwise a `time > 0` filter is harmless).
  if (grepl("PKNCA", blob, fixed = TRUE) &&
      grepl("filter\\([^)]*\\b(time\\s*>\\s*0|Cc\\s*>\\s*0)\\b", blob, perl = TRUE)) {
    iss[[length(iss) + 1L]] <- .lintIssue(
      "pknca-zero-row", "warning",
      "a filter() may drop the time=0 record before PKNCA",
      "filter only with !is.na(); ensure a time=0 observation exists (else 'AUC range starting before first measurement')")
  }

  # 4. Simulated cohort larger than max_per_arm
  nums <- integer()
  for (pat in c("nSub\\s*=\\s*(\\d+)", "nsub\\s*=\\s*(\\d+)",
                "id\\s*=\\s*1:(\\d+)", "seq_len\\((\\d+)\\)",
                "id\\s*=\\s*seq_len\\((\\d+)\\)")) {
    m <- regmatches(blob, gregexpr(pat, blob, perl = TRUE))[[1L]]
    if (length(m)) nums <- c(nums,
      as.integer(regmatches(m, regexpr("\\d+", m))))
  }
  nums <- nums[is.finite(nums)]
  if (length(nums) && max(nums) > max_per_arm) {
    iss[[length(iss) + 1L]] <- .lintIssue(
      "cohort-too-large", "warning",
      paste0("simulated cohort of ", max(nums), " exceeds the ", max_per_arm,
             "-per-arm cap"),
      paste0("reduce per-arm simulation to <= ", max_per_arm,
             " (oversized cohorts are the top render-timeout / cost cause)"))
  }

  issues <- if (length(iss)) do.call(rbind, iss) else
    data.frame(check = character(), severity = character(),
               message = character(), fix = character(), stringsAsFactors = FALSE)
  rownames(issues) <- NULL
  structure(list(issues = issues, n = nrow(issues)), class = "nli_vignette_lint")
}

#' @export
print.nli_vignette_lint <- function(x, ...) {
  if (x$n == 0L) {
    cli::cli_alert_success("vignette pre-lint: no known render-killers found")
    return(invisible(x))
  }
  cli::cli_alert_warning("vignette pre-lint: {x$n} potential render issue{?s} (fix before rendering)")
  for (i in seq_len(x$n)) {
    cli::cli_text("! [{x$issues$check[i]}] {x$issues$message[i]}")
    cli::cli_text("    {.emph fix:} {x$issues$fix[i]}")
  }
  invisible(x)
}
