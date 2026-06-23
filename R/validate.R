# Unified, terse model + vignette validation -- one "Success / fix-list" result.
#
# The build->check->fix loop is the dominant token sink (cache_read x turns).
# Running validations one-by-one and reading their multi-thousand-line logs on
# every turn is the worst case. validate_model() runs the whole chain and
# returns a COMPACT result: either "success" or a short list of
# {stage, severity, location, message, fix} the agent can act on directly.
#
# Two tiers:
#   - "fast" (default): parse -> conventions -> source-trace. Pure R, seconds,
#     no package build. This is what the agent runs while iterating.
#   - "full": fast + R CMD check (filtered) + vignette render (filtered). The
#     pre-commit gate; needs the package toolchain.
#
# It composes existing pieces (rxode2 parse, nlmixr2lib::checkModelConventions,
# source_trace(), filter_check_log()/filter_render_log()); it never reimplements
# validation -- conventions stay in nlmixr2lib (package boundary).

.vIssues0 <- function() {
  data.frame(stage = character(), severity = character(),
             location = character(), message = character(),
             fix = character(), stringsAsFactors = FALSE)
}

.vIssue <- function(stage, severity, location, message, fix = NA_character_) {
  data.frame(stage = stage, severity = as.character(severity),
             location = as.character(location), message = as.character(message),
             fix = as.character(fix), stringsAsFactors = FALSE)
}

# --- fast-tier stages -------------------------------------------------------

# Parse to rxUi, capturing a syntax failure as a structured issue (not a stop).
.vParse <- function(model) {
  ui <- tryCatch(.stAsUi(model), error = function(e) e)
  if (inherits(ui, "condition")) {
    return(list(ui = NULL, issues = .vIssue(
      "parse", "error", NA,
      paste0("model does not parse: ", conditionMessage(ui)),
      "fix the ini()/model() syntax error reported above")))
  }
  list(ui = ui, issues = .vIssues0())
}

.vConventions <- function(ui) {
  # Resolve checkModelConventions() dynamically rather than via `::`: it is a
  # newer nlmixr2lib export, so a `nlmixr2lib::checkModelConventions` literal
  # trips R CMD check's "Missing or unexported object" WARNING against older
  # installed nlmixr2lib versions. getExportedValue() is checked at runtime.
  ok <- requireNamespace("nlmixr2lib", quietly = TRUE) &&
    "checkModelConventions" %in% getNamespaceExports("nlmixr2lib")
  if (!ok) {
    return(.vIssue("conventions", "note", NA,
                   "checkModelConventions() unavailable in the installed nlmixr2lib; convention check skipped",
                   "install a nlmixr2lib that exports checkModelConventions()"))
  }
  check_conventions <- getExportedValue("nlmixr2lib", "checkModelConventions")
  iss <- tryCatch(
    suppressWarnings(check_conventions(ui, verbose = FALSE)),
    error = function(e) e)
  if (inherits(iss, "condition")) {
    return(.vIssue("conventions", "error", NA,
                   paste0("checkModelConventions failed: ", conditionMessage(iss)),
                   NA))
  }
  if (is.null(iss) || !nrow(iss)) return(.vIssues0())
  sev <- iss$severity
  sev[sev == "info"] <- "note"
  .vIssue("conventions", sev, iss$name, iss$message, iss$suggestion)
}

.vSourceTrace <- function(ui, paper, tol) {
  st <- tryCatch(source_trace(ui, paper, tol = tol), error = function(e) e)
  if (inherits(st, "condition")) {
    return(.vIssue("source_trace", "note", NA,
                   paste0("source_trace failed: ", conditionMessage(st)), NA))
  }
  out <- .vIssues0()
  un <- st$ini[!st$ini$found, , drop = FALSE]
  if (nrow(un)) {
    out <- rbind(out, .vIssue(
      "source_trace", "warning", un$param,
      paste0("no supporting number found in paper for ", un$param,
             " = ", signif(un$est, 4), " (", un$kind, ")"),
      "read the paper; confirm the value or correct the ini() entry"))
  }
  nv <- st$summary$equations_to_verify
  if (isTRUE(nv > 0)) {
    out <- rbind(out, .vIssue(
      "source_trace", "note", NA,
      paste0(nv, " model() equation(s) and ", st$summary$hardcoded_constants,
             " hardcoded constant(s) to confirm against the paper"),
      "confirm each nonstandard equation/constant appears in the paper"))
  }
  out
}

# --- full-tier stages (heavy; need the package toolchain) -------------------

.vCheck <- function(pkg) {
  if (!requireNamespace("rcmdcheck", quietly = TRUE)) {
    return(.vIssue("check", "note", NA,
                   "rcmdcheck not installed; R CMD check skipped",
                   "install.packages('rcmdcheck') for full-tier checks"))
  }
  # --no-build-vignettes: the vignette is render-checked separately in .vRender,
  # and a full vignette build during check can hit environment-specific C-level
  # segfaults (e.g. the known nlmixr2lib CarlssonPetri case) unrelated to the
  # model under test. Checking the package without rebuilding vignettes mirrors
  # how the production extract-literature-model skill runs devtools::check().
  res <- tryCatch(
    rcmdcheck::rcmdcheck(pkg, args = c("--no-manual", "--no-build-vignettes"),
                         error_on = "never", quiet = TRUE),
    error = function(e) e)
  if (inherits(res, "condition")) {
    return(.vIssue("check", "error", pkg,
                   paste0("R CMD check could not run: ", conditionMessage(res)),
                   "ensure the package builds and its deps are installed"))
  }
  out <- .vIssues0()
  add <- function(vec, sev) {
    for (m in vec) {
      head1 <- trimws(strsplit(m, "\n", fixed = TRUE)[[1L]][[1L]])
      out <<- rbind(out, .vIssue("check", sev, NA, head1,
                                 "see the full R CMD check output for detail"))
    }
  }
  add(res$errors, "error"); add(res$warnings, "warning"); add(res$notes, "note")
  out
}

.vRender <- function(vignette) {
  if (is.null(vignette) || is.na(vignette) || !nzchar(vignette)) {
    return(.vIssue("render", "note", NA,
                   "no vignette path supplied; render skipped",
                   "pass vignette=<path/to/article.Rmd> to render-check"))
  }
  if (!file.exists(vignette)) {
    return(.vIssue("render", "error", vignette,
                   "vignette file not found", "check the vignette path"))
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    return(.vIssue("render", "note", NA,
                   "rmarkdown not installed; render skipped", NA))
  }
  tmp <- tempfile(fileext = ".html")
  log <- character()
  ok <- tryCatch({
    withCallingHandlers(
      rmarkdown::render(vignette, output_file = tmp, quiet = TRUE,
                        envir = new.env()),
      message = function(m) log <<- c(log, conditionMessage(m)),
      warning = function(w) log <<- c(log, conditionMessage(w)))
    TRUE
  }, error = function(e) { log <<- c(log, conditionMessage(e)); FALSE })
  if (ok) return(.vIssues0())
  # Reduce the captured render transcript to the failure + context.
  filtered <- utils::capture.output(filter_render_log(paste(log, collapse = "\n")))
  .vIssue("render", "error", vignette,
          paste(utils::head(filtered, 25L), collapse = " | "),
          "fix the failing vignette chunk shown above")
}

# Load the in-development package from the worktree (once) so the render and
# convention checks see the model under edit, not the stale installed package.
# This is the per-iteration setup the skill otherwise does as a separate
# `devtools::load_all()` call; folding it in collapses a turn + a reload.
.vLoadAll <- function(pkg) {
  if (is.null(pkg)) {
    return(.vIssue("load_all", "note", NA,
                   "no pkg supplied; render uses the installed package", NA))
  }
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    return(.vIssue("load_all", "note", NA,
                   "pkgload not installed; render uses the installed package",
                   "install.packages('pkgload') so the render sees the worktree"))
  }
  res <- tryCatch({
    suppressMessages(pkgload::load_all(pkg, quiet = TRUE))
    TRUE
  }, error = function(e) e)
  if (inherits(res, "condition")) {
    return(.vIssue("load_all", "error", pkg,
                   paste0("load_all failed: ", conditionMessage(res)),
                   "fix the package so it loads (syntax / dependency error above)"))
  }
  .vIssues0()
}

#' Validate a model (and optionally its vignette) into one terse result
#'
#' Runs the whole validation chain and returns a compact pass/fix object
#' instead of the verbose logs that otherwise re-cache on every turn of the
#' build-check-fix loop. The fast tier (parse, conventions, source-trace) is
#' pure R and meant for iteration; the full tier adds R CMD check and a vignette
#' render. Convention checking is delegated to
#' `nlmixr2lib::checkModelConventions()` -- this function never reimplements it.
#'
#' @param model An `rxUi`, a model function, or an `nlmixr2lib` model name.
#' @param paper Optional paper text or path; enables the source-trace stage.
#' @param level `"fast"` (parse + conventions + source-trace), `"model"` (adds a
#'   one-session `load_all` of `pkg` + the model's vignette render -- the
#'   per-iteration combined gate, *no* whole-package check), or `"full"` (adds a
#'   whole-package R CMD check + render; the pre-commit gate).
#' @param pkg Package directory: `load_all`-ed for the `model` tier, R-CMD-checked
#'   for the `full` tier.
#' @param vignette Optional path to the model's validation vignette to render.
#' @param tol Relative tolerance for source-trace numeric matches.
#' @return An `nli_validation` object: `status` (`"success"`/`"issues"`),
#'   `issues` (data frame), `counts`, and the `stages` run.
#' @export
validate_model <- function(model, paper = NULL,
                           level = c("fast", "full", "model"),
                           pkg = NULL, vignette = NULL, tol = 0.05) {
  level <- match.arg(level)
  issues <- .vIssues0()
  stages <- "parse"

  p <- .vParse(model)
  issues <- rbind(issues, p$issues)
  ui <- p$ui
  # A parse failure is fatal for every downstream stage.
  if (is.null(ui)) {
    return(.vFinish(issues, stages, level))
  }

  stages <- c(stages, "conventions")
  issues <- rbind(issues, .vConventions(ui))

  if (!is.null(paper)) {
    stages <- c(stages, "source_trace")
    issues <- rbind(issues, .vSourceTrace(ui, paper, tol))
  }

  # full: whole-package R CMD check (heavy) + render -- the pre-commit gate.
  # model: load_all the worktree once, then render the model's vignette -- the
  #   per-iteration combined gate (conventions + source-trace + render in one
  #   call), deliberately WITHOUT the whole-package check that makes the full
  #   tier re-run everything on every fix iteration.
  if (level == "full") {
    if (!is.null(pkg)) {
      stages <- c(stages, "check")
      issues <- rbind(issues, .vCheck(pkg))
    }
    stages <- c(stages, "render")
    issues <- rbind(issues, .vRender(vignette))
  } else if (level == "model") {
    stages <- c(stages, "load_all")
    issues <- rbind(issues, .vLoadAll(pkg))
    stages <- c(stages, "render")
    issues <- rbind(issues, .vRender(vignette))
  }

  .vFinish(issues, stages, level)
}

.vFinish <- function(issues, stages, level) {
  rownames(issues) <- NULL
  counts <- c(error = sum(issues$severity == "error"),
              warning = sum(issues$severity == "warning"),
              note = sum(issues$severity == "note"))
  status <- if (counts[["error"]] == 0L && counts[["warning"]] == 0L)
    "success" else "issues"
  structure(list(status = status, issues = issues, counts = counts,
                 stages = stages, level = level),
            class = "nli_validation")
}

#' @export
print.nli_validation <- function(x, ...) {
  if (identical(x$status, "success")) {
    n <- x$counts[["note"]]
    cli::cli_alert_success(
      "Success ({paste(x$stages, collapse = ' / ')}){if (n) paste0('  [', n, ' note', if (n > 1) 's' else '', ']') else ''}")
    if (n) {
      notes <- x$issues[x$issues$severity == "note", , drop = FALSE]
      for (i in seq_len(nrow(notes))) {
        cli::cli_text("  {.emph note} [{notes$stage[i]}] {notes$message[i]}")
      }
    }
    return(invisible(x))
  }
  cli::cli_alert_danger(
    "{x$counts[['error']]} error(s), {x$counts[['warning']]} warning(s), {x$counts[['note']]} note(s)  ({paste(x$stages, collapse = ' / ')})")
  bad <- x$issues[x$issues$severity %in% c("error", "warning"), , drop = FALSE]
  bad <- bad[order(match(bad$severity, c("error", "warning"))), , drop = FALSE]
  for (i in seq_len(nrow(bad))) {
    loc <- if (is.na(bad$location[i])) "" else paste0(" {.field ", bad$location[i], "}")
    sym <- if (bad$severity[i] == "error") "x" else "!"
    cli::cli_text("{sym} [{bad$stage[i]}]{loc} {bad$message[i]}")
    if (!is.na(bad$fix[i])) cli::cli_text("    {.emph fix:} {bad$fix[i]}")
  }
  invisible(x)
}
