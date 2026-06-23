# Programmatic source-trace PRE-CHECK (quality-firewall assist).
#
# The firewall requires the agent to source-trace every final ini() value AND
# any nonstandard model() equation against the original paper. This module
# automates the cheap, high-recall part: for each ini() value it searches the
# paper for a supporting number (back-transform- and rounding-tolerant, numeric
# comparison so formatting doesn't matter) and FLAGS values with no nearby number
# -- the actionable "this value may not be in the paper" signal. It also lists
# the structural model() equations and any hardcoded constants the agent must
# confirm. It ASSISTS, and never replaces, the mandatory manual source-trace:
# a "found" match can still be coincidental, so the agent confirms context.

.numRe <- "[-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?"

.stAsUi <- function(model) {
  if (inherits(model, "rxUi")) return(model)
  if (is.function(model)) return(rxode2::rxode2(model))
  if (!requireNamespace("rxode2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg rxode2} is required to parse the model.")
  }
  # A bare name (no model-code syntax) is resolved via readModelDb() -- the
  # authoritative resolver -- rather than membership in `modeldb$name`, which
  # can be stale relative to the installed model files. Only fall through to
  # parsing the string as model code when it isn't a resolvable library name.
  if (is.character(model) && length(model) == 1L &&
      !grepl("[\n;~]|<-|d/dt|=", model) &&
      requireNamespace("nlmixr2lib", quietly = TRUE)) {
    fn <- tryCatch(nlmixr2lib::readModelDb(model), error = function(e) NULL)
    if (!is.null(fn)) return(rxode2::rxode2(fn))
  }
  rxode2::rxode2(model)
}

.stPaperText <- function(paper) {
  if (length(paper) == 1L && !grepl("\n", paper) && file.exists(paper)) {
    return(paste(readLines(paper, warn = FALSE), collapse = " "))
  }
  paste(paper, collapse = " ")
}

.stPaperNums <- function(txt) {
  v <- suppressWarnings(as.numeric(regmatches(txt, gregexpr(.numRe, txt))[[1L]]))
  v[is.finite(v)]
}

# Natural-scale candidate values a paper might report for one ini parameter.
.stCandidates <- function(name, est, kind, backTransform = NA_character_) {
  cands <- est
  low <- tolower(name)
  bt <- tolower(ifelse(is.na(backTransform), "", backTransform))
  if (kind == "theta") {
    if (grepl("exp", bt) || grepl("^l[a-z]", low)) cands <- c(cands, exp(est))
    if (grepl("expit|plogis|logit", bt) || grepl("^logit", low)) cands <- c(cands, stats::plogis(est))
  } else if (kind == "eta") {
    # est is a variance on the (usually log) scale; papers report SD or %CV
    sd <- sqrt(abs(est))
    cands <- c(est, sd, sd * 100, if (est > 0 && est < 50) sqrt(exp(est) - 1) * 100 else NA)
  } else if (kind == "err") {
    cands <- c(est, est * 100, est^2)   # SD, %, variance
  }
  unique(cands[is.finite(cands)])
}

.stMatch <- function(cands, paper_nums, tol) {
  best <- list(found = FALSE, cand = NA_real_, paper = NA_real_, relerr = NA_real_)
  if (!length(paper_nums)) return(best)
  for (cval in cands) {
    scale <- max(abs(cval), 1e-8)
    d <- abs(paper_nums - cval) / scale
    i <- which.min(d)
    if (length(i) && d[i] <= tol && (is.na(best$relerr) || d[i] < best$relerr)) {
      best <- list(found = TRUE, cand = cval, paper = paper_nums[i], relerr = d[i])
    }
  }
  best
}

# Structural classification of a model() expression.
.stEqType <- function(line) {
  if (grepl("d/dt\\(", line)) return("ode")
  if (grepl("~", line)) return("error")
  if (grepl("linCmt\\(", line)) return("lincmt")
  "algebraic"
}
.stHasConst <- function(line) {
  rhs <- sub("^.*?(<-|=|~)", "", line)
  nums <- suppressWarnings(as.numeric(regmatches(rhs, gregexpr(.numRe, rhs))[[1L]]))
  any(is.finite(nums) & !(nums %in% c(0, 1)))
}

#' Programmatic source-trace pre-check for a model against its paper
#'
#' For every estimated `ini()` value, searches the paper text for a supporting
#' number (back-transform- and rounding-tolerant) and flags values with none.
#' Also lists structural `model()` equations and hardcoded constants to verify.
#' This assists -- never replaces -- the mandatory manual source-trace.
#'
#' @param model An `rxUi`, a model function, or an `nlmixr2lib` model name.
#' @param paper Paper text, or a path to a (trimmed) paper file.
#' @param tol Relative tolerance for a numeric match (default 0.05 = 5%).
#' @return An `nli_sourcetrace` list with `ini` (per-parameter trace),
#'   `equations` (per-line model() classification), and `summary` counts.
#' @export
source_trace <- function(model, paper, tol = 0.05) {
  ui <- .stAsUi(model)
  txt <- .stPaperText(paper)
  pnums <- .stPaperNums(txt)
  ini <- ui$iniDf

  rows <- list()
  for (i in seq_len(nrow(ini))) {
    r <- ini[i, ]
    if (!is.na(r$ntheta)) kind <- "theta"
    else if (!is.na(r$neta1) && r$neta1 == r$neta2) kind <- "eta"
    else if (!is.na(r$neta1)) kind <- "eta_cov"
    else kind <- "other"
    is_err <- isTRUE(nzchar(r$err) && !is.na(r$err))
    if (is_err) kind <- "err"
    bt <- if ("backTransform" %in% names(ini)) r$backTransform else NA_character_
    cands <- .stCandidates(r$name, r$est, if (kind == "eta_cov") "eta" else kind, bt)
    m <- .stMatch(cands, pnums, tol)
    rows[[i]] <- data.frame(
      param = r$name, kind = kind, est = r$est,
      found = m$found,
      paper_value = m$paper,
      rel_err = round(m$relerr, 4),
      stringsAsFactors = FALSE)
  }
  ini_tr <- do.call(rbind, rows)

  eqs <- vapply(ui$lstExpr, function(e) paste(deparse(e), collapse = " "), character(1L))
  etype <- vapply(eqs, .stEqType, character(1L), USE.NAMES = FALSE)
  hasc <- vapply(eqs, .stHasConst, logical(1L), USE.NAMES = FALSE)
  reparam <- etype == "algebraic" & grepl("<-\\s*exp\\(", eqs) & !hasc
  needs <- etype == "ode" | (etype == "algebraic" & !reparam) | hasc
  eq_tr <- data.frame(line = eqs, type = etype, has_constant = hasc,
                      needs_verify = needs, stringsAsFactors = FALSE)

  out <- list(
    ini = ini_tr,
    equations = eq_tr,
    summary = list(
      n_ini = nrow(ini_tr),
      ini_unverified = sum(!ini_tr$found),
      n_equations = nrow(eq_tr),
      equations_to_verify = sum(eq_tr$needs_verify),
      hardcoded_constants = sum(eq_tr$has_constant)
    ))
  class(out) <- "nli_sourcetrace"
  out
}

#' @export
print.nli_sourcetrace <- function(x, ...) {
  s <- x$summary
  cli::cli_h2("Source-trace pre-check (assist; manual verification still required)")
  cli::cli_text("ini values: {s$n_ini} | {.strong unverified (no number in paper): {s$ini_unverified}}")
  un <- x$ini[!x$ini$found, , drop = FALSE]
  if (nrow(un)) {
    cli::cli_text("  unverified parameters:")
    for (i in seq_len(nrow(un))) {
      cli::cli_text("    - {un$param[i]} = {signif(un$est[i], 4)} ({un$kind[i]})")
    }
  }
  cli::cli_text("model() equations: {s$n_equations} | to verify: {s$equations_to_verify} | hardcoded constants: {s$hardcoded_constants}")
  invisible(x)
}
