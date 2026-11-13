# Build a minimal but format-faithful set of the three registers in a temp dir,
# so parser/lookup tests do not depend on nlmixr2lib being installed.

make_fixture_refs <- function(extra_cov = NULL) {
  dir <- file.path(tempfile("refs-"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  cov <- c(
    "# Canonical covariate columns",
    "## Demographics",
    "### WT (**canonical for body weight**)",
    "- **Description:** Body weight (baseline or time-varying).",
    "- **Units:** kg",
    "- **Type:** continuous",
    "- **Scope:** general",
    "- **Reference category:** n/a -- allometric scaling.",
    "- **Source aliases:**",
    "  - `WEIG` -- used in `Wang_2012_levetiracetam.R`",
    "- **Example models:** `Clegg_2024_nirsevimab.R`, `Hu_2026_clesrovimab.R`",
    "- **Notes:** Universal.",
    "",
    "### CRCL (**canonical for creatinine clearance**)",
    "- **Description:** Creatinine clearance (renal function).",
    "- **Units:** mL/min",
    "- **Type:** continuous",
    "- **Scope:** general",
    "- **Source aliases:** none.",
    "- **Example models:** `Li_2006_meropenem.R`",
    "- **Notes:** Cockcroft-Gault unless stated."
  )
  if (!is.null(extra_cov)) cov <- c(cov, "", extra_cov)

  cmt <- c(
    "# Canonical compartment names",
    "## Standard drug-disposition compartments",
    "### central (**canonical central compartment**)",
    "- **Type:** compartment",
    "- **Role:** Central compartment; the output state for `Cc = central / vc`.",
    "- **Source aliases:** none.",
    "- **Example models:** universal."
  )

  par <- c(
    "# Canonical parameter names",
    "## Log-transformed structural PK parameters",
    "### lcl (**canonical log-transformed total clearance**)",
    "- **Type:** log-transformed-pk",
    "- **Role:** Apparent total drug clearance from the central compartment.",
    "- **Source aliases:** none.",
    "- **Example models:** universal."
  )

  writeLines(cov, file.path(dir, "covariate-columns.md"))
  writeLines(cmt, file.path(dir, "compartment-names.md"))
  writeLines(par, file.path(dir, "parameter-names.md"))
  dir
}

# A minimal rxode2 model function with a depot, a central compartment and the
# covariates FED (documented) and WT (on clearance only). `pre` adds statements
# before the ODEs; `f` and `lag` are the bioavailability and lag statements
# (NULL omits them); `covariate_data = FALSE` drops the covariateData list so
# the covariates are only the ones rxode2 detects.
make_lag_model <- function(pre = character(),
                           f = "f(depot) <- exp(lf) * (1 + e_fed * FED)",
                           lag = "alag(depot) <- exp(ltlag)",
                           covariate_data = TRUE) {
  cov_line <- paste0(
    "covariateData <- list(",
    'FED = list(description = "fed", units = "(binary)", type = "binary"), ',
    'WT = list(description = "weight", units = "kg", type = "continuous"))')
  txt <- c(
    "function() {",
    if (covariate_data) cov_line,
    "ini({",
    "  lka <- log(1); lcl <- log(2); lvc <- log(10); prop.sd <- 0.1",
    "})",
    "model({",
    "  ka <- exp(lka); cl <- exp(lcl) * (WT / 70)^0.75; vc <- exp(lvc)",
    # Constants live in model(), not ini(): rxode2 rejects an ini() parameter
    # the model never reads, and `f` / `lag` may be NULL.
    "  lf <- 0; e_fed <- -0.2; ltlag <- -0.7",
    pre,
    "  d/dt(depot) <- -ka * depot",
    "  d/dt(central) <- ka * depot - cl / vc * central",
    f,
    lag,
    "  Cc <- central / vc",
    "  Cc ~ prop(prop.sd)",
    "})",
    "}")
  eval(parse(text = txt))
}

# An article (Rmd) holding `code` in one R chunk; `header` is the chunk header.
write_article <- function(code, env = parent.frame(), header = "```{r}") {
  rmd <- withr::local_tempfile(fileext = ".Rmd", .local_envir = env)
  writeLines(c("---", "title: t", "---", "", header, code, "```"), rmd)
  rmd
}

# The state .lintWalkSolves() threads through an article, for direct tests.
new_acc <- function(modelnames = character(), containers = character(),
                    eventnames = character()) {
  acc <- new.env(parent = emptyenv())
  acc$sites <- list()
  acc$models <- character()
  acc$modelnames <- modelnames
  acc$containers <- containers
  acc$eventnames <- eventnames
  acc$funs <- list()
  acc$pending <- list()
  acc$ctx <- NULL
  acc$unparsed <- character()
  acc
}

# --- helpers for the lagged-dose tests ------------------------------------------

# The lagged-dose findings of a lint result.
lagged_checks <- function(res) {
  res$issues$check[grepl("^addlkeepscov", res$issues$check)]
}

# lint_vignette() of an article holding `code`, with `model` as the model.
lagged_one <- function(code, model = make_lag_model(), ...) {
  lint_vignette(write_article(code, ...), model = model)
}

# The lagged-dose findings for an article holding `code`.
lagged_checks_of <- function(code, model = make_lag_model()) {
  lagged_checks(lagged_one(code, model))
}

# How many findings are "addlkeepscov-unstated".
count_unstated <- function(code, model = make_lag_model()) {
  sum(lagged_checks_of(code, model) == "addlkeepscov-unstated")
}

# TRUE when the only finding is "addlkeepscov-unverifiable".
is_unverifiable <- function(code, model = make_lag_model()) {
  identical(lagged_checks_of(code, model), "addlkeepscov-unverifiable")
}

# TRUE when rxSolve(mod, ev, <args>) is the only finding, as "addlkeepscov-unstated".
rx_unstated <- function(args) {
  identical(lagged_checks_of(sprintf("sim <- rxode2::rxSolve(mod, ev, %s)", args)),
            "addlkeepscov-unstated")
}

# The covariates the f() statements of a lag-model variant depend on.
lag_covs <- function(...) {
  .lintLaggedFCovariates(rxode2::rxode2(make_lag_model(...)))
}

# Whether one call's text states the setting.
call_states <- function(code) {
  .lintCallStates(parse(text = code, keep.source = FALSE)[[1L]])
}

# A model read from the library, and a rxSolve call on it, for the lagged-dose tests.
from_db <- 'mod <- readModelDb("Some_Model")'
rx_call <- "sim <- rxode2::rxSolve(mod, ev)"

# TRUE when an article holding `code` (after the readModelDb() line) has no
# lagged-dose finding: the rule neither resolves nor reports it.
is_silent <- function(code) {
  identical(lagged_checks_of(c(from_db, code)), character())
}

# The R chunks of an Rmd holding `lines`.
read_chunks <- function(lines) {
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(lines, rmd)
  .lintRchunks(rmd)
}

# TRUE when a chunk with this header and body is not scanned for simulations.
is_skipped <- function(header, body = rx_call) {
  identical(lagged_one(body, header = header)$n, 0L)
}

# The finding ids of lint_vignette() on an article holding `code`.
lint_checks <- function(code) {
  lint_vignette(write_article(code))$issues$check
}

# Which chunks knitr evaluates: `chunks` is a list of c(header, lines...) and the
# answer says, for each, whether its code ran.
knitr_ran <- function(chunks) {
  src <- unlist(lapply(seq_along(chunks), function(i) {
    c(chunks[[i]][[1L]], chunks[[i]][-1L], sprintf("cat('RAN_%d\\n')", i), "```", "")
  }))
  out <- unlist(strsplit(knitr::knit(text = src, quiet = TRUE, envir = new.env()), "\n"))
  vapply(seq_along(chunks), function(i) any(grepl(sprintf("^## RAN_%d$", i), out)), NA)
}

# --- helpers for the pairing-rule tests -----------------------------------------------

# A model whose depot has alag(), f(), rate() and dur(), all reading the
# covariate OCC; `lag` is the alag() right-hand side.
make_pair_model <- function(lag = "exp(ltlag) * (1 + OCC)",
                            f = "exp(lf) * (1 + e_occ * OCC)", pre = character(),
                            ini_extra = character()) {
  txt <- c(
    "function() {",
    'covariateData <- list(OCC = list(description = "occasion", units = "(count)", type = "categorical"))',
    paste("ini({ lka <- log(1); lcl <- log(2); lvc <- log(10); prop.sd <- 0.1;",
          paste(ini_extra, collapse = "; "), "})"),
    "model({",
    "  ka <- exp(lka); cl <- exp(lcl); vc <- exp(lvc)",
    "  lf <- 0; e_occ <- -0.2; ltlag <- log(0.25)",
    pre,
    "  d/dt(depot) <- -ka * depot",
    "  d/dt(central) <- ka * depot - cl / vc * central",
    paste0("  f(depot) <- ", f),
    "  rate(depot) <- 50 * (1 + OCC)",
    "  dur(depot) <- 2 * (1 + OCC)",
    paste0("  alag(depot) <- ", lag),
    "  Cc <- central / vc",
    "  Cc ~ prop(prop.sd)",
    "})",
    "}")
  eval(parse(text = txt))
}

# The pairing spec of a model, with the sandbox environment the lag is run in.
pair_spec <- function(...) {
  spec <- .lintLagSpec(rxode2::rxode2(make_pair_model(...)))
  spec$env <- .lintSandboxEnv()
  spec
}

# An event table from compact records: each record is c(time, kind, OCC) with kind
# "d" (dose) or "o" (observation), in data order. `rate` is the rate column of the
# doses (0 for a bolus); `dur` adds a fixed-duration column instead.
pair_events <- function(records, rate = 0, dur = NULL, id = 1L) {
  time <- vapply(records, function(r) as.numeric(r[[1L]]), 0)
  kind <- vapply(records, function(r) as.character(r[[2L]]), "")
  occ <- vapply(records, function(r) as.numeric(r[[3L]]), 0)
  ev <- data.frame(id = id, time = time, evid = ifelse(kind == "d", 1L, 0L),
                   amt = ifelse(kind == "d", 100, NA_real_),
                   cmt = ifelse(kind == "d", "depot", "central"), OCC = occ,
                   stringsAsFactors = FALSE)
  if (is.null(dur)) ev$rate <- ifelse(kind == "d", rate, NA_real_)
  else ev$dur <- ifelse(kind == "d", dur, NA_real_)
  ev
}

# The pairing differences of an event table under a setting: a data frame, or NULL.
pair_diffs <- function(ev, setting, spec = pair_spec(lag = "0.5")) {
  recs <- .lintEventRecords(ev, spec)
  stopifnot(isTRUE(recs$ok))
  .lintPairCheck(recs$rec, spec, setting, new.env(parent = emptyenv()))
}

# lint_vignette() with the pairing rule on, for an article holding `code`.
pair_lint <- function(code, model = make_pair_model(lag = "0.5"), ...) {
  lint_vignette(write_article(code, ...), model = model, pairing = TRUE)
}

# The pairing findings of a lint result.
pair_checks <- function(res) res$issues$check[grepl("^lagged-dose-pairing", res$issues$check)]
