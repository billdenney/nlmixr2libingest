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
