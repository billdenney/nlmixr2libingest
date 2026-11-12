test_that("lint_vignette flags oversized cohort, named-scalar amt, and PKNCA zero-row", {
  rmd <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---", "title: t", "output: html_document", "---", "",
    "```{r}",
    'ev <- et(amt = doses["depot"])',          # named-vector single-bracket -> amt
    "sim <- rxSolve(mod, ev, nSub = 5000)",     # cohort > 200
    "nca_in <- dplyr::filter(sim, time > 0)",   # drops the time=0 row (PKNCA input)
    "pk <- PKNCA::PKNCAconc(nca_in, conc ~ time | id)",
    "```"), rmd)
  res <- lint_vignette(rmd)
  expect_s3_class(res, "nli_vignette_lint")
  checks <- res$issues$check
  expect_true("cohort-too-large" %in% checks)
  expect_true("amt-named-scalar" %in% checks)
  expect_true("pknca-zero-row" %in% checks)
})

test_that("lint_vignette passes a clean vignette", {
  rmd <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---", "title: t", "output: html_document", "---", "",
    "```{r}",
    'ev <- et(amt = doses[["depot"]])',         # [[ ]] -> fine
    "sim <- rxSolve(mod, ev, nSub = 100)",       # <= 200
    "nca_in <- dplyr::filter(sim, !is.na(Cc))",  # no time>0 filter
    "```"), rmd)
  res <- lint_vignette(rmd)
  expect_equal(res$n, 0L)
})

test_that("max_per_arm is configurable", {
  rmd <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "t", "---", "```{r}", "rxSolve(m, e, nSub = 300)", "```"), rmd)
  expect_true("cohort-too-large" %in% lint_vignette(rmd, max_per_arm = 200L)$issues$check)
  expect_equal(lint_vignette(rmd, max_per_arm = 500L)$n, 0L)
})

test_that("lint_vignette flags an event table that uses cmt= on an algebraic observable", {
  skip_if_not_installed("rxode2")
  skip_if_not_installed("nlmixr2lib")
  # PK_1cmt observes Cc (algebraic, central/vc); referencing cmt="Cc" is the
  # slot-renumbering bug -- event tables must use the ODE state (cmt="central").
  bad <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "t", "---", "```{r}",
               'ev <- et(amt = 100, cmt = "depot") |> et(time = 1:24, cmt = "Cc")', "```"), bad)
  res <- lint_vignette(bad, model = "PK_1cmt")
  expect_true("cmt-observable" %in% res$issues$check)
  # the correct version (observing the state) is not flagged
  good <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "t", "---", "```{r}",
               'ev <- et(amt = 100, cmt = "depot") |> et(time = 1:24, cmt = "central")', "```"), good)
  expect_false("cmt-observable" %in% lint_vignette(good, model = "PK_1cmt")$issues$check)
})

test_that("print renders for both outcomes", {
  rmd <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "t", "---", "```{r}", "1+1", "```"), rmd)
  expect_s3_class(print(lint_vignette(rmd)), "nli_vignette_lint")
})

test_that("lint_vignette flags assertions that cannot hold across machines", {
  # rxSetSeed() partitions rxode2's RNG per solver thread, so a CI runner draws
  # a different cohort than the authoring machine. These three shapes therefore
  # pass where they are written and fail where they run.
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c(
    "```{r}", "set.seed(1)", "sim <- rxSolve(mod, ev)",
    "stopifnot(all(diff(pv) < 0))",
    "stopifnot(all(pct_over_mrl == 0))",
    "```"
  ), rmd)
  res <- lint_vignette(rmd)
  expect_true(all(c("assert-strict-monotone", "assert-exact-zero") %in%
                    res$issues$check))
})

test_that("lint_vignette does not flag the recommended >= solver-noise guard", {
  # `all(conc >= 0)` is what the failure-pattern catalogue tells authors to
  # write, so flagging it would train them to ignore the linter.
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c("```{r}", "set.seed(1)", "sim <- rxSolve(mod, ev)",
               "stopifnot(all(sim$conc >= 0))", "```"), rmd)
  res <- lint_vignette(rmd)
  expect_false(any(grepl("^assert-", res$issues$check)))
})

test_that("lint_vignette does not flag assertions in a non-simulating vignette", {
  # With no cohort draw there is no thread-dependent RNG, so a sign assertion
  # on a deterministic quantity is legitimate and must not be flagged.
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c("```{r}", "x <- solve_closed_form()",
               "stopifnot(all(x > 0))", "```"), rmd)
  res <- lint_vignette(rmd)
  expect_false(any(grepl("^assert-", res$issues$check)))
})

test_that("lint_vignette does not re-flag a fixed vignette that documents the old assertion", {
  # A good fix leaves a comment quoting the assertion it replaced. Matching raw
  # text would re-flag the repaired vignette, which is how a linter earns being
  # ignored.
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c("```{r}", "set.seed(1)", "sim <- rxSolve(mod, ev)",
               "# An earlier revision used `all(diff(pv) < 0)`, which requires",
               "# every adjacent pair to be ordered.",
               "stopifnot(pv[length(pv)] < pv[1])", "```"), rmd)
  res <- lint_vignette(rmd)
  expect_false(any(grepl("^assert-", res$issues$check)))
})

test_that("lint_vignette does not flag a step TOLERANCE as strict monotonicity", {
  # `all(diff(x) < 0.25)` is the recommended replacement for
  # `all(diff(x) < 0)`; flagging it would fire on the fix.
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c("```{r}", "set.seed(1)", "sim <- rxSolve(mod, ev)",
               "stopifnot(all(diff(peaks$cmax_norm) < 0.25))", "```"), rmd)
  expect_false(any(grepl("^assert-", lint_vignette(rmd)$issues$check)))
})

# --- lagged-dose f() covariates (addlKeepsCov) -------------------------------

lagged_checks <- function(res) {
  res$issues$check[grepl("^addlkeepscov", res$issues$check)]
}

test_that(".lintRchunks splits chunks and .lintRcode is their concatenation", {
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c("prose `r 1 + 1`", "```{r a}", "x <- 1", "y <- 2", "```",
               "more prose", "```{r}", "z <- 3", "```", "```{r empty}", "```"), rmd)
  expect_identical(.lintRchunks(rmd),
                   list(c("x <- 1", "y <- 2"), "z <- 3", character()))
  expect_identical(.lintRcode(rmd), c("x <- 1", "y <- 2", "z <- 3"))
  none <- withr::local_tempfile(fileext = ".Rmd")
  writeLines("no chunks here", none)
  expect_identical(.lintRchunks(none), list())
  expect_identical(.lintRcode(none), character())
})

test_that(".lintReachable follows chains, collects every branch and survives cycles", {
  deps <- list(a = c("b", "x"), b = "c", c = character(), p = "q", q = "p")
  expect_setequal(.lintReachable("a", deps), c("a", "b", "x", "c"))
  expect_setequal(.lintReachable("p", deps), c("p", "q"))
  expect_identical(.lintReachable(character(), deps), character())
  expect_identical(.lintReachable("nothing_else", deps), "nothing_else")
})

test_that(".lintLaggedFCovariates names exactly the covariates f() depends on", {
  skip_if_not_installed("rxode2")
  cov_of <- function(...) .lintLaggedFCovariates(rxode2::rxode2(make_lag_model(...)))
  # direct dependence
  expect_identical(cov_of(), "FED")
  # through two intermediate assignments
  expect_identical(
    cov_of(pre = c("fa <- 1 + e_fed * FED", "fb <- fa * 0.9", "fx <- exp(lf) * fb"),
           f = "f(depot) <- fx"),
    "FED")
  # only through an if() condition: FED never appears on a right-hand side
  expect_identical(
    cov_of(pre = c("if (FED > 0) {", "  fx <- exp(lf) * 0.8", "} else {",
                   "  fx <- exp(lf)", "}"),
           f = "f(depot) <- fx"),
    "FED")
  # several covariates, sorted, each reached a different way
  expect_identical(
    cov_of(pre = "fx <- exp(lf) * (WT / 70)", f = "f(depot) <- fx * (1 + e_fed * FED)"),
    c("FED", "WT"))
  # the source spellings F() / lag() are the same statements
  expect_identical(
    cov_of(f = "F(depot) = exp(lf) * (1 + e_fed * FED)", lag = "lag(depot) <- exp(ltlag)"),
    "FED")
  # a covariate that is not listed in covariateData is still found
  expect_identical(cov_of(covariate_data = FALSE), "FED")
})

test_that(".lintLaggedFCovariates ignores models the rule does not apply to", {
  skip_if_not_installed("rxode2")
  cov_of <- function(...) .lintLaggedFCovariates(rxode2::rxode2(make_lag_model(...)))
  # alag() with a constant f(): WT is a covariate of the model, but not of f()
  expect_identical(cov_of(f = "f(depot) <- exp(lf)"), character())
  # a covariate f() without alag()
  expect_identical(cov_of(lag = NULL), character())
  # alag() without any f()
  expect_identical(cov_of(f = NULL), character())
  # neither
  expect_identical(cov_of(f = NULL, lag = NULL), character())
})

test_that("a lagged covariate-dependent f() without the setting is flagged", {
  skip_if_not_installed("rxode2")
  rmd <- write_article("sim <- rxode2::rxSolve(mod, events = ev, nSub = 10)")
  res <- lint_vignette(rmd, model = make_lag_model())
  expect_identical(lagged_checks(res), "addlkeepscov-unstated")
  expect_identical(res$n, 1L)
  msg <- res$issues$message
  expect_match(msg, "rxode2::rxSolve(mod, events = ev, nSub = 10)", fixed = TRUE)
  expect_match(msg, "covariate(s) FED.", fixed = TRUE)
  expect_match(msg, "rxode2 reads a lagged dose's f() covariates at its arrival time by default (addlKeepsCov = FALSE).", fixed = TRUE)
  expect_match(msg, "NONMEM uses the dose record's covariates (addlKeepsCov = TRUE).", fixed = TRUE)
  expect_match(msg, "State the setting and say in the article which you chose.", fixed = TRUE)
})

test_that("addlKeepsCov = TRUE, addlKeepsCov = FALSE and nonmem = TRUE all pass", {
  skip_if_not_installed("rxode2")
  mod <- make_lag_model()
  for (arg in c("addlKeepsCov = TRUE", "addlKeepsCov = FALSE", "nonmem = TRUE",
                "control = rxControl(addlKeepsCov = TRUE)")) {
    rmd <- write_article(sprintf("sim <- rxode2::rxSolve(mod, events = ev, %s)", arg))
    res <- lint_vignette(rmd, model = mod)
    expect_identical(res$n, 0L, info = arg)
  }
})

test_that("nonmem = FALSE does not state the setting", {
  skip_if_not_installed("rxode2")
  rmd <- write_article("sim <- rxode2::rxSolve(mod, events = ev, nonmem = FALSE)")
  expect_identical(lagged_checks(lint_vignette(rmd, model = make_lag_model())),
                   "addlkeepscov-unstated")
})

test_that("a lagged model with a constant f() is not flagged", {
  skip_if_not_installed("rxode2")
  rmd <- write_article("sim <- rxode2::rxSolve(mod, events = ev)")
  res <- lint_vignette(rmd, model = make_lag_model(f = "f(depot) <- exp(lf)"))
  expect_identical(res$n, 0L)
})

test_that("a covariate reached only through intermediate variables is flagged", {
  skip_if_not_installed("rxode2")
  rmd <- write_article("sim <- rxode2::rxSolve(mod, events = ev)")
  chain <- make_lag_model(
    pre = c("fa <- 1 + e_fed * FED", "fx <- exp(lf) * fa"), f = "f(depot) <- fx")
  res <- lint_vignette(rmd, model = chain)
  expect_identical(lagged_checks(res), "addlkeepscov-unstated")
  expect_match(res$issues$message, "covariate(s) FED.", fixed = TRUE)
  cond <- make_lag_model(
    pre = c("if (FED > 0) {", "  fx <- exp(lf) * 0.8", "} else {",
            "  fx <- exp(lf)", "}"), f = "f(depot) <- fx")
  expect_identical(lagged_checks(lint_vignette(rmd, model = cond)),
                   "addlkeepscov-unstated")
})

test_that("every way of writing an rxSolve() call is recognised", {
  skip_if_not_installed("rxode2")
  mod <- make_lag_model()
  flagged <- function(code) sum(lagged_checks(lint_vignette(write_article(code), model = mod)) ==
                                  "addlkeepscov-unstated")
  expect_equal(flagged("sim <- rxSolve(mod, ev)"), 1L)
  expect_equal(flagged("sim <- rxode2::rxSolve(mod, ev)"), 1L)
  expect_equal(flagged("sim <- rxode2:::rxSolve(mod, ev)"), 1L)
  expect_equal(flagged("sim <- ev %>% rxode2::rxSolve(mod, .)"), 1L)
  skip_if(getRversion() < "4.2.0", "the native pipe placeholder needs R >= 4.2")
  expect_equal(flagged("sim <- ev |> rxode2::rxSolve(mod, events = _)"), 1L)
  expect_equal(flagged("sim <- ev |> rxSolve(object = mod, events = _)"), 1L)
  # arguments over several lines, with a comment holding an unbalanced paren
  expect_equal(flagged(c(
    "sim <- rxode2::rxSolve(",
    "  mod,",
    "  events = ev,  # a comment with a ( in it",
    "  nSub = 10",
    ")")), 1L)
  # the setting on a piped, multi-line call
  expect_equal(flagged(c(
    "sim <- ev |>",
    "  rxode2::rxSolve(mod, events = _,",
    "                  addlKeepsCov = TRUE)")), 0L)
})

test_that("each call is judged on its own", {
  skip_if_not_installed("rxode2")
  rmd <- write_article(c(
    "a <- rxode2::rxSolve(mod, ev)",
    "b <- rxode2::rxSolve(mod, ev, addlKeepsCov = TRUE)",
    "c <- ev |> rxode2::rxSolve(mod, events = _)"))
  res <- lint_vignette(rmd, model = make_lag_model())
  expect_identical(lagged_checks(res), rep("addlkeepscov-unstated", 2L))
  expect_match(res$issues$message[[1L]], "rxSolve(mod, ev)", fixed = TRUE)
  expect_match(res$issues$message[[2L]], "rxSolve(mod, events = ev)", fixed = TRUE)
})

test_that("an rxSolve mentioned only in a comment or in prose is not a solve", {
  skip_if_not_installed("rxode2")
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c("Call rxode2::rxSolve(mod, ev) to simulate.", "```{r}",
               "# sim <- rxode2::rxSolve(mod, ev)", "x <- 1", "```"), rmd)
  expect_identical(lint_vignette(rmd, model = make_lag_model())$n, 0L)
})

test_that("a model with alag() and a covariate f() but no solve in the article passes", {
  skip_if_not_installed("rxode2")
  rmd <- write_article("x <- 1")
  expect_identical(lint_vignette(rmd, model = make_lag_model())$n, 0L)
})

test_that("simulations the linter cannot read are reported as not verifiable", {
  skip_if_not_installed("rxode2")
  mod <- make_lag_model()
  unverifiable <- function(code) lagged_checks(lint_vignette(write_article(code), model = mod))
  expect_identical(unverifiable('fit <- nlmixr2(mod, dat, est = "rxSolve")'),
                   "addlkeepscov-unverifiable")
  expect_identical(unverifiable("s <- simulate(fit, nsim = 5)"),
                   "addlkeepscov-unverifiable")
  expect_identical(unverifiable("s <- do.call(rxode2::rxSolve, args)"),
                   "addlkeepscov-unverifiable")
  expect_identical(unverifiable("s <- lapply(evs, rxSolve, object = mod)"),
                   "addlkeepscov-unverifiable")
  expect_identical(unverifiable('s <- do.call("rxSolve", args)'),
                   "addlkeepscov-unverifiable")
  # a chunk that mentions a solver but does not parse cannot be read either
  expect_identical(unverifiable("s <- rxSolve(mod, (ev"),
                   "addlkeepscov-unverifiable")
  # a call that names the setting itself is verified, so it passes
  expect_identical(
    unverifiable('fit <- nlmixr2(mod, dat, est = "rxSolve", control = rxControl(addlKeepsCov = TRUE))'),
    character())
  expect_identical(unverifiable("s <- simulate(fit, nsim = 5, nonmem = TRUE)"),
                   character())
})

test_that("not verifiable simulations are not reported for a model without the hazard", {
  skip_if_not_installed("rxode2")
  mod <- make_lag_model(f = "f(depot) <- exp(lf)")
  rmd <- write_article(c('fit <- nlmixr2(mod, dat, est = "rxSolve")',
                         "s <- simulate(fit, nsim = 5)"))
  expect_identical(lint_vignette(rmd, model = mod)$n, 0L)
})

test_that("the lagged-dose rule needs a model, and says so when the model cannot be read", {
  skip_if_not_installed("rxode2")
  rmd <- write_article("sim <- rxode2::rxSolve(mod, ev)")
  # no model known: the rule cannot apply, as for the other model checks
  expect_identical(lint_vignette(rmd)$n, 0L)
  # a model that is named but cannot be parsed is reported, not skipped
  bad <- lint_vignette(rmd, model = "no_such_model_9999")
  expect_identical(bad$issues$check, "addlkeepscov-model")
  expect_match(bad$issues$message, "`no_such_model_9999`", fixed = TRUE)
  # a vignette with no solve has nothing to verify, so an unreadable model is silent
  expect_identical(
    lint_vignette(write_article("x <- 1"), model = "no_such_model_9999")$n, 0L)
})

test_that("without a model argument the model is taken from readModelDb() in the article", {
  skip_if_not_installed("rxode2")
  skip_if_not_installed("nlmixr2lib")
  # Hughes 2022 brepocitinib: alag(depot) with f(depot) driven by FED_HIGHFAT
  # and DOSE_HIGH.
  skip_if_not("Hughes_2022_brepocitinib" %in% nlmixr2lib::modeldb$name)
  rmd <- write_article(c('mod <- readModelDb("Hughes_2022_brepocitinib")',
                         "sim <- rxode2::rxSolve(mod, ev)"))
  res <- lint_vignette(rmd)
  expect_identical(lagged_checks(res), "addlkeepscov-unstated")
  expect_match(res$issues$message, "covariate(s) DOSE_HIGH, FED_HIGHFAT.", fixed = TRUE)
  ok <- write_article(c('mod <- readModelDb("Hughes_2022_brepocitinib")',
                        "sim <- rxode2::rxSolve(mod, ev, addlKeepsCov = TRUE)"))
  expect_identical(lint_vignette(ok)$n, 0L)
  # a name that does not resolve is reported
  gone <- write_article(c('mod <- readModelDb("No_Such_Model_9999")',
                          "sim <- rxode2::rxSolve(mod, ev)"))
  expect_identical(lint_vignette(gone)$issues$check, "addlkeepscov-model")
})

# --- solve(): rxode2's model method versus base::solve() ----------------------

test_that(".lintDePipe rewrites a magrittr pipe the way magrittr evaluates it", {
  expect_identical(.lintDePipe(quote(a %>% f(b))), quote(f(a, b)))
  expect_identical(.lintDePipe(quote(a %>% f(b, .))), quote(f(b, a)))
  expect_identical(.lintDePipe(quote(a %>% f)), quote(f(a)))
  expect_identical(.lintDePipe(quote(a %>% f(x = 1))), quote(f(a, x = 1)))
  # a chain is rewritten one pipe at a time, outermost first
  expect_identical(.lintDePipe(quote(a %>% f() %>% g())), quote(g(a %>% f())))
})

test_that("base::solve() on a matrix is not a simulation", {
  skip_if_not_installed("rxode2")
  rmd <- write_article(c(
    "A <- matrix(c(2, 1, 1, 3), 2)",
    "inv <- solve(A)",
    "x <- solve(A, c(1, 2))",
    "y <- solve(chol(A), t(A), tol = 1e-10)",
    "ev <- eigen(A)",
    "w <- solve(ev$vectors, c(amt, 0, 0))",
    "z <- z %*% solve(chol(cov(z)))"))
  res <- lint_vignette(rmd, model = make_lag_model())
  expect_identical(res$n, 0L)
})

test_that("a model solve() without the setting is flagged, however the model is reached", {
  skip_if_not_installed("rxode2")
  mod <- make_lag_model()
  flagged <- function(code) {
    res <- lint_vignette(write_article(code), model = mod)
    list(checks = lagged_checks(res), msg = res$issues$message)
  }
  from_db <- 'mod <- readModelDb("Some_Model")'
  # the model object itself, wrapped in rxode2() or rxode()
  for (code in list(
    c('mod <- rxode2::rxode2(readModelDb("Some_Model"))', "sim <- solve(mod, ev)"),
    c(from_db, "ui <- rxode2::rxode(mod)", "sim <- solve(ui, ev)"),
    # a model()/ini() pipe of one, native and magrittr, and wrapped
    c(from_db, "m2 <- mod |> model({ a <- 1 })", "sim <- solve(m2, ev)"),
    c(from_db, "m2 <- mod %>% ini(lka = 1) %>% zeroRe()", "sim <- solve(m2, ev)"),
    c(from_db, "m2 <- suppressMessages(mod |> model({ a <- 1 }))", "sim <- solve(m2, ev)"),
    c(from_db, "m2 <- rxode2::zeroRe(mod)", "sim <- solve(m2, ev)"),
    # the model as the second argument of a piped event table
    c(from_db, "sim <- ev |> solve(mod)"),
    c(from_db, "sim <- mod %>% et(amt = 100) %>% solve()"),
    # nested in a pipeline, and its literal library name
    c(from_db, "sim <- solve(mod, ev) |> dplyr::mutate(arm = 1)"),
    "sim <- solve(Some_Model, ev, nSub = 3)"
  )) {
    # the literal-name form needs the name registered by a readModelDb() literal
    if (!any(grepl("readModelDb", code))) code <- c(from_db, code)
    got <- flagged(code)
    expect_identical(got$checks, "addlkeepscov-unstated", info = paste(code, collapse = " / "))
    expect_match(got$msg, "solve(", fixed = TRUE)
    expect_match(got$msg, "covariate(s) FED.", fixed = TRUE)
  }
  one <- flagged(c(from_db, "sim <- solve(mod, ev)"))
  expect_match(one$msg, "`solve(mod, ev)` states neither addlKeepsCov nor nonmem = TRUE",
               fixed = TRUE)
})

test_that("a model solve() that states the setting passes", {
  skip_if_not_installed("rxode2")
  mod <- make_lag_model()
  for (arg in c("addlKeepsCov = TRUE", "addlKeepsCov = FALSE", "nonmem = TRUE")) {
    rmd <- write_article(c('mod <- readModelDb("Some_Model")',
                           sprintf("sim <- solve(mod, ev, %s)", arg)))
    expect_identical(lint_vignette(rmd, model = mod)$n, 0L, info = arg)
  }
})

test_that("each solve() is judged on its own, beside matrix solves", {
  skip_if_not_installed("rxode2")
  rmd <- write_article(c(
    'mod <- readModelDb("Some_Model")',
    "a <- solve(mod, ev)",
    "inv <- solve(matrix(c(2, 1, 1, 3), 2))",
    "b <- solve(mod, ev, addlKeepsCov = TRUE)",
    "c <- rxode2::rxSolve(mod, ev)"))
  res <- lint_vignette(rmd, model = make_lag_model())
  expect_identical(lagged_checks(res), rep("addlkeepscov-unstated", 2L))
  expect_match(res$issues$message[[1L]], "`solve(mod, ev)`", fixed = TRUE)
  expect_match(res$issues$message[[2L]], "`rxode2::rxSolve(mod, ev)`", fixed = TRUE)
})

test_that("a solve() whose model cannot be traced is not verifiable, never silently passed", {
  skip_if_not_installed("rxode2")
  mod <- make_lag_model()
  checks <- function(code) lagged_checks(lint_vignette(write_article(code), model = mod))
  # event-table-like arguments: an et() call, a name holding one, a data frame
  # of dosing records, a user function that builds event rows, rxSolve-only
  # arguments such as atol / params
  expect_identical(checks("sim <- solve(other, rxode2::et(amt = 100))"),
                   "addlkeepscov-unverifiable")
  expect_identical(checks(c("ev <- rxode2::et(amt = 100)", "sim <- solve(get_model(), ev)")),
                   "addlkeepscov-unverifiable")
  expect_identical(
    checks(c("pk_ev <- rbind(data.frame(id = 1L, time = 0, amt = 100, evid = 1L),",
             "              data.frame(id = 1L, time = 1:3, amt = 0, evid = 0L))",
             "sim <- solve(pk_ev)")),
    "addlkeepscov-unverifiable")
  expect_identical(
    checks(c("obs_rows <- function(id, times) data.frame(id = id, time = times, evid = 0L)",
             "sim <- solve(obs_rows(1L, 1:3))")),
    "addlkeepscov-unverifiable")
  expect_identical(checks("sim <- solve(thing, atol = 1e-10, rtol = 1e-8)"),
                   "addlkeepscov-unverifiable")
  expect_identical(checks("sim <- solve(thing, params = p)"),
                   "addlkeepscov-unverifiable")
  # a call that names the setting is verified, so it passes
  expect_identical(checks("sim <- solve(thing, params = p, addlKeepsCov = TRUE)"),
                   character())
  # nothing event-table-like, and no model: base::solve()
  expect_identical(checks("x <- solve(thing)"), character())
  expect_identical(checks("x <- solve(thing, other)"), character())
})

test_that("the model's library name counts as a model, and a solve() of it is flagged", {
  skip_if_not_installed("rxode2")
  skip_if_not_installed("nlmixr2lib")
  skip_if_not("Hughes_2022_brepocitinib" %in% nlmixr2lib::modeldb$name)
  rmd <- write_article("sim <- solve(Hughes_2022_brepocitinib, ev)")
  res <- lint_vignette(rmd, model = "Hughes_2022_brepocitinib")
  expect_identical(lagged_checks(res), "addlkeepscov-unstated")
  # with no model name known, the same call is a base::solve() on an unknown
  expect_identical(lint_vignette(rmd)$n, 0L)
})

# --- direct known answers for the resolvers ------------------------------------

test_that(".lintIsModel resolves exactly the forms that hold a model", {
  acc <- new_acc(modelnames = c("mod", "Some_Model"), containers = "models")
  is_model <- function(e) .lintIsModel(e, acc)
  # names and strings known to hold a model, aliases through them
  expect_true(is_model(quote(mod)))
  expect_true(is_model("Some_Model"))
  expect_false(is_model(quote(other)))
  expect_false(is_model("other"))
  # makers, with or without a namespace
  expect_true(is_model(quote(readModelDb("X"))))
  expect_true(is_model(quote(rxode2::rxode2(readModelDb("X")))))
  expect_true(is_model(quote(rxode2::rxode(mod))))
  # wrappers carry the model of their first argument, and only that
  expect_true(is_model(quote(model(mod, {a <- 1}))))
  expect_true(is_model(quote(mod %>% ini(a = 1) %>% zeroRe())))
  expect_true(is_model(quote(suppressMessages(rxode2::zeroRe(mod)))))
  expect_true(is_model(quote(et(mod, amt = 100))))
  expect_false(is_model(quote(zeroRe(other))))
  expect_false(is_model(quote(model(other, {a <- 1}))))
  # elements of a list of models, and the ui of a model
  expect_true(is_model(quote(models$a)))
  expect_true(is_model(quote(models[["a"]])))
  expect_false(is_model(quote(models[1])))
  expect_true(is_model(quote(mod$ui)))
  expect_false(is_model(quote(mod$other)))
  expect_false(is_model(quote(other$ui)))
  # things that are not models
  expect_false(is_model(quote(eigen(A))))
  expect_false(is_model(quote(chol(A))))
  expect_false(is_model(42))
  expect_false(is_model(quote(A %*% B)))
})

test_that(".lintIsContainer finds lists of models", {
  acc <- new_acc(modelnames = "mod", containers = "models")
  is_cont <- function(e) .lintIsContainer(e, acc)
  expect_true(is_cont(quote(models)))
  expect_true(is_cont(quote(list(a = mod))))
  expect_true(is_cont(quote(list(a = readModelDb("X")))))
  expect_true(is_cont(quote(c(mod, models))))
  expect_true(is_cont(quote(lapply(c("X", "Y"), readModelDb))))
  expect_false(is_cont(quote(list(1, 2))))
  expect_false(is_cont(quote(lapply(xs, f))))
  expect_false(is_cont(quote(mod)))
})

test_that(".lintIsEvents counts a whole event table, not something taken from one", {
  acc <- new_acc(eventnames = c("ev", "obs_rows"))
  is_ev <- function(e) .lintIsEvents(e, acc)
  # a name holding one, builders, inline dosing records, and combinations
  expect_true(is_ev(quote(ev)))
  expect_true(is_ev(quote(et(amt = 100))))
  expect_true(is_ev(quote(rxode2::et(c(0, 1000)))))
  expect_true(is_ev(quote(data.frame(id = 1, time = 0, amt = 100))))
  expect_true(is_ev(quote(data.frame(id = 1, time = 1, evid = 0))))
  expect_true(is_ev(quote(obs_rows(1L, 1:3))))
  expect_true(is_ev(quote(rbind(dose_rows, ev))))
  expect_true(is_ev(quote(do.call(rbind, lapply(ids, obs_rows, times = 1:3)))))
  expect_true(is_ev(quote(function(id) data.frame(id = id, evid = 0L))))
  expect_true(is_ev(quote(function(id) { x <- 1; data.frame(id = id, amt = 1) })))
  # not event-like
  expect_false(is_ev(quote(ev$time)))
  expect_false(is_ev(quote(ev[["id"]])))
  expect_false(is_ev(quote(ev[1, ])))
  expect_false(is_ev(quote(ev * 2)))
  expect_false(is_ev(quote(crossprod(ev))))
  expect_false(is_ev(quote(t(X) %*% ev$amt)))
  expect_false(is_ev(quote(data.frame(id = 1, time = 1))))
  expect_false(is_ev(quote(matrix(1:4, 2))))
  expect_false(is_ev(quote(A)))
  expect_false(is_ev(3))
})

test_that(".lintSolveKind separates model solves, untraceable simulations and matrix solves", {
  acc <- new_acc(modelnames = "mod", eventnames = "ev")
  kind <- function(e) .lintSolveKind(e, acc)
  expect_identical(kind(quote(solve(mod, x))), "rxSolve")
  expect_identical(kind(quote(solve(x, mod))), "rxSolve")
  expect_identical(kind(quote(solve(readModelDb("X"), x))), "rxSolve")
  expect_identical(kind(quote(solve(other, ev))), "solve-unresolved")
  expect_identical(kind(quote(solve(other, et(amt = 1)))), "solve-unresolved")
  expect_identical(kind(quote(solve(other, atol = 1e-8))), "solve-unresolved")
  expect_identical(kind(quote(solve(other, params = p))), "solve-unresolved")
  expect_true(is.na(kind(quote(solve(A)))))
  expect_true(is.na(kind(quote(solve(A, b)))))
  expect_true(is.na(kind(quote(solve(A, b, tol = 1e-10)))))
  # taken from or computed on an event table: matrix algebra
  expect_true(is.na(kind(quote(solve(crossprod(X), crossprod(X, ev$time))))))
  expect_true(is.na(kind(quote(solve(t(X) %*% X, t(X) %*% ev$amt)))))
  expect_true(is.na(kind(quote(solve(ev$vectors, c(amt, 0, 0))))))
})

test_that(".lintCallStates reads only the call's own arguments and its control", {
  states <- function(code) .lintCallStates(parse(text = code, keep.source = FALSE)[[1L]])
  # stated, in the call itself
  expect_true(states("rxSolve(m, e, addlKeepsCov = TRUE)"))
  expect_true(states("rxSolve(m, e, addlKeepsCov = FALSE)"))
  expect_true(states("rxSolve(m, e, nonmem = TRUE)"))
  expect_true(states("rxSolve(m, e, nonmem = T)"))
  # a variable states the setting, though its value cannot be read
  expect_true(states("rxSolve(m, e, addlKeepsCov = flag)"))
  expect_true(states("rxSolve(m, e, nonmem = nm)"))
  # inside rxControl() or a control = list()
  expect_true(states("rxSolve(m, e, control = rxControl(addlKeepsCov = TRUE))"))
  expect_true(states("rxSolve(m, e, control = list(nonmem = TRUE))"))
  expect_true(states("rxSolve(m, e, rxControl(nonmem = TRUE))"))
  # not stated
  expect_false(states("rxSolve(m, e)"))
  expect_false(states("rxSolve(m, e, nonmem = FALSE)"))
  expect_false(states("rxSolve(m, e, nonmem = F)"))
  expect_false(states("rxSolve(m, e, nonmem = NULL)"))
  expect_false(states("rxSolve(m, e, addlKeepsCov = NULL)"))
  expect_false(states("rxSolve(m, e, control = ctl)"))
  expect_false(states("rxSolve(m, e, 'addlKeepsCov')"))
  # another call's argument is not this call's
  expect_false(states("rxSolve(m, e, foo(addlKeepsCov = TRUE))"))
  expect_false(states("rxSolve(m, e, foo(nonmem = TRUE))"))
  expect_false(states("rxSolve(m, e, list(addlKeepsCov = TRUE))"))
  expect_false(states("rxSolve(m, e, control = foo(addlKeepsCov = TRUE))"))
  expect_false(states("rxSolve(m, e, note = 'addlKeepsCov = TRUE')"))
})

test_that(".lintWalkSolves records each place the model is solved, in order", {
  acc <- new_acc(modelnames = "mod")
  code <- c(
    "a <- rxode2::rxSolve(mod, ev)",
    "b <- solve(mod, ev, addlKeepsCov = TRUE)",
    'c <- nlmixr2(mod, dat, est = "rxSolve")',
    "d <- simulate(fit)",
    "e <- do.call(rxSolve, args)",
    "f <- solve(A)")
  for (e in as.list(parse(text = code, keep.source = FALSE))) .lintWalkSolves(e, acc)
  expect_identical(vapply(acc$sites, function(s) s$kind, ""),
                   c("rxSolve", "rxSolve", "est-rxSolve", "simulate", "indirect"))
  expect_identical(vapply(acc$sites, function(s) s$stated, NA),
                   c(FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(acc$sites[[2L]]$text, "solve(mod, ev, addlKeepsCov = TRUE)")
  # nothing in it is a simulation
  acc <- new_acc()
  for (e in as.list(parse(text = c("x <- 1", "y <- foo(x)", "A <- solve(M)"),
                          keep.source = FALSE))) .lintWalkSolves(e, acc)
  expect_identical(acc$sites, list())
})

# --- forms that used to pass silently ---------------------------------------------

lagged_one <- function(code, model = make_lag_model(), ...) {
  lint_vignette(write_article(code, ...), model = model)
}

test_that("a model reached through a list, a loop or $ui is resolved", {
  skip_if_not_installed("rxode2")
  from_db <- 'mod <- readModelDb("X")'
  for (code in list(
    c('models <- list(a = readModelDb("X"))', "sim <- solve(models$a, ev)"),
    c(from_db, "models <- list(a = mod)", "sim <- solve(models$a, ev)"),
    c(from_db, "models <- list(a = mod)", 'sim <- solve(models[["a"]], ev)'),
    c(from_db, "models <- list(a = mod)", "models$b <- mod", "sim <- solve(models$b, ev)"),
    c('models <- lapply(c("X", "Y"), readModelDb)', "sim <- solve(models[[1]], ev)"),
    c(from_db, "for (m in list(mod)) sim <- solve(m, ev)"),
    c(from_db, "models <- list(mod)", "for (m in models) sim <- solve(m, ev)"),
    c(from_db, "ui <- mod$ui", "sim <- solve(ui, ev)"),
    c(from_db, "sim <- with(list(m = mod), solve(m, ev))"),
    c(from_db, "sim <- lapply(list(mod), function(m) solve(m, ev))"),
    c(from_db, "sim <- purrr::map(list(mod), ~ solve(.x, ev))")
  )) {
    res <- lagged_one(code)
    expect_identical(lagged_checks(res), "addlkeepscov-unstated",
                     info = paste(code, collapse = " / "))
    expect_match(res$issues$message, "`solve(", fixed = TRUE)
  }
  # a loop variable does not leak into code that follows the scoped call
  res <- lagged_one(c(from_db, "x <- lapply(list(mod), function(m) m)",
                      "y <- solve(m, ev)"))
  expect_identical(res$n, 0L)
})

test_that("a solve() on a parameter of a local function is resolved at the call", {
  skip_if_not_installed("rxode2")
  from_db <- 'mod <- readModelDb("X")'
  for (def in list(
    "runsim <- function(m, e) solve(m, e)",
    "runsim <- function(m, e) m |> solve(e)",
    "runsim <- function(m, e) solve(m, e, atol = 1e-8)",
    "runsim <- function(e, m) { out <- solve(m, e); out }")) {
    res <- lagged_one(c(from_db, def, "a <- runsim(mod, ev)", "b <- runsim(mod, ev2)"))
    # reported once, at the definition, whatever the number of calls
    expect_identical(lagged_checks(res), "addlkeepscov-unstated", info = def)
    expect_match(res$issues$message, "`solve(m, e", fixed = TRUE)
  }
  # arguments matched by name
  res <- lagged_one(c(from_db, "runsim <- function(m, e) solve(m, e)",
                      "a <- runsim(e = ev, m = mod)"))
  expect_identical(lagged_checks(res), "addlkeepscov-unstated")
  # defined but never called with a model: not a simulation
  res <- lagged_one(c(from_db, "runsim <- function(m, e) solve(m, e)",
                      "a <- runsim(A, b)"))
  expect_identical(res$n, 0L)
  res <- lagged_one(c(from_db, "pinv <- function(m) t(m) %*% solve(m %*% t(m))"))
  expect_identical(res$n, 0L)
  # stating the setting in the definition clears it
  res <- lagged_one(c(from_db, "runsim <- function(m, e) solve(m, e, addlKeepsCov = TRUE)",
                      "a <- runsim(mod, ev)"))
  expect_identical(res$n, 0L)
  # an event-like solve in a function never given a model stays reported
  res <- lagged_one(c("runsim <- function(m, e) solve(m, e, atol = 1e-8)"))
  expect_identical(lagged_checks(res), "addlkeepscov-unverifiable")
})

test_that("a solve() the article defines itself is judged by the rxSolve() inside it", {
  skip_if_not_installed("rxode2")
  res <- lagged_one(c(
    'mod <- readModelDb("X")',
    "solve <- function(ev, ...) rxode2::rxSolve(mod, ev, maxsteps = 1e6, ...)",
    "a <- solve(ev1, atol = 1e-8)",
    "b <- solve(ev2)"))
  expect_identical(lagged_checks(res), "addlkeepscov-unstated")
  expect_match(res$issues$message, "`rxode2::rxSolve(mod, ev, maxsteps = 1e+06, ...)`", fixed = TRUE)
})

test_that("solve passed as a value is reported when it travels with a model", {
  skip_if_not_installed("rxode2")
  from_db <- 'mod <- readModelDb("X")'
  for (code in list(
    "x <- lapply(list(mod), solve, events = ev)",
    "x <- purrr::map(list(mod), solve, ev)",
    "x <- Map(solve, list(mod), list(ev))",
    "x <- do.call(solve, list(mod, ev))",
    'x <- do.call("solve", list(mod, ev))',
    'f <- match.fun("solve")',
    'f <- get("solve")',
    "x <- lapply(list(ev), solve, mod)"
  )) {
    expect_identical(lagged_checks(lagged_one(c(from_db, code))),
                     "addlkeepscov-unverifiable", info = code)
  }
  # matrix algebra: solve as a value with nothing model-like beside it
  for (code in c("x <- lapply(mats, solve)", "x <- sapply(mats, solve, b = 1)",
                 "x <- do.call(solve, args)", "f <- solve")) {
    expect_identical(lagged_one(c(from_db, code))$n, 0L, info = code)
  }
})

test_that("rxSolve passed to another function or hidden behind a wrapper is still seen", {
  skip_if_not_installed("rxode2")
  flagged <- function(code) lagged_checks(lagged_one(code))
  expect_identical(flagged("x <- purrr::map(evs, ~ rxode2::rxSolve(mod, .x))"), "addlkeepscov-unstated")
  expect_identical(flagged("x <- Map(function(e) rxSolve(mod, e), evs)"), "addlkeepscov-unstated")
  expect_identical(flagged("x <- with(list(m = mod), rxSolve(m, ev))"), "addlkeepscov-unstated")
  expect_identical(flagged("x <- local({ rxSolve(mod, ev) })"), "addlkeepscov-unstated")
  expect_identical(flagged("x <- purrr::map(evs, rxode2::rxSolve, object = mod)"),
                   "addlkeepscov-unverifiable")
  expect_identical(flagged("x <- Map(rxSolve, list(mod), list(ev))"), "addlkeepscov-unverifiable")
  expect_identical(flagged('x <- do.call("rxSolve", args)'), "addlkeepscov-unverifiable")
})

test_that("an unparsed chunk counts as a solver only when it can be about a model", {
  skip_if_not_installed("rxode2")
  # rxSolve is a solver wherever it appears
  expect_identical(lagged_checks(lagged_one("x <- rxSolve(mod, (ev")),
                   "addlkeepscov-unverifiable")
  # a bare solve( needs the article to work with a model
  expect_identical(lagged_checks(lagged_one(c('mod <- readModelDb("X")', "x <- solve(mod, (ev"))),
                   "addlkeepscov-unverifiable")
  expect_identical(lagged_one("x <- solve(A, (b")$n, 0L)
  # ... the model's own name is enough
  expect_identical(
    lagged_checks(lagged_one("x <- solve(Some_Model, (ev", model = "Some_Model")),
    "addlkeepscov-model")
})

# --- matrix algebra beside an event table ------------------------------------------

test_that("least-squares matrix solves are not simulations even beside dosing rows", {
  skip_if_not_installed("rxode2")
  dat <- c("dat <- data.frame(id = 1, time = 0:3, amt = c(100, 0, 0, 0), evid = c(1, 0, 0, 0))",
           "X <- model.matrix(~ time, dat)")
  expect_identical(
    lagged_one(c(dat, "b <- solve(crossprod(X), crossprod(X, dat$time))"))$n, 0L)
  expect_identical(
    lagged_one(c(dat, "b <- solve(t(X) %*% X, t(X) %*% dat$amt)"))$n, 0L)
  expect_identical(lagged_one(c(dat, "b <- solve(X[1:2, ], dat[1:2, 2])"))$n, 0L)
  # the event table passed whole is event-like
  expect_identical(lagged_checks(lagged_one(c(dat, "b <- solve(other, dat)"))),
                   "addlkeepscov-unverifiable")
})

# --- what counts as stating the setting, end to end ---------------------------------

test_that("the setting must be in the call or its control, and a variable counts", {
  skip_if_not_installed("rxode2")
  flagged <- function(args) {
    res <- lagged_one(sprintf("sim <- rxode2::rxSolve(mod, ev, %s)", args))
    identical(lagged_checks(res), "addlkeepscov-unstated")
  }
  for (args in c("foo(addlKeepsCov = TRUE)", "foo(nonmem = TRUE)",
                 "addlKeepsCov = NULL", "nonmem = NULL", "nonmem = FALSE",
                 "control = ctl", "control = foo(nonmem = TRUE)")) {
    expect_true(flagged(args), info = args)
  }
  for (args in c("addlKeepsCov = keep", "nonmem = nm", "nonmem = T",
                 "control = list(addlKeepsCov = TRUE)",
                 "control = rxControl(nonmem = TRUE)", "rxControl(addlKeepsCov = FALSE)")) {
    expect_false(flagged(args), info = args)
  }
})

# --- nlmixr2(est =), rxSolve spellings, eval = FALSE ---------------------------------

test_that("nlmixr2(est = ) is read for rxSolve anywhere in est, and a non-literal est is reported", {
  skip_if_not_installed("rxode2")
  unverifiable <- function(code) {
    identical(lagged_checks(lagged_one(code)), "addlkeepscov-unverifiable")
  }
  expect_true(unverifiable('x <- nlmixr2(mod, dat, est = "rxSolve")'))
  expect_true(unverifiable('x <- nlmixr2(mod, dat, est = c("rxSolve"))'))
  expect_true(unverifiable('x <- nlmixr2(mod, dat, est = c("focei", "rxSolve"))'))
  expect_true(unverifiable('x <- nlmixr2(mod, dat, "rxSolve")'))
  expect_true(unverifiable("x <- nlmixr2(mod, dat, est = est_name)"))
  expect_true(unverifiable("x <- nlmixr2(mod, dat, est = paste0('r', 'xSolve'))"))
  expect_true(unverifiable('x <- nlmixr2::nlmixr(mod, dat, est = "rxSolve")'))
  expect_identical(lagged_one('x <- nlmixr2(mod, dat, est = "focei")')$n, 0L)
  expect_identical(lagged_one('x <- nlmixr2(mod, dat, est = c("focei", "saem"))')$n, 0L)
  expect_identical(
    lagged_one('x <- nlmixr2(mod, dat, est = "rxSolve", control = rxControl(addlKeepsCov = TRUE))')$n,
    0L)
})

test_that("rxSolve.default is rxSolve, and naming rxSolve without using it is not a use", {
  skip_if_not_installed("rxode2")
  expect_identical(lagged_checks(lagged_one("x <- rxode2::rxSolve.default(mod, ev)")),
                   "addlkeepscov-unstated")
  expect_identical(lagged_checks(lagged_one("x <- rxSolve.default(mod, ev)")),
                   "addlkeepscov-unstated")
  expect_identical(lagged_one("x <- rxSolve.default(mod, ev, nonmem = TRUE)")$n, 0L)
  for (code in c("?rxSolve", "args(rxSolve)", "formals(rxode2::rxSolve)",
                 "body(rxSolve)", "environment(rxSolve)", "help(rxSolve)")) {
    expect_identical(lagged_one(code)$n, 0L, info = code)
  }
})

test_that("a chunk that does not run is not scanned", {
  skip_if_not_installed("rxode2")
  call <- "sim <- rxode2::rxSolve(mod, ev)"
  for (header in c("```{r, eval = FALSE}", "```{r name, echo = TRUE, eval=FALSE}",
                   "```{r eval = F}")) {
    expect_identical(lagged_one(call, header = header)$n, 0L, info = header)
  }
  expect_identical(lagged_one(c("#| eval: false", call))$n, 0L)
  # eval = TRUE, an unrelated option, and a non-literal eval all run
  for (header in c("```{r, eval = TRUE}", "```{r, echo = FALSE}", "```{r, eval = run}")) {
    expect_identical(lagged_checks(lagged_one(call, header = header)),
                     "addlkeepscov-unstated", info = header)
  }
  # an unparseable solver mention in a chunk that does not run is skipped too
  expect_identical(lagged_one("x <- rxSolve(mod, (ev", header = "```{r, eval = FALSE}")$n, 0L)
  # only the chunk that does not run is skipped
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c("```{r, eval = FALSE}", call, "```", "```{r}", call, "```"), rmd)
  expect_identical(lagged_checks(lint_vignette(rmd, model = make_lag_model())),
                   "addlkeepscov-unstated")
  # the other lint rules still read a chunk that does not run
  expect_identical(.lintRcode(rmd), c(call, call))
  chunks <- .lintRchunks(rmd)
  expect_false(chunks[[1L]] |> attr("eval"))
  expect_null(attr(chunks[[2L]], "eval"))
})

# --- a model that cannot be read is reported, not skipped ------------------------------

test_that("a failure reading the model's covariates is reported", {
  skip_if_not_installed("rxode2")
  testthat::local_mocked_bindings(
    .lintModelCovariates = function(ui) stop("covariates boom", call. = FALSE))
  res <- lagged_one("sim <- rxode2::rxSolve(mod, ev)")
  expect_identical(res$issues$check, "addlkeepscov-model")
  expect_match(res$issues$message, "covariates boom", fixed = TRUE)
  expect_match(res$issues$message, "the supplied model", fixed = TRUE)
})

test_that("a model whose statements cannot be read is an error, not an empty result", {
  expect_error(.lintLaggedFCovariates(list(lstExpr = NULL)), "no readable statements")
  expect_error(.lintModelCovariates(list(meta = NULL, allCovs = NULL)), NA)
  expect_identical(.lintModelCovariates(list(meta = list(covariateData = list(A = 1, B = 2)),
                                             allCovs = c("B", "C"))),
                   c("A", "B", "C"))
})

# --- known over-approximations, pinned ---------------------------------------------------

test_that("alag() and f() are not tied to the same compartment", {
  skip_if_not_installed("rxode2")
  ui <- rxode2::rxode2(make_lag_model(f = "f(central) <- exp(lf) * (1 + e_fed * FED)"))
  expect_identical(.lintLaggedFCovariates(ui), "FED")
  expect_identical(
    lagged_checks(lagged_one("sim <- rxode2::rxSolve(mod, ev)",
                             model = make_lag_model(f = "f(central) <- exp(lf) * (1 + e_fed * FED)"))),
    "addlkeepscov-unstated")
})

test_that("assignments are not ordered: every right-hand side a variable gets counts", {
  skip_if_not_installed("rxode2")
  cov_of <- function(...) .lintLaggedFCovariates(rxode2::rxode2(make_lag_model(...)))
  # a covariate that a later assignment overwrites with a constant
  expect_identical(cov_of(pre = c("fx <- FED", "fx <- 1"), f = "f(depot) <- fx"), "FED")
  # a variable read by f() before it is assigned: the read counts, and rxode2
  # itself then takes `fx` for a data column, so it is listed as well
  expect_identical(cov_of(f = c("f(depot) <- fx", "fx <- exp(lf) * (1 + e_fed * FED)")),
                   c("FED", "fx"))
  # a covariate reached only through an if() branch of f() itself
  expect_identical(cov_of(f = "if (FED > 0) f(depot) <- 0.8 else f(depot) <- 1"), "FED")
})

test_that("an article with several models takes the union of their covariates", {
  skip_if_not_installed("rxode2")
  skip_if_not_installed("nlmixr2lib")
  skip_if_not(all(c("Hughes_2022_brepocitinib", "PK_1cmt") %in% nlmixr2lib::modeldb$name))
  # PK_1cmt has no alag(), so its own simulation needs nothing; the article
  # still reports it because Hughes_2022_brepocitinib is in the article too.
  rmd <- write_article(c('hughes <- readModelDb("Hughes_2022_brepocitinib")',
                         'pk <- readModelDb("PK_1cmt")',
                         "a <- rxode2::rxSolve(hughes, ev)",
                         "b <- rxode2::rxSolve(pk, ev)"))
  res <- lint_vignette(rmd)
  expect_identical(lagged_checks(res), rep("addlkeepscov-unstated", 2L))
  expect_match(res$issues$message[[2L]], "`rxode2::rxSolve(pk, ev)`", fixed = TRUE)
})
