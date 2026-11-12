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
