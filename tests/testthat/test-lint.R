# The lagged-dose pairing rule runs the code that builds each simulation's event
# table; these tests are about the other rules, whose articles leave the event
# table undefined, so it is off here (test-lint-pairing.R tests it).
withr::local_options(nlmixr2libingest.lint_pairing = FALSE)

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

test_that(".lintRchunks splits the R chunks of an Rmd", {
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c("prose `r 1 + 1`", "```{r a}", "x <- 1", "y <- 2", "```",
               "more prose", "```{r}", "z <- 3", "```", "```{r empty}", "```"), rmd)
  expect_identical(.lintRchunks(rmd),
                   list(c("x <- 1", "y <- 2"), "z <- 3", character()))
  none <- withr::local_tempfile(fileext = ".Rmd")
  writeLines("no chunks here", none)
  expect_identical(.lintRchunks(none), list())
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
  # direct dependence
  expect_identical(lag_covs(), "FED")
  # through two intermediate assignments
  expect_identical(
    lag_covs(pre = c("fa <- 1 + e_fed * FED", "fb <- fa * 0.9", "fx <- exp(lf) * fb"),
           f = "f(depot) <- fx"),
    "FED")
  # only through an if() condition: FED never appears on a right-hand side
  expect_identical(
    lag_covs(pre = c("if (FED > 0) {", "  fx <- exp(lf) * 0.8", "} else {",
                   "  fx <- exp(lf)", "}"),
           f = "f(depot) <- fx"),
    "FED")
  # several covariates, sorted, each reached a different way
  expect_identical(
    lag_covs(pre = "fx <- exp(lf) * (WT / 70)", f = "f(depot) <- fx * (1 + e_fed * FED)"),
    c("FED", "WT"))
  # the source spellings F() / lag() are the same statements
  expect_identical(
    lag_covs(f = "F(depot) = exp(lf) * (1 + e_fed * FED)", lag = "lag(depot) <- exp(ltlag)"),
    "FED")
  # a covariate that is not listed in covariateData is still found
  expect_identical(lag_covs(covariate_data = FALSE), "FED")
})

test_that(".lintLaggedFCovariates ignores models the rule does not apply to", {
  skip_if_not_installed("rxode2")
  # alag() with a constant f(): WT is a covariate of the model, but not of f()
  expect_identical(lag_covs(f = "f(depot) <- exp(lf)"), character())
  # a covariate f() without alag()
  expect_identical(lag_covs(lag = NULL), character())
  # alag() without any f()
  expect_identical(lag_covs(f = NULL), character())
  # neither
  expect_identical(lag_covs(f = NULL, lag = NULL), character())
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
  expect_match(msg, "With alag(), which record supplies f()'s covariates depends on the setting and the dose type.", fixed = TRUE)
  expect_match(msg, "By default (addlKeepsCov = FALSE) a bolus dose reads the record in force when it arrives, so a record at the dose's time or inside the lag window that carries another occasion's covariates changes the bioavailability", fixed = TRUE)
  expect_match(msg, "an infusion (rate = -1, rate = -2 or a fixed rate) reads its own record unless another record shares its dose time.", fixed = TRUE)
  expect_match(msg, "addlKeepsCov = TRUE and nonmem = TRUE read the dose's own record for a bolus, which is NONMEM's pairing, but for an infusion they read the record before the dose or a record at the dose's time, not the dose's own record.", fixed = TRUE)
  expect_match(msg, "Give every record the covariates of the dose that opened its interval, state addlKeepsCov = FALSE, and say in the article that you did.", fixed = TRUE)
  expect_match(msg, "TRUE or nonmem = TRUE also passes this check, but matches NONMEM only for bolus doses.", fixed = TRUE)
  # TRUE is not presented as NONMEM's behaviour in general, and the author is
  # told which setting to state rather than left to choose one
  expect_false(grepl("NONMEM uses", msg, fixed = TRUE))
  expect_false(grepl("which you chose", msg, fixed = TRUE))
})

test_that("every lagged-dose finding recommends FALSE and says TRUE matches NONMEM only for bolus doses", {
  skip_if_not_installed("rxode2")
  # one article per way a finding is produced: an rxSolve() call that states
  # nothing, one with a control argument that cannot be read, a solve() with no
  # traceable model, a simulation without a readable call, and a chunk that
  # does not parse
  articles <- list(
    unstated = list(
      "sim <- rxode2::rxSolve(mod, events = ev)",
      "add addlKeepsCov = FALSE to this call (or inside rxControl() given as control =); "),
    control = list(
      "sim <- rxode2::rxSolve(mod, events = ev, control = ctl)",
      "state addlKeepsCov = FALSE in the call itself, or give control = rxControl(addlKeepsCov = FALSE) inline so it can be read; "),
    unresolved = list(
      "sim <- solve(thing, params = p)",
      "assign the model from readModelDb(), modellib() or rxode2() and solve it by that name, or put addlKeepsCov = FALSE in this call; "),
    unreadable = list(
      'fit <- nlmixr2(mod, dat, est = "rxSolve")',
      'solve with rxSolve() directly, or put addlKeepsCov = FALSE in this call (for nlmixr2(est = "rxSolve"), through rxControl()); '),
    unparsed = list(
      "sim <- rxSolve(mod, (ev",
      "fix the chunk so it parses, then state addlKeepsCov = FALSE in each solve; "))
  for (nm in names(articles)) {
    iss <- lagged_one(articles[[nm]][[1L]])$issues
    iss <- iss[grepl("^addlkeepscov", iss$check), ]
    expect_identical(nrow(iss), 1L, info = nm)
    expect_true(startsWith(iss$fix, articles[[nm]][[2L]]), info = nm)
    expect_match(iss$fix, "give every record the covariates of the dose that opened its interval and say in the article that you did", fixed = TRUE, info = nm)
    expect_match(iss$fix, "addlKeepsCov = TRUE or nonmem = TRUE also pass this check, but match NONMEM only for bolus doses", fixed = TRUE, info = nm)
    expect_match(iss$message, "matches NONMEM only for bolus doses", fixed = TRUE, info = nm)
    for (txt in c(iss$message, iss$fix)) {
      expect_false(grepl("(NONMEM)", txt, fixed = TRUE), info = nm)
      expect_false(grepl("NONMEM uses", txt, fixed = TRUE), info = nm)
      expect_false(grepl("which you chose", txt, fixed = TRUE), info = nm)
    }
  }
  # the finding for a model that cannot be read does not mention NONMEM
  res <- lagged_one("sim <- solve(Some_Model, ev)", model = "Some_Model")
  expect_identical(lagged_checks(res), "addlkeepscov-model")
  expect_identical(res$n, 1L)
  expect_match(res$issues$message, "cannot read ", fixed = TRUE)
  expect_false(grepl("NONMEM", res$issues$message, fixed = TRUE))
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
  expect_equal(count_unstated("sim <- rxSolve(mod, ev)"), 1L)
  expect_equal(count_unstated("sim <- rxode2::rxSolve(mod, ev)"), 1L)
  expect_equal(count_unstated("sim <- rxode2:::rxSolve(mod, ev)"), 1L)
  expect_equal(count_unstated("sim <- ev %>% rxode2::rxSolve(mod, .)"), 1L)
  skip_if(getRversion() < "4.2.0", "the native pipe placeholder needs R >= 4.2")
  expect_equal(count_unstated("sim <- ev |> rxode2::rxSolve(mod, events = _)"), 1L)
  expect_equal(count_unstated("sim <- ev |> rxSolve(object = mod, events = _)"), 1L)
  # arguments over several lines, with a comment holding an unbalanced paren
  expect_equal(count_unstated(c(
    "sim <- rxode2::rxSolve(",
    "  mod,",
    "  events = ev,  # a comment with a ( in it",
    "  nSub = 10",
    ")")), 1L)
  # the setting on a piped, multi-line call
  expect_equal(count_unstated(c(
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
  expect_identical(lagged_checks_of('fit <- nlmixr2(mod, dat, est = "rxSolve")'),
                   "addlkeepscov-unverifiable")
  expect_identical(lagged_checks_of("s <- simulate(fit, nsim = 5)"),
                   "addlkeepscov-unverifiable")
  expect_identical(lagged_checks_of("s <- do.call(rxode2::rxSolve, args)"),
                   "addlkeepscov-unverifiable")
  expect_identical(lagged_checks_of("s <- lapply(evs, rxSolve, object = mod)"),
                   "addlkeepscov-unverifiable")
  expect_identical(lagged_checks_of('s <- do.call("rxSolve", args)'),
                   "addlkeepscov-unverifiable")
  # a chunk that mentions a solver but does not parse cannot be read either
  expect_identical(lagged_checks_of("s <- rxSolve(mod, (ev"),
                   "addlkeepscov-unverifiable")
  # a call that names the setting itself is verified, so it passes
  expect_identical(
    lagged_checks_of('fit <- nlmixr2(mod, dat, est = "rxSolve", control = rxControl(addlKeepsCov = TRUE))'),
    character())
  expect_identical(lagged_checks_of("s <- simulate(fit, nsim = 5, nonmem = TRUE)"),
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
    res <- lagged_one(code)
    expect_identical(lagged_checks(res), "addlkeepscov-unstated",
                     info = paste(code, collapse = " / "))
    expect_match(res$issues$message, "solve(", fixed = TRUE)
    expect_match(res$issues$message, "covariate(s) FED.", fixed = TRUE)
  }
  one <- lagged_one(c(from_db, "sim <- solve(mod, ev)"))
  expect_match(one$issues$message, "`solve(mod, ev)` states neither addlKeepsCov nor nonmem = TRUE",
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
  # event-table-like arguments: an et() call, a name holding one, a data frame
  # of dosing records, a user function that builds event rows, rxSolve-only
  # arguments such as atol / params
  expect_identical(lagged_checks_of("sim <- solve(other, rxode2::et(amt = 100))"),
                   "addlkeepscov-unverifiable")
  expect_identical(lagged_checks_of(c("ev <- rxode2::et(amt = 100)", "sim <- solve(get_model(), ev)")),
                   "addlkeepscov-unverifiable")
  expect_identical(
    lagged_checks_of(c("pk_ev <- rbind(data.frame(id = 1L, time = 0, amt = 100, evid = 1L),",
             "              data.frame(id = 1L, time = 1:3, amt = 0, evid = 0L))",
             "sim <- solve(pk_ev)")),
    "addlkeepscov-unverifiable")
  expect_identical(
    lagged_checks_of(c("obs_rows <- function(id, times) data.frame(id = id, time = times, evid = 0L)",
             "sim <- solve(obs_rows(1L, 1:3))")),
    "addlkeepscov-unverifiable")
  expect_identical(lagged_checks_of("sim <- solve(thing, atol = 1e-10, rtol = 1e-8)"),
                   "addlkeepscov-unverifiable")
  expect_identical(lagged_checks_of("sim <- solve(thing, params = p)"),
                   "addlkeepscov-unverifiable")
  # a call that names the setting is verified, so it passes
  expect_identical(lagged_checks_of("sim <- solve(thing, params = p, addlKeepsCov = TRUE)"),
                   character())
  # nothing event-table-like, and no model: base::solve()
  expect_identical(lagged_checks_of("x <- solve(thing)"), character())
  expect_identical(lagged_checks_of("x <- solve(thing, other)"), character())
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
  # names and strings known to hold a model, aliases through them
  expect_true(.lintIsModel(quote(mod), acc))
  expect_true(.lintIsModel("Some_Model", acc))
  expect_false(.lintIsModel(quote(other), acc))
  expect_false(.lintIsModel("other", acc))
  # makers, with or without a namespace
  expect_true(.lintIsModel(quote(readModelDb("X")), acc))
  expect_true(.lintIsModel(quote(rxode2::rxode2(readModelDb("X"))), acc))
  expect_true(.lintIsModel(quote(rxode2::rxode(mod)), acc))
  # wrappers carry the model of their first argument, and only that
  expect_true(.lintIsModel(quote(model(mod, {a <- 1})), acc))
  expect_true(.lintIsModel(quote(mod %>% ini(a = 1) %>% zeroRe()), acc))
  expect_true(.lintIsModel(quote(suppressMessages(rxode2::zeroRe(mod))), acc))
  expect_true(.lintIsModel(quote(et(mod, amt = 100)), acc))
  expect_false(.lintIsModel(quote(zeroRe(other)), acc))
  expect_false(.lintIsModel(quote(model(other, {a <- 1})), acc))
  # elements of a list of models, and the ui of a model
  expect_true(.lintIsModel(quote(models$a), acc))
  expect_true(.lintIsModel(quote(models[["a"]]), acc))
  expect_false(.lintIsModel(quote(models[1]), acc))
  expect_true(.lintIsModel(quote(mod$ui), acc))
  expect_false(.lintIsModel(quote(mod$other), acc))
  expect_false(.lintIsModel(quote(other$ui), acc))
  # things that are not models
  expect_false(.lintIsModel(quote(eigen(A)), acc))
  expect_false(.lintIsModel(quote(chol(A)), acc))
  expect_false(.lintIsModel(42, acc))
  expect_false(.lintIsModel(quote(A %*% B), acc))
})

test_that(".lintIsContainer finds lists of models", {
  acc <- new_acc(modelnames = "mod", containers = "models")
  expect_true(.lintIsContainer(quote(models), acc))
  expect_true(.lintIsContainer(quote(list(a = mod)), acc))
  expect_true(.lintIsContainer(quote(list(a = readModelDb("X"))), acc))
  expect_true(.lintIsContainer(quote(c(mod, models)), acc))
  expect_true(.lintIsContainer(quote(lapply(c("X", "Y"), readModelDb)), acc))
  expect_false(.lintIsContainer(quote(list(1, 2)), acc))
  expect_false(.lintIsContainer(quote(lapply(xs, f)), acc))
  expect_false(.lintIsContainer(quote(mod), acc))
})

test_that(".lintIsEvents counts a whole event table, not something taken from one", {
  acc <- new_acc(eventnames = c("ev", "obs_rows"))
  # a name holding one, builders, inline dosing records, and combinations
  expect_true(.lintIsEvents(quote(ev), acc))
  expect_true(.lintIsEvents(quote(et(amt = 100)), acc))
  expect_true(.lintIsEvents(quote(rxode2::et(c(0, 1000))), acc))
  expect_true(.lintIsEvents(quote(data.frame(id = 1, time = 0, amt = 100)), acc))
  expect_true(.lintIsEvents(quote(data.frame(id = 1, time = 1, evid = 0)), acc))
  expect_true(.lintIsEvents(quote(obs_rows(1L, 1:3)), acc))
  expect_true(.lintIsEvents(quote(rbind(dose_rows, ev)), acc))
  expect_true(.lintIsEvents(quote(do.call(rbind, lapply(ids, obs_rows, times = 1:3))), acc))
  expect_true(.lintIsEvents(quote(function(id) data.frame(id = id, evid = 0L)), acc))
  expect_true(.lintIsEvents(quote(function(id) { x <- 1; data.frame(id = id, amt = 1) }), acc))
  # not event-like
  expect_false(.lintIsEvents(quote(ev$time), acc))
  expect_false(.lintIsEvents(quote(ev[["id"]]), acc))
  expect_false(.lintIsEvents(quote(ev[1, ]), acc))
  expect_false(.lintIsEvents(quote(ev * 2), acc))
  expect_false(.lintIsEvents(quote(crossprod(ev)), acc))
  expect_false(.lintIsEvents(quote(t(X) %*% ev$amt), acc))
  expect_false(.lintIsEvents(quote(data.frame(id = 1, time = 1)), acc))
  expect_false(.lintIsEvents(quote(matrix(1:4, 2)), acc))
  expect_false(.lintIsEvents(quote(A), acc))
  expect_false(.lintIsEvents(3, acc))
})

test_that(".lintSolveKind separates model solves, untraceable simulations and matrix solves", {
  acc <- new_acc(modelnames = "mod", eventnames = "ev")
  expect_identical(.lintSolveKind(quote(solve(mod, x)), acc), "rxSolve")
  expect_identical(.lintSolveKind(quote(solve(x, mod)), acc), "rxSolve")
  expect_identical(.lintSolveKind(quote(solve(readModelDb("X"), x)), acc), "rxSolve")
  expect_identical(.lintSolveKind(quote(solve(other, ev)), acc), "solve-unresolved")
  expect_identical(.lintSolveKind(quote(solve(other, et(amt = 1))), acc), "solve-unresolved")
  expect_identical(.lintSolveKind(quote(solve(other, atol = 1e-8)), acc), "solve-unresolved")
  expect_identical(.lintSolveKind(quote(solve(other, params = p)), acc), "solve-unresolved")
  expect_true(is.na(.lintSolveKind(quote(solve(A)), acc)))
  expect_true(is.na(.lintSolveKind(quote(solve(A, b)), acc)))
  expect_true(is.na(.lintSolveKind(quote(solve(A, b, tol = 1e-10)), acc)))
  # taken from or computed on an event table: matrix algebra
  expect_true(is.na(.lintSolveKind(quote(solve(crossprod(X), crossprod(X, ev$time))), acc)))
  expect_true(is.na(.lintSolveKind(quote(solve(t(X) %*% X, t(X) %*% ev$amt)), acc)))
  expect_true(is.na(.lintSolveKind(quote(solve(ev$vectors, c(amt, 0, 0))), acc)))
})

test_that(".lintCallStates reads only the call's own arguments and its control", {
  # stated, in the call itself
  expect_true(call_states("rxSolve(m, e, addlKeepsCov = TRUE)"))
  expect_true(call_states("rxSolve(m, e, addlKeepsCov = FALSE)"))
  expect_true(call_states("rxSolve(m, e, nonmem = TRUE)"))
  expect_true(call_states("rxSolve(m, e, nonmem = T)"))
  # a variable counts for addlKeepsCov, which the author addressed directly
  expect_true(call_states("rxSolve(m, e, addlKeepsCov = flag)"))
  # inside rxControl() or a control = list()
  expect_true(call_states("rxSolve(m, e, control = rxControl(addlKeepsCov = TRUE))"))
  expect_true(call_states("rxSolve(m, e, control = list(nonmem = TRUE))"))
  expect_true(call_states("rxSolve(m, e, rxControl(nonmem = TRUE))"))
  # not stated
  expect_false(call_states("rxSolve(m, e)"))
  expect_false(call_states("rxSolve(m, e, nonmem = FALSE)"))
  expect_false(call_states("rxSolve(m, e, nonmem = F)"))
  expect_false(call_states("rxSolve(m, e, nonmem = NULL)"))
  # nonmem sets addlKeepsCov only when TRUE, and a variable may hold FALSE
  expect_false(call_states("rxSolve(m, e, nonmem = nm)"))
  expect_false(call_states("rxSolve(m, e, nonmem = c(TRUE))"))
  expect_false(call_states("rxSolve(m, e, nonmem = !FALSE)"))
  expect_false(call_states("rxSolve(m, e, control = list(nonmem = nm))"))
  expect_false(call_states("rxSolve(m, e, addlKeepsCov = NULL)"))
  expect_false(call_states("rxSolve(m, e, control = ctl)"))
  expect_false(call_states("rxSolve(m, e, 'addlKeepsCov')"))
  # another call's argument is not this call's
  expect_false(call_states("rxSolve(m, e, foo(addlKeepsCov = TRUE))"))
  expect_false(call_states("rxSolve(m, e, foo(nonmem = TRUE))"))
  expect_false(call_states("rxSolve(m, e, list(addlKeepsCov = TRUE))"))
  expect_false(call_states("rxSolve(m, e, control = foo(addlKeepsCov = TRUE))"))
  expect_false(call_states("rxSolve(m, e, note = 'addlKeepsCov = TRUE')"))
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
  expect_identical(lagged_checks_of("x <- purrr::map(evs, ~ rxode2::rxSolve(mod, .x))"), "addlkeepscov-unstated")
  expect_identical(lagged_checks_of("x <- Map(function(e) rxSolve(mod, e), evs)"), "addlkeepscov-unstated")
  expect_identical(lagged_checks_of("x <- with(list(m = mod), rxSolve(m, ev))"), "addlkeepscov-unstated")
  expect_identical(lagged_checks_of("x <- local({ rxSolve(mod, ev) })"), "addlkeepscov-unstated")
  expect_identical(lagged_checks_of("x <- purrr::map(evs, rxode2::rxSolve, object = mod)"),
                   "addlkeepscov-unverifiable")
  expect_identical(lagged_checks_of("x <- Map(rxSolve, list(mod), list(ev))"), "addlkeepscov-unverifiable")
  expect_identical(lagged_checks_of('x <- do.call("rxSolve", args)'), "addlkeepscov-unverifiable")
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

test_that("the setting must be in the call or its control; a variable counts for addlKeepsCov only", {
  skip_if_not_installed("rxode2")
  for (args in c("foo(addlKeepsCov = TRUE)", "foo(nonmem = TRUE)",
                 "addlKeepsCov = NULL", "nonmem = NULL", "nonmem = FALSE",
                 "nonmem = nm", "control = rxControl(nonmem = nm)",
                 "control = list(nonmem = FALSE)")) {
    expect_true(rx_unstated(args), info = args)
  }
  # a control that is not an rxControl()/list() call may hold the setting: it
  # cannot be read, which is not the same as not stated
  for (args in c("control = ctl", "control = foo(nonmem = TRUE)",
                 "control = ctl, nonmem = FALSE")) {
    expect_true(is_unverifiable(sprintf("sim <- rxode2::rxSolve(mod, ev, %s)", args)),
                info = args)
  }
  for (args in c("addlKeepsCov = keep", "nonmem = T",
                 "control = list(addlKeepsCov = TRUE)",
                 "control = rxControl(nonmem = TRUE)", "rxControl(addlKeepsCov = FALSE)",
                 "control = ctl, addlKeepsCov = FALSE", "control = ctl, nonmem = TRUE")) {
    expect_false(rx_unstated(args), info = args)
    expect_identical(lagged_checks_of(sprintf("sim <- rxode2::rxSolve(mod, ev, %s)", args)),
                     character(), info = args)
  }
})

# --- nlmixr2(est =), rxSolve spellings, eval = FALSE ---------------------------------

test_that("nlmixr2(est = ) is read for rxSolve anywhere in est, and a non-literal est is reported", {
  skip_if_not_installed("rxode2")
  expect_true(is_unverifiable('x <- nlmixr2(mod, dat, est = "rxSolve")'))
  expect_true(is_unverifiable('x <- nlmixr2(mod, dat, est = c("rxSolve"))'))
  expect_true(is_unverifiable('x <- nlmixr2(mod, dat, est = c("focei", "rxSolve"))'))
  expect_true(is_unverifiable('x <- nlmixr2(mod, dat, "rxSolve")'))
  expect_true(is_unverifiable("x <- nlmixr2(mod, dat, est = est_name)"))
  expect_true(is_unverifiable("x <- nlmixr2(mod, dat, est = paste0('r', 'xSolve'))"))
  expect_true(is_unverifiable('x <- nlmixr2::nlmixr(mod, dat, est = "rxSolve")'))
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
  expect_identical(.lintCode(.lintRchunks(rmd)), c(call, call))
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
  # a covariate that a later assignment overwrites with a constant
  expect_identical(lag_covs(pre = c("fx <- FED", "fx <- 1"), f = "f(depot) <- fx"), "FED")
  # a variable read by f() before it is assigned: the read counts, and rxode2
  # itself then takes `fx` for a data column, so it is listed as well
  expect_identical(lag_covs(f = c("f(depot) <- fx", "fx <- exp(lf) * (1 + e_fed * FED)")),
                   c("FED", "fx"))
  # a covariate reached only through an if() branch of f() itself
  expect_identical(lag_covs(f = "if (FED > 0) f(depot) <- 0.8 else f(depot) <- 1"), "FED")
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

# --- round 2: shadowing, indirect calls, tail forms, reassignment ------------------

test_that("only a top-level solve() the article defines shadows the model solve()", {
  skip_if_not_installed("rxode2")
  # defined inside a function body or local(): the article-wide solve is untouched
  for (def in c("g <- function() { solve <- function(a) 1; solve(2) }",
                "local({ solve <- function(a) 1 })",
                "h <- function() local({ solve <- function(a) 1 })")) {
    expect_identical(lagged_checks_of(c(from_db, def, "sim <- solve(mod, ev)")),
                     "addlkeepscov-unstated", info = def)
  }
  # defined at top level, it is the article's own function, and the rxSolve()
  # inside it is what is judged
  expect_identical(
    lagged_checks_of(c(from_db, "solve <- function(m, e) m$ui", "sim <- solve(mod, ev)")),
    character())
  expect_identical(
    lagged_checks_of(c(from_db, "solve <- function(m, e) rxode2::rxSolve(m, e)",
                       "sim <- solve(mod, ev)")),
    "addlkeepscov-unstated")
  # the registered methods are not shadowed by a local solve
  expect_identical(
    lagged_checks_of(c(from_db, "solve <- function(m, e) m$ui", "sim <- solve.rxUi(mod, ev)")),
    "addlkeepscov-unstated")
})

test_that("an indirect call is stated when its arguments or a list among them say so", {
  skip_if_not_installed("rxode2")
  for (code in c(
    "x <- do.call(rxode2::rxSolve, list(mod, ev, addlKeepsCov = TRUE))",
    'x <- do.call("rxSolve", list(mod, ev, nonmem = TRUE))',
    "x <- do.call(solve, list(mod, ev, addlKeepsCov = FALSE))",
    "x <- do.call(rxSolve, c(list(mod, ev), addlKeepsCov = TRUE))",
    "x <- lapply(list(mod), solve, ev, addlKeepsCov = TRUE)",
    "x <- purrr::map(list(mod), rxode2::rxSolve, ev, nonmem = TRUE)",
    "x <- Map(rxSolve, list(mod), list(ev), MoreArgs = list(addlKeepsCov = TRUE))")) {
    expect_identical(lagged_checks_of(c(from_db, "ev <- et(amt = 1)", code)), character(),
                     info = code)
  }
  # without the setting, or with it inside another call, they stay unverifiable
  for (code in c("x <- do.call(rxode2::rxSolve, list(mod, ev))",
                 "x <- do.call(rxSolve, list(mod, ev, foo(addlKeepsCov = TRUE)))",
                 "x <- do.call(solve, list(mod, ev, nonmem = FALSE))",
                 "x <- lapply(list(mod), solve, ev)")) {
    expect_identical(lagged_checks_of(c(from_db, "ev <- et(amt = 1)", code)),
                     "addlkeepscov-unverifiable", info = code)
  }
})

test_that("the rest of the apply family, foreach and the registered solve methods are followed", {
  skip_if_not_installed("rxode2")
  pre <- c('mods <- list(a = readModelDb("A"))', "ev <- et(amt = 1)")
  for (code in c(
    "x <- purrr::map2(mods, list(ev), ~ solve(.x, .y))",
    "x <- purrr::pmap(list(mods, list(ev)), function(m, e) solve(m, e))",
    "x <- purrr::imap(mods, function(m, n) solve(m, ev))",
    "x <- purrr::map_dfr(mods, ~ solve(.x, ev))",
    "x <- purrr::map_dfc(mods, ~ solve(.x, ev))",
    "x <- purrr::map_lgl(mods, function(m) is.null(solve(m, ev)))",
    "purrr::walk(mods, function(m) solve(m, ev))",
    "x <- foreach::foreach(m = mods) %do% solve(m, ev)",
    "x <- foreach(m = mods) %dopar% solve(m, ev)",
    "x <- lapply(mods, \\(m) solve(m, ev))",
    # the methods rxode2 registers, by their full names
    "x <- rxode2:::solve.rxUi(mods$a, ev)",
    "x <- solve.rxUi(mods$a, ev)",
    "x <- solve.rxode2(mods$a, ev)")) {
    expect_identical(lagged_checks_of(c(pre, code)), "addlkeepscov-unstated", info = code)
  }
  # a list of models built by a function mapped over their names
  for (build in c('mods <- lapply(c("A", "B"), function(n) readModelDb(n))',
                  'mods <- lapply(c("A", "B"), \\(n) readModelDb(n))',
                  'mods <- lapply(c("A", "B"), ~ readModelDb(.x))',
                  'mods <- setNames(lapply(c("A", "B"), readModelDb), c("A", "B"))',
                  'mods <- rev(lapply(c("A", "B"), readModelDb))',
                  'mods <- purrr::map(c("A", "B"), readModelDb)')) {
    expect_identical(lagged_checks_of(c(build, "ev <- et(amt = 1)", "x <- solve(mods[[1]], ev)")),
                     "addlkeepscov-unstated", info = build)
  }
  # a model assigned inside a loop over names is resolved where it is used
  expect_identical(
    lagged_checks_of(c('for (nm in c("A", "B")) {', "  mod <- readModelDb(nm)",
                       "  sim <- solve(mod, ev)", "}")),
    "addlkeepscov-unstated")
  # `mod$simulate()` and `mod$solve()` are not rxode2 methods, so they are not simulations
  expect_identical(lagged_checks_of(c(from_db, "x <- mod$simulate(ev)")), character())
  expect_identical(lagged_checks_of(c(from_db, "x <- mod$solve(ev)")), character())
})

test_that("a name assigned something else stops being a model, a list of models or an event table", {
  skip_if_not_installed("rxode2")
  expect_identical(lagged_one(c(from_db, "mod <- diag(2)", "x <- solve(mod, c(1, 2))"))$n, 0L)
  expect_identical(
    lagged_one(c(from_db, "models <- list(a = mod)", "for (m in models) NULL",
                 "m <- diag(2)", "x <- solve(m)"))$n,
    0L)
  expect_identical(lagged_one(c("ev <- et(amt = 1)", "ev <- diag(2)", "x <- solve(ev)"))$n, 0L)
  expect_identical(
    lagged_one(c('models <- list(a = readModelDb("A"))', "models <- NULL",
                 "x <- solve(models$a, ev)"))$n,
    0L)
  # ... but a value built from the name itself keeps it
  expect_identical(
    lagged_checks_of(c("ev <- et(amt = 1)", "ev <- ev[order(ev$id), ]", "x <- solve(other, ev)")),
    "addlkeepscov-unverifiable")
  expect_identical(
    lagged_checks_of(c(from_db, "mod <- mod |> zeroRe()", "x <- solve(mod, ev)")),
    "addlkeepscov-unstated")
  # a name can change kind: an event table that becomes a model
  expect_identical(
    lagged_checks_of(c("x <- et(amt = 1)", 'x <- readModelDb("A")', "y <- solve(x, ev)")),
    "addlkeepscov-unstated")
})

# --- limits, pinned -------------------------------------------------------------------

test_that("the forms the rule cannot follow stay silent, as documented", {
  skip_if_not_installed("rxode2")
  # built from text or fetched by a name it cannot read
  expect_true(is_silent('x <- eval(parse(text = "rxSolve(mod, ev)"))'))
  expect_true(is_silent('x <- utils::getFromNamespace("rxSolve", "rxode2")(mod, ev)'))
  # solve handed over with nothing model-like beside it
  expect_true(is_silent("x <- lapply(mats, solve)"))
  expect_true(is_silent("x <- do.call(solve, args)"))
  # a model returned by a user function: seen only when the arguments are event-like
  expect_true(is_silent(c('getm <- function(n) readModelDb(n)', 'm <- getm("A")', "x <- solve(m, dat)")))
  expect_identical(
    lagged_checks_of(c('getm <- function(n) readModelDb(n)', 'm <- getm("A")',
                       "ev <- et(amt = 1)", "x <- solve(m, ev)")),
    "addlkeepscov-unverifiable")
  # a model reaching a local function through lapply()/do.call(), not a direct call
  expect_true(is_silent(c("runsim <- function(m, e) solve(m, e)", "x <- lapply(list(mod), runsim, ev)")))
  expect_true(is_silent(c("runsim <- function(m, e) solve(m, e)", "x <- do.call(runsim, list(mod, ev))")))
  expect_identical(
    lagged_checks_of(c(from_db, "runsim <- function(m, e) solve(m, e, atol = 1e-8)",
                       "x <- lapply(list(mod), runsim, ev)")),
    "addlkeepscov-unverifiable")
  # a model that is only the `model` argument is not a tracked name, so solving it
  # by its name is not seen unless the article also reads it with readModelDb()
  expect_identical(lagged_one("sim <- solve(Some_Model, ev)")$n, 0L)
  expect_identical(
    lagged_checks_of(c('x <- readModelDb("Some_Model")', "sim <- solve(Some_Model, ev)")),
    "addlkeepscov-unstated")
  # a record with amt = / evid = built inline is an event table, matrix or not
  expect_identical(lagged_checks_of("x <- solve(rbind(amt = c(1, 2), time = c(3, 4)))"),
                   "addlkeepscov-unverifiable")
})

test_that("a wrapper that forwards ... is reported at its definition even when callers state the setting", {
  skip_if_not_installed("rxode2")
  res <- lagged_one(c(from_db, "sim <- function(m, e, ...) rxode2::rxSolve(m, e, ...)",
                      "x <- sim(mod, ev, addlKeepsCov = TRUE)"))
  expect_identical(lagged_checks(res), "addlkeepscov-unstated")
  expect_match(res$issues$message, "`rxode2::rxSolve(m, e, ...)`", fixed = TRUE)
})

test_that("a covariate that never changes between occasions is reported like any other", {
  skip_if_not_installed("rxode2")
  mod <- make_lag_model(f = "f(depot) <- exp(lf) * (WT / 70)")
  res <- lagged_one("sim <- rxode2::rxSolve(mod, ev)", model = mod)
  expect_identical(lagged_checks(res), "addlkeepscov-unstated")
  expect_match(res$issues$message, "covariate(s) WT.", fixed = TRUE)
  expect_identical(lagged_one("sim <- rxode2::rxSolve(mod, ev, addlKeepsCov = FALSE)",
                              model = mod)$n, 0L)
})

test_that("an unreadable control argument is not verifiable, not unstated", {
  skip_if_not_installed("rxode2")
  res <- lagged_one(c("ctl <- rxControl(addlKeepsCov = TRUE)",
                      "sim <- rxode2::rxSolve(mod, ev, control = ctl)"))
  expect_identical(lagged_checks(res), "addlkeepscov-unverifiable")
  msg <- res$issues$message
  expect_match(msg, "cannot verify `rxode2::rxSolve(mod, ev, control = ctl)`", fixed = TRUE)
  expect_match(msg, "its control argument, `ctl`, is a variable or call whose setting could not be read",
               fixed = TRUE)
  expect_false(grepl("states neither", msg, fixed = TRUE))
  # a model solve() with such a control is judged the same way
  expect_identical(
    lagged_checks_of(c(from_db, "sim <- solve(mod, ev, control = ctl)")),
    "addlkeepscov-unverifiable")
  # without a control argument the call plainly states neither
  expect_match(lagged_one("sim <- rxode2::rxSolve(mod, ev)")$issues$message,
               "states neither addlKeepsCov nor nonmem = TRUE", fixed = TRUE)
})

# --- chunks: fences, options, comments ----------------------------------------------

test_that("chunk fences are read the way knitr reads them", {
  call <- "sim <- rxode2::rxSolve(mod, ev)"
  expect_identical(read_chunks(c("  ```{r}", paste0("  ", call), "  ```")), list(call))
  expect_identical(read_chunks(c("\t```{r}", paste0("\t", call), "\t```")), list(call))
  expect_identical(read_chunks(c("````{r}", call, "````")), list(call))
  expect_identical(read_chunks(c("> ```{r}", paste0("> ", call), "> ```")), list(call))
  # every spelling of the R engine and its options
  for (header in c("```{r}", "```{R}", "```{r a}", "```{r, a}", "```{r a, echo = FALSE}",
                   "```{r,echo=FALSE}", "```{r }")) {
    expect_identical(read_chunks(c(header, call, "```")), list(call), info = header)
  }
  # other engines and plain code blocks are not R chunks
  expect_identical(read_chunks(c("```{python}", call, "```")), list())
  expect_identical(read_chunks(c("```{bash}", call, "```")), list())
  expect_identical(read_chunks(c("```r", call, "```")), list())
  expect_identical(read_chunks(c("```", call, "```")), list())
  # the R chunk after another engine's chunk is still found
  expect_identical(read_chunks(c("```{python}", "x = 1", "```", "```{r}", call, "```")), list(call))
})

test_that("the fence patterns agree with knitr's", {
  skip_if_not_installed("knitr")
  lines <- c("```{r}", "```{r a, echo=FALSE}", "  ```{r}", "````{r}", "> ```{r}", "```{python}",
             "```r", "```", "````", "  ```", "> ```", "``` ", "```{r}x", "text ```{r}", "``{r}",
             "```{r, fig.cap='a}'}", "```{}", "```{ r}")
  md <- knitr::all_patterns$md
  expect_identical(grepl(.lintChunkBegin, lines), grepl(md$chunk.begin, lines))
  expect_identical(grepl(.lintChunkEnd, lines), grepl(md$chunk.end, lines))
})

test_that("eval options are read outside quoted strings, and the unsupported forms are pinned", {
  skip_if_not_installed("rxode2")
  call <- "sim <- rxode2::rxSolve(mod, ev)"
  expect_true(is_skipped("```{r, eval = FALSE}"))
  expect_true(is_skipped("```{r eval=F}"))
  expect_true(is_skipped("```{r, echo = TRUE, eval = FALSE, include = FALSE}"))
  expect_true(is_skipped("```{r}", c("#| eval: false", call)))
  expect_true(is_skipped("```{r}", c("#| eval: False", call)))
  expect_true(is_skipped("```{r}", c("#| echo = FALSE", "#| eval = FALSE", call)))
  # an option name inside a quoted string, or part of another name, is not the option
  expect_false(is_skipped("```{r, fig.cap='eval = FALSE in cap'}"))
  expect_false(is_skipped('```{r, fig.cap="a, eval = FALSE"}'))
  expect_false(is_skipped("```{r, fig.cap='a eval = FALSE, b'}"))
  expect_false(is_skipped("```{r, my_eval = FALSE}"))
  expect_false(is_skipped("```{r, eval = TRUE}"))
  # not honoured: an expression, a variable, a document-wide default, and an
  # option line that comes after code
  expect_false(is_skipped("```{r, eval = c(FALSE)}"))
  expect_false(is_skipped("```{r, eval = !TRUE}"))
  expect_false(is_skipped("```{r, eval = skip_sims}"))
  expect_false(is_skipped("```{r}", c(call, "#| eval: false")))
  rmd <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c("```{r setup}", "knitr::opts_chunk$set(eval = FALSE)", "```",
               "```{r}", call, "```"), rmd)
  expect_identical(lagged_checks(lint_vignette(rmd, model = make_lag_model())),
                   "addlkeepscov-unstated")
})

test_that(".lintStripComments blanks comments, not the # inside a string", {
  expect_identical(
    .lintStripComments(c("x <- 1 # a", "y <- '#' # b", "# whole line", 'z <- "a # b"')),
    c("x <- 1 ", "y <- '#' ", "", 'z <- "a # b"'))
  expect_identical(.lintStripComments(character()), character())
  # code that does not parse falls back to cutting at the first #
  expect_identical(.lintStripComments(c("x <- (", "y # c")), c("x <- (", "y "))
})

test_that("the text rules ignore comments", {
  skip_if_not_installed("rxode2")
  # 2. amt-named-scalar
  expect_identical(lint_checks('# ev <- et(amt = doses["depot"])'), character())
  expect_identical(lint_checks('ev <- et(amt = doses["depot"])'), "amt-named-scalar")
  # 4. cohort-too-large
  expect_identical(lint_checks("# sim <- rxSolve(m, e, nSub = 5000)"), character())
  expect_identical(lint_checks("sim <- rxSolve(m, e, nSub = 5000) # a cohort"), "cohort-too-large")
  expect_identical(lint_checks('x <- "a # b"; sim <- rxSolve(m, e, nSub = 5000)'), "cohort-too-large")
  expect_identical(lint_checks(c("x <- rxSolve(m, e,", "  nSub = 5000 # large", ")")), "cohort-too-large")
  # 3. pknca-zero-row needs PKNCA and the filter in code
  filt <- "nca_in <- dplyr::filter(sim, time > 0)"
  expect_identical(lint_checks(c("# PKNCA", filt)), character())
  expect_identical(lint_checks(c("library(PKNCA)", paste("#", filt))), character())
  expect_identical(lint_checks(c("library(PKNCA)", filt)), "pknca-zero-row")
  # 5. assertions are gated on set.seed and rxSolve in code
  expect_identical(
    lint_checks(c("# set.seed(1)", "sim <- rxSolve(mod, ev)", "stopifnot(all(diff(pv) < 0))")),
    character())
  expect_identical(
    lint_checks(c("set.seed(1)", "# sim <- rxSolve(mod, ev)", "stopifnot(all(diff(pv) < 0))")),
    character())
  # 1. cmt-observable
  mod <- make_lag_model()
  obs <- 'ev <- et(time = 1:24, cmt = "Cc")'
  expect_identical(lint_vignette(write_article(paste("#", obs)), model = mod)$n, 0L)
  expect_identical(lint_vignette(write_article(obs), model = mod)$issues$check, "cmt-observable")
})

# --- rule 1 reads the article's own model, and says when it cannot -----------------------

test_that("the cmt-observable rule falls back to the models the article reads", {
  skip_if_not_installed("rxode2")
  skip_if_not_installed("nlmixr2lib")
  skip_if_not("PK_1cmt" %in% nlmixr2lib::modeldb$name)
  bad <- write_article(c('mod <- readModelDb("PK_1cmt")',
                         'ev <- et(amt = 100, cmt = "depot") |> et(time = 1:24, cmt = "Cc")'))
  expect_identical(lint_vignette(bad)$issues$check, "cmt-observable")
  # the model argument wins over the article's own reading
  expect_identical(lint_vignette(bad, model = "PK_1cmt")$issues$check, "cmt-observable")
  good <- write_article(c('mod <- readModelDb("PK_1cmt")',
                          'ev <- et(amt = 100, cmt = "depot") |> et(time = 1:24, cmt = "central")'))
  expect_identical(lint_vignette(good)$n, 0L)
})

test_that("a model that cannot be read is reported by the cmt-observable rule, quietly", {
  skip_if_not_installed("rxode2")
  cmt <- 'ev <- et(amt = 100, cmt = "depot")'
  # as the model argument
  res <- expect_silent(lint_vignette(write_article(cmt), model = "no_such_model_9999"))
  expect_identical(res$issues$check, "cmt-observable-model")
  expect_match(res$issues$message, "`no_such_model_9999`", fixed = TRUE)
  # as a name the article reads
  res <- expect_silent(lint_vignette(write_article(c('mod <- readModelDb("No_Such_Model_9999")', cmt))))
  expect_identical(res$issues$check, "cmt-observable-model")
  expect_match(res$issues$message, "`No_Such_Model_9999`", fixed = TRUE)
  # nothing to check when the article names no compartment by cmt =
  expect_identical(
    lint_vignette(write_article('mod <- readModelDb("No_Such_Model_9999")'))$n, 0L)
  # an article that both names a compartment and simulates is told by both rules
  res <- lint_vignette(write_article(c('mod <- readModelDb("No_Such_Model_9999")', cmt,
                                       "sim <- rxSolve(mod, ev)")))
  expect_setequal(res$issues$check, c("cmt-observable-model", "addlkeepscov-model"))
})

# --- modellib() is a model loader too ------------------------------------------------

test_that("modellib() resolves a model the way readModelDb() does", {
  skip_if_not_installed("rxode2")
  for (code in c('mod <- modellib("Some_Model")', 'mod <- nlmixr2lib::modellib("Some_Model")',
                 'mod <- rxode2::rxode2(modellib("Some_Model"))',
                 'mods <- lapply(c("A", "B"), modellib)')) {
    pre <- if (grepl("^mods", code)) c(code, "mod <- mods[[1]]") else code
    expect_identical(lagged_checks_of(c(pre, "sim <- solve(mod, ev)")), "addlkeepscov-unstated",
                     info = code)
  }
  # its literal name is a tracked name, and enough to treat a bare solve( as model work
  expect_identical(lagged_checks_of(c('x <- modellib("Some_Model")', "sim <- solve(Some_Model, ev)")),
                   "addlkeepscov-unstated")
  expect_identical(lagged_checks_of(c('x <- modellib("Some_Model")', "x <- solve(mod, (ev")),
                   "addlkeepscov-unverifiable")
})

test_that("the model is taken from modellib() in the article when none is given", {
  skip_if_not_installed("rxode2")
  skip_if_not_installed("nlmixr2lib")
  skip_if_not("Hughes_2022_brepocitinib" %in% nlmixr2lib::modeldb$name)
  rmd <- write_article(c('mod <- rxode2::rxode2(modellib("Hughes_2022_brepocitinib"))',
                         "sim <- rxode2::rxSolve(mod, ev)"))
  res <- lint_vignette(rmd)
  expect_identical(lagged_checks(res), "addlkeepscov-unstated")
  expect_match(res$issues$message, "covariate(s) DOSE_HIGH, FED_HIGHFAT.", fixed = TRUE)
  gone <- write_article(c('mod <- modellib("No_Such_Model_9999")', "sim <- rxode2::rxSolve(mod, ev)"))
  expect_identical(lint_vignette(gone)$issues$check, "addlkeepscov-model")
})

# --- round 3: members, option lines, tabs, branching values -------------------------

test_that("a call to rxSolve fetched from an object is reported unless it states the setting", {
  skip_if_not_installed("rxode2")
  for (code in c('x <- asNamespace("rxode2")$rxSolve(mod, ev)',
                 'ns <- asNamespace("rxode2"); x <- ns$rxSolve(mod, ev)',
                 'x <- ns[["rxSolve"]](mod, ev)',
                 "x <- ns$rxSolve.default(mod, ev)",
                 "x <- obj@rxSolve(mod, ev)")) {
    # one finding, not one for the member access and another for the call
    expect_identical(lagged_checks_of(code), "addlkeepscov-unverifiable", info = code)
  }
  for (code in c("x <- ns$rxSolve(mod, ev, addlKeepsCov = TRUE)",
                 'x <- ns[["rxSolve"]](mod, ev, nonmem = TRUE)',
                 "x <- ns$rxSolve(mod, ev, control = rxControl(addlKeepsCov = FALSE))")) {
    expect_identical(lagged_checks_of(code), character(), info = code)
  }
  # rxSolve as a member passed on as a value
  expect_identical(lagged_checks_of("f <- ns$rxSolve"), "addlkeepscov-unverifiable")
  expect_identical(lagged_checks_of("x <- lapply(evs, ns$rxSolve, mod)"), "addlkeepscov-unverifiable")
  # members that are not rxSolve are not looked at
  for (code in c("x <- ns$solve(mod, ev)", "x <- ns$other(mod, ev)", "x <- obj$rxSolveSimple(mod)",
                 'x <- ns[["other"]](mod, ev)')) {
    expect_identical(lagged_checks_of(code), character(), info = code)
  }
})

test_that("a solve method fetched by name is a documented silent limit", {
  skip_if_not_installed("rxode2")
  expect_true(is_silent('x <- getS3method("solve", "rxUi")(mod, ev)'))
  expect_true(is_silent('x <- utils::getS3method("solve", "rxUi")(mod, ev)'))
})

test_that(".lintStripComments cuts by the comment's own text, so tabs and wide characters are safe", {
  expect_identical(.lintStripComments("\t\tx <- 1 # all(a == 0)"), "\t\tx <- 1 ")
  expect_identical(.lintStripComments(c("\tx <- 1 # c", "\t\ty <- '#' # d")),
                   c("\tx <- 1 ", "\t\ty <- '#' "))
  expect_identical(.lintStripComments("x <- '日本' # all(a == 0)"), "x <- '日本' ")
  expect_identical(.lintStripComments("\t# only a comment"), "\t")
  # at the rule level: the commented assertion and cohort are not findings
  expect_identical(lint_checks(c("set.seed(1)", "sim <- rxSolve(mod, ev)",
                                 "\t\tx <- 1 # stopifnot(all(pct == 0))")), character())
  expect_identical(lint_checks("\t\tsim <- rxSolve(m, e) # nSub = 5000"), character())
})

test_that("comments are found whatever the session's parse options are", {
  withr::local_options(keep.parse.data = FALSE, keep.source = FALSE)
  # a # inside a string survives only if the parse data was used, not the fallback
  expect_identical(.lintStripComments(c("x <- '#' # c", "y <- \"a # b\"")),
                   c("x <- '#' ", "y <- \"a # b\""))
  expect_identical(getOption("keep.parse.data"), FALSE)
  expect_identical(getOption("keep.source"), FALSE)
})

test_that("option lines are read the way xfun::divide_chunk and knitr read them", {
  # known answers
  expect_false(.lintOptionLinesNoEval(character()))
  for (v in c("false", "False", "FALSE", "no", "No", "NO", "n", "N", "off", "Off", "OFF",
              "false # why")) {
    expect_true(.lintOptionLinesNoEval(paste("eval:", v)), info = v)
  }
  for (v in c("F", "f", "\"false\"", "'no'", "true", "false#no", "TRUE", "0")) {
    expect_false(.lintOptionLinesNoEval(paste("eval:", v)), info = v)
  }
  expect_true(.lintOptionLinesNoEval(c("echo: false", "eval: no")))
  expect_true(.lintOptionLinesNoEval("eval = FALSE"))
  expect_true(.lintOptionLinesNoEval("eval = F"))
  expect_true(.lintOptionLinesNoEval("echo = FALSE, eval = FALSE"))
  expect_true(.lintOptionLinesNoEval(c("echo = FALSE,", "eval = FALSE")))
  expect_false(.lintOptionLinesNoEval("eval = TRUE"))
  expect_false(.lintOptionLinesNoEval("my_eval = FALSE"))
  # an option line needs `#| ` with its space, and only the leading block counts
  expect_false(is_skipped("```{r}", c("#|eval: false", rx_call)))
  expect_false(is_skipped("```{r}", c(rx_call, "#| eval: false")))
  expect_false(is_skipped("```{r}", c("", "#| eval: false", rx_call)))
  expect_true(is_skipped("```{r}", c("#| eval: no", rx_call)))
  expect_true(is_skipped("```{r}", c("#| eval: false # why", rx_call)))
  expect_false(is_skipped("```{r}", c("#| eval: F", rx_call)))
})

test_that("a chunk is skipped exactly when knitr does not run it", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("xfun")
  skip_if_not_installed("yaml")
  cases <- list(
    c("```{r}", "#| eval: false"), c("```{r}", "#| eval: False"), c("```{r}", "#| eval: FALSE"),
    c("```{r}", "#| eval: no"), c("```{r}", "#| eval: No"), c("```{r}", "#| eval: n"),
    c("```{r}", "#| eval: N"), c("```{r}", "#| eval: off"), c("```{r}", "#| eval: Off"),
    c("```{r}", "#| eval: OFF"), c("```{r}", "#| eval: F"), c("```{r}", "#| eval: f"),
    c("```{r}", "#| eval: \"false\""), c("```{r}", "#| eval: true"),
    c("```{r}", "#| eval: false # why"), c("```{r}", "#| eval: false#no"),
    c("```{r}", "#| echo: false", "#| eval: false"),
    c("```{r}", "#| eval = FALSE"), c("```{r}", "#| eval = F"),
    c("```{r}", "#| echo = FALSE, eval = FALSE"), c("```{r}", "#| eval = TRUE"),
    c("```{r}", "#|eval: false"), c("```{r}", "x <- 1", "#| eval: false"),
    c("```{r}", "", "#| eval: false"),
    c("```{r, eval = FALSE}"), c("```{r, eval=F}"), c("```{r eval = FALSE}"),
    c("```{r, fig.cap='a eval = FALSE, b'}"), c("```{r, my_eval = FALSE}"),
    c("```{r, eval = TRUE}"))
  runs <- knitr_ran(cases)
  mine <- vapply(cases, function(ch) {
    !isFALSE(attr(read_chunks(c(ch, "x <- 1", "```"))[[1L]], "eval"))
  }, logical(1L))
  expect_identical(mine, runs, info = paste(cases[mine != runs], collapse = " ; "))
  # the two forms knitr evaluates as expressions are not honoured: knitr skips
  # these chunks and the rule scans them
  not_honoured <- list(c("```{r, eval = !TRUE}"), c("```{r, eval = c(FALSE)}"))
  expect_identical(knitr_ran(not_honoured), c(FALSE, FALSE))
  expect_identical(
    vapply(not_honoured, function(ch) {
      !isFALSE(attr(read_chunks(c(ch, "x <- 1", "```"))[[1L]], "eval"))
    }, logical(1L)),
    c(TRUE, TRUE))
})

test_that("a bare > ends an empty blockquoted line instead of becoming code", {
  expect_identical(read_chunks(c("> ```{r}", "> x <- 1", ">", "> y <- 2", "> ```")),
                   list(c("x <- 1", "", "y <- 2")))
  expect_identical(read_chunks(c("> ```{r}", "> x <- 1", "> ", "> y <- 2", "> ```")),
                   list(c("x <- 1", "", "y <- 2")))
  expect_identical(read_chunks(c("  ```{r}", "  x <- 1", "", "  y <- 2", "  ```")),
                   list(c("x <- 1", "", "y <- 2")))
})

test_that("an assignment that yields a model or event table in every branch keeps the name", {
  skip_if_not_installed("rxode2")
  for (code in c('mod <- if (cond) readModelDb("A") else readModelDb("B")',
                 'mod <- switch(kind, a = readModelDb("A"), b = readModelDb("B"))',
                 'mod <- switch(kind, a = , b = readModelDb("B"), readModelDb("C"))',
                 'mod <- tryCatch(readModelDb("A"), error = function(e) readModelDb("B"))',
                 'mod <- try(readModelDb("A"))',
                 'mod <- local({ x <- 1; readModelDb("A") })',
                 'mod <- { x <- 1; readModelDb("A") }',
                 'mod <- if (a) { readModelDb("A") } else if (b) { readModelDb("B") } else { readModelDb("C") }')) {
    expect_identical(lagged_checks_of(c(code, "sim <- solve(mod, ev)")), "addlkeepscov-unstated",
                     info = code)
  }
  for (code in c("ev <- if (cond) et(amt = 1) else et(amt = 2)",
                 "ev <- switch(kind, a = et(amt = 1), b = et(amt = 2))",
                 "ev <- tryCatch(et(amt = 1), error = function(e) et(amt = 2))",
                 "ev <- local({ x <- 1; et(amt = 1) })")) {
    expect_identical(lagged_checks_of(c(code, "sim <- solve(other, ev)")),
                     "addlkeepscov-unverifiable", info = code)
  }
  # a branch that is not a model or event table, or a missing else, does not
  for (code in c('mod <- if (cond) readModelDb("A")',
                 'mod <- if (cond) readModelDb("A") else NULL',
                 'mod <- switch(kind, a = readModelDb("A"), b = NULL)',
                 'mod <- tryCatch(readModelDb("A"), error = function(e) NULL)',
                 'mod <- tryCatch(readModelDb("A"), error = function(e) stop(e))')) {
    expect_identical(lagged_one(c(code, "sim <- solve(mod, ev)"))$n, 0L, info = code)
  }
  # the same tests, directly
  acc <- new_acc()
  expect_true(.lintIsModel(quote(if (a) readModelDb("A") else readModelDb("B")), acc))
  expect_false(.lintIsModel(quote(if (a) readModelDb("A")), acc))
  expect_true(.lintIsEvents(quote(if (a) et(amt = 1) else et(amt = 2)), acc))
  expect_false(.lintIsEvents(quote(if (a) et(amt = 1) else 0), acc))
})

test_that("names are tracked across the whole article, so a function body can clear a global", {
  skip_if_not_installed("rxode2")
  # documented limit: `ev <- 1` inside f() is read as an assignment to the global ev
  expect_identical(
    lagged_one(c("ev <- et(amt = 1)", "f <- function() { ev <- 1 }", "x <- solve(other, ev)"))$n,
    0L)
  expect_identical(
    lagged_checks_of(c("ev <- et(amt = 1)", "x <- solve(other, ev)")), "addlkeepscov-unverifiable")
})
