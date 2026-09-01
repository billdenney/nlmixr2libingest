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
