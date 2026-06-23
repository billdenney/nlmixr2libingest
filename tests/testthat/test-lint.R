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
