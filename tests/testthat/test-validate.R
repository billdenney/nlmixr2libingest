test_that("validate_model runs the fast-tier stages and returns a terse object", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  res <- validate_model("PK_1cmt")
  expect_s3_class(res, "nli_validation")
  expect_true(all(c("parse", "conventions") %in% res$stages))
  expect_named(res$counts, c("error", "warning", "note"))
  expect_true(res$status %in% c("success", "issues"))
  # status is success iff there are no error/warning rows
  expect_equal(res$status,
               if (res$counts[["error"]] + res$counts[["warning"]] == 0L)
                 "success" else "issues")
})

test_that("a parse failure is fatal and stops downstream stages", {
  skip_if_not_installed("rxode2")
  res <- validate_model("d/dt(central) <- * 2")   # invalid RHS
  expect_s3_class(res, "nli_validation")
  expect_equal(res$status, "issues")
  expect_equal(res$stages, "parse")
  expect_true(any(res$issues$stage == "parse" & res$issues$severity == "error"))
})

test_that("validate_model folds in the source-trace stage when given a paper", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  # paper reports ka/CL/error but NOT vc -> lvc should be flagged unverified
  paper <- paste("One-compartment PK. ka 1.57 /h, apparent clearance CL/F",
                 "2.72 L/h, proportional residual error 50%.")
  res <- validate_model("PK_1cmt", paper = paper)
  expect_true("source_trace" %in% res$stages)
  st <- res$issues[res$issues$stage == "source_trace", , drop = FALSE]
  expect_true(any(grepl("lvc", st$message)))
  expect_true(any(st$severity == "warning"))
  expect_equal(res$status, "issues")   # the unverified value is a warning
})

test_that("print method renders without error for both outcomes", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  res <- validate_model("PK_1cmt",
                        paper = "ka 1.57; CL/F 2.72; proportional error 50%.")
  expect_s3_class(print(res), "nli_validation")   # prints; returns invisibly
})

test_that("full tier handles the vignette-render outcomes", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  # no vignette supplied -> render is skipped with a note; stage still present
  res <- validate_model("PK_1cmt", level = "full")
  expect_true("render" %in% res$stages)
  expect_false("check" %in% res$stages)            # no pkg -> check skipped
  expect_true(any(res$issues$stage == "render" & res$issues$severity == "note"))
  # a missing vignette path -> render error
  res2 <- validate_model("PK_1cmt", level = "full", vignette = "/no/such/file.Rmd")
  expect_true(any(res2$issues$stage == "render" & res2$issues$severity == "error"))
})

test_that("full tier renders a valid vignette cleanly (no issue rows)", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())
  rmd <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "title: t", "output: html_document", "---", "",
               "```{r}", "1 + 1", "```"), rmd)
  res <- validate_model("PK_1cmt", level = "full", vignette = rmd)
  expect_equal(nrow(res$issues[res$issues$stage == "render", ]), 0L)
})

test_that("full tier surfaces an R CMD check failure", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  skip_if_not_installed("rcmdcheck")
  res <- validate_model("PK_1cmt", level = "full",
                        pkg = file.path(tempdir(), "definitely-no-such-pkg"))
  expect_true("check" %in% res$stages)
  expect_true(any(res$issues$stage == "check"))
  expect_equal(res$status, "issues")
})

test_that("model tier runs load_all + render but never the whole-package check", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  # no pkg -> load_all is a note; no vignette -> render is a note. The point is
  # the staging: load_all + render present, whole-package check absent.
  res <- validate_model("PK_1cmt", level = "model")
  expect_true("load_all" %in% res$stages)
  expect_true("render" %in% res$stages)
  expect_false("check" %in% res$stages)
  expect_equal(res$level, "model")
})
