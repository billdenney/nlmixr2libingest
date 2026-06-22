check_log <- c(
  "* using R version 4.6.0 (2026-04-24)",
  "* using platform: x86_64-pc-linux-gnu",
  "* checking package namespace information ... OK",
  "* checking package dependencies ... OK",
  "* checking R code for possible problems ... NOTE",
  "  foo: no visible binding for global variable 'bar'",
  "* checking examples ... OK",
  "* checking tests ...",
  "  Running 'testthat.R'",
  " ERROR",
  "Running the tests in 'tests/testthat.R' failed.",
  "Last 5 lines of output:",
  "  Error in foo() : boom",
  "* checking PDF version of manual ... OK",
  "Status: 1 ERROR, 1 NOTE"
)

test_that("filter_check_log keeps every problem and drops OK noise", {
  out <- filter_check_log(check_log)
  joined <- paste(out, collapse = "\n")
  # problems retained
  expect_match(joined, "NOTE")
  expect_match(joined, "no visible binding")
  expect_match(joined, "ERROR")
  expect_match(joined, "boom")
  expect_match(joined, "Status: 1 ERROR, 1 NOTE")
  # OK steps dropped
  expect_false(grepl("namespace information ... OK", joined, fixed = TRUE))
  expect_false(grepl("package dependencies ... OK", joined, fixed = TRUE))
})

test_that("keep_notes = FALSE drops NOTE steps but keeps ERROR", {
  out <- filter_check_log(check_log, keep_notes = FALSE)
  joined <- paste(out, collapse = "\n")
  expect_false(grepl("no visible binding", joined))
  expect_match(joined, "ERROR")
  expect_match(joined, "boom")
})

test_that("filter_check_log handles the devtools cli format", {
  dlog <- c(
    "── R CMD check results ── nlmixr2libingest 0.0.0.9000 ──",
    "Duration: 31.2s",
    "❯ checking tests ...",
    "   See below.",
    "   Error: test failed",
    "❯ checking R code for possible problems ... NOTE",
    "   foo: no visible binding for global variable 'bar'",
    "0 errors ✔ | 0 warnings ✔ | 1 note ✖"
  )
  joined <- paste(filter_check_log(dlog), collapse = "\n")
  expect_match(joined, "R CMD check results")
  expect_match(joined, "checking tests")        # header kept with its error
  expect_match(joined, "Error: test failed")
  expect_match(joined, "no visible binding")
  expect_match(joined, "1 note")                # count summary kept
  expect_false(grepl("Duration", joined))       # passing noise dropped
})

test_that("a clean check log reports a clean pass", {
  clean <- c(
    "* checking package namespace information ... OK",
    "* checking examples ... OK",
    "Status: OK"
  )
  out <- filter_check_log(clean)
  joined <- paste(out, collapse = "\n")
  expect_match(joined, "Status: OK")
  expect_false(grepl("no ERROR/WARNING/NOTE", joined)) # status line satisfies "always"
})

test_that("filter_render_log windows around the failure and drops filler", {
  filler <- sprintf("  ordinary text line %d", 1:50)
  render_log <- c(
    "processing file: test.Rmd",
    filler,
    "Quitting from lines 60-62 (test.Rmd)",
    "Error in eval(expr) : object 'x' not found",
    "Execution halted"
  )
  out <- filter_render_log(render_log, context = 5L)
  joined <- paste(out, collapse = "\n")
  expect_match(joined, "Quitting from lines 60-62")
  expect_match(joined, "object 'x' not found")
  expect_match(joined, "Execution halted")
  # far-away filler is dropped
  expect_false(grepl("ordinary text line 1\\b", joined))
})

test_that("filter_render_log reports a clean render", {
  out <- filter_render_log(c("processing file: ok.Rmd", "output created"))
  expect_match(paste(out, collapse = "\n"), "rendered cleanly")
})
