# The register of parked work (inst/design/deferred.yaml): every entry is
# well-formed and none is past its date, and where an entry has a testable
# precondition a test pins the current state so it goes red when the work starts.

read_register <- function() {
  path <- system.file("design", "deferred.yaml", package = "nlmixr2libingest")
  if (!nzchar(path)) path <- testthat::test_path("../../inst/design/deferred.yaml")
  yaml::read_yaml(path)
}

test_that("every deferred item has an id, a description, a pointer and an ISO revisit date", {
  skip_if_not_installed("yaml")
  reg <- read_register()
  expect_gt(length(reg), 0L)
  ids <- vapply(reg, function(e) as.character(e$id), "")
  expect_identical(anyDuplicated(ids), 0L)
  for (e in reg) {
    expect_setequal(names(e), c("id", "description", "pointer", "revisit_by"))
    expect_true(nzchar(trimws(e$description)), info = e$id)
    expect_true(nzchar(trimws(e$pointer)), info = e$id)
    expect_match(as.character(e$revisit_by), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$", info = e$id)
    expect_false(is.na(as.Date(as.character(e$revisit_by), format = "%Y-%m-%d")), info = e$id)
  }
})

test_that("no deferred item is past its revisit date", {
  skip_if_not_installed("yaml")
  for (e in read_register()) {
    due <- as.Date(as.character(e$revisit_by), format = "%Y-%m-%d")
    expect_true(
      Sys.Date() <= due,
      info = paste0(
        "deferred item `", e$id, "` was to be revisited by ", e$revisit_by, ": ",
        gsub("\\s+", " ", e$description), " (see ", gsub("\\s+", " ", e$pointer), "). ",
        "Resolve it and delete its entry from inst/design/deferred.yaml in the same commit, ",
        "or move revisit_by and say why in the commit message."))
  }
})

test_that("addl and ss tables are not expanded: the pairing rule reports them as not verifiable", {
  skip_if_not_installed("rxode2")
  spec <- pair_spec()
  ev <- pair_events(list(c(0, "d", 1), c(5, "o", 1)))
  ev$addl <- c(3L, NA)
  ev$ii <- c(12, NA)
  expect_false(.lintEventRecords(ev, spec)$ok)
  ev$addl <- NULL
  ev$ss <- c(1L, NA)
  expect_false(.lintEventRecords(ev, spec)$ok)
})

test_that("a dose next to the dose is a conservative candidate until its pairing is measured", {
  time <- c(20, 24, 24, 26)
  dose <- c(FALSE, TRUE, TRUE, FALSE)
  # a dose at the dose's time: itself, the record before, and every later record for rate() / dur()
  expect_identical(sort(.lintPairCandidates(time, 2L, 0.5, "f", "modelled", "FALSE", dose)), c(1L, 3L))
  expect_identical(sort(.lintPairCandidates(time, 2L, 0.5, "rate", "modelled", "FALSE", dose)), 3:4)
})
