test_that("lookup_canonical finds entries by name, alias, and concept", {
  skip_if_not_installed("duckdb")
  dir <- make_fixture_refs()
  db <- tempfile(fileext = ".duckdb")
  build_register_index(db_path = db, dir = dir, quiet = TRUE)

  # exact canonical name
  r <- lookup_canonical("WT", db_path = db, dir = dir)
  expect_true("WT" %in% r$name)

  # free-text concept (FTS or LIKE over the text document); a multi-word phrase
  # that appears in WT's description should rank WT first.
  r <- lookup_canonical("body weight", db_path = db, dir = dir)
  expect_true("WT" %in% r$name)
  expect_equal(r$name[1], "WT")

  # alias
  r <- lookup_canonical("WEIG", db_path = db, dir = dir)
  expect_true("WT" %in% r$name)

  # kind restriction
  r <- lookup_canonical("clearance", kind = "parameter", db_path = db, dir = dir)
  expect_true("lcl" %in% r$name)
  expect_true(all(r$kind == "parameter"))

  r <- lookup_canonical("central", kind = "compartment", db_path = db, dir = dir)
  expect_true("central" %in% r$name)
})

test_that("lookup respects top_k and summarises example_models to a count", {
  skip_if_not_installed("duckdb")
  dir <- make_fixture_refs()
  db <- tempfile(fileext = ".duckdb")
  build_register_index(db_path = db, dir = dir, quiet = TRUE)

  r <- lookup_canonical("WT", db_path = db, dir = dir)
  expect_false("example_models" %in% names(r)) # excluded by default
  expect_true("n_example_models" %in% names(r))
  expect_equal(r$n_example_models[r$name == "WT"], 2L)

  r_full <- lookup_canonical("WT", full = TRUE, db_path = db, dir = dir)
  expect_true("example_models" %in% names(r_full))

  r1 <- lookup_canonical("clearance", top_k = 1, db_path = db, dir = dir)
  expect_lte(nrow(r1), 1L)
})

test_that("no-match returns zero rows, not an error", {
  skip_if_not_installed("duckdb")
  dir <- make_fixture_refs()
  db <- tempfile(fileext = ".duckdb")
  build_register_index(db_path = db, dir = dir, quiet = TRUE)
  r <- lookup_canonical("zzzznotathing", db_path = db, dir = dir)
  expect_equal(nrow(r), 0L)
})

test_that("index rebuilds when a source register changes", {
  skip_if_not_installed("duckdb")
  withr::local_options(nlmixr2libingest.register_ttl = -1) # force revalidation
  dir <- make_fixture_refs()
  db <- tempfile(fileext = ".duckdb")
  build_register_index(db_path = db, dir = dir, quiet = TRUE)

  expect_equal(nrow(lookup_canonical("albumin", db_path = db, dir = dir)), 0L)

  # add a new canonical covariate and bump mtime
  newdir <- make_fixture_refs(extra_cov = c(
    "### ALB (**canonical for serum albumin**)",
    "- **Description:** Serum albumin concentration.",
    "- **Units:** g/dL",
    "- **Type:** continuous",
    "- **Scope:** general",
    "- **Source aliases:** none.",
    "- **Example models:** `Foo_2020_bar.R`"
  ))
  # point lookups at the augmented dir; ensureIndex should detect the change
  r <- lookup_canonical("albumin", db_path = db, dir = newdir)
  expect_true("ALB" %in% r$name)
})

test_that("render_lookup emits compact markdown", {
  skip_if_not_installed("duckdb")
  dir <- make_fixture_refs()
  db <- tempfile(fileext = ".duckdb")
  build_register_index(db_path = db, dir = dir, quiet = TRUE)
  r <- lookup_canonical("WT", db_path = db, dir = dir)
  md <- render_lookup(r)
  expect_match(md, "### WT")
  expect_match(md, "covariate")
  expect_match(md, "example model")
})
