test_that("naming_prebrief resolves register covariates with no LLM", {
  skip_if_not_installed("duckdb")
  dir <- make_fixture_refs()
  db <- tempfile(fileext = ".duckdb")
  build_register_index(db_path = db, dir = dir, quiet = TRUE)

  # paper that names covariates by canonical code (WT, CRCL) and an alias (WEIG)
  paper <- paste("Population PK. Covariate analysis tested WT and CRCL on CL/F.",
                 "Body weight (WEIG) was significant on clearance.")
  res <- naming_prebrief(paper, kinds = "covariate", backend = "none",
                         dir = dir, db_path = db)
  expect_s3_class(res, "nli_prebrief")
  cov <- res$covariate$matched
  expect_true(all(c("WT", "CRCL") %in% cov$canonical))
  # WT matched via both its name and an alias -> a single collapsed row
  expect_equal(sum(cov$canonical == "WT"), 1L)
  expect_identical(res$backend, "none")
})

test_that("naming_prebrief ignores covariates the paper does not mention", {
  skip_if_not_installed("duckdb")
  dir <- make_fixture_refs()
  db <- tempfile(fileext = ".duckdb")
  build_register_index(db_path = db, dir = dir, quiet = TRUE)

  paper <- "Population PK with body weight WT only; no renal covariate."
  res <- naming_prebrief(paper, kinds = "covariate", backend = "none",
                         dir = dir, db_path = db)
  cov <- res$covariate$matched
  expect_true("WT" %in% cov$canonical)
  expect_false("CRCL" %in% cov$canonical)   # CRCL/creatinine not in the paper
})

test_that("render_prebrief produces compact markdown", {
  skip_if_not_installed("duckdb")
  dir <- make_fixture_refs()
  db <- tempfile(fileext = ".duckdb")
  build_register_index(db_path = db, dir = dir, quiet = TRUE)

  res <- naming_prebrief("Covariates WT and CRCL on CL/F.", kinds = "covariate",
                         backend = "none", dir = dir, db_path = db)
  md <- render_prebrief(res)
  expect_type(md, "character")
  expect_match(md, "WT")
  expect_match(md, "pre-brief", ignore.case = TRUE)
})
