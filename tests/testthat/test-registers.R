test_that("parse_registers reads all three registers with expected fields", {
  dir <- make_fixture_refs()
  df <- parse_registers(dir)

  expect_setequal(unique(df$kind), c("covariate", "compartment", "parameter"))
  expect_equal(nrow(df), 4L) # WT, CRCL, central, lcl
  expect_true(all(c("name", "description", "type", "source_aliases",
                    "example_models", "text", "id") %in% names(df)))

  wt <- df[df$name == "WT", ]
  expect_equal(wt$kind, "covariate")
  expect_equal(wt$units, "kg")
  expect_equal(wt$type, "continuous")
  expect_equal(wt$scope, "general")
  expect_match(wt$description, "Body weight")
  # multi-line source-aliases block is folded into one field
  expect_match(wt$source_aliases, "WEIG")
  expect_match(wt$source_aliases, "Wang_2012")
  # FTS document carries searchable concepts but not the example-model list
  expect_match(wt$text, "Body weight")
  expect_false(grepl("Clegg_2024", wt$text))
})

test_that("canonical name is the first token; parenthetical becomes description", {
  dir <- make_fixture_refs()
  df <- parse_registers(dir)
  central <- df[df$name == "central", ]
  expect_equal(central$kind, "compartment")
  expect_equal(central$type, "compartment")
  expect_match(central$role, "Central compartment")
})

test_that("integration: real nlmixr2lib registers parse", {
  skip_if_not_installed("nlmixr2lib")
  dir <- system.file("references", package = "nlmixr2lib")
  skip_if(!nzchar(dir) || !file.exists(file.path(dir, "covariate-columns.md")))
  df <- parse_registers(dir)
  expect_gt(nrow(df), 200L)
  expect_true("WT" %in% df$name[df$kind == "covariate"])
  expect_true("central" %in% df$name[df$kind == "compartment"])
  expect_true("lcl" %in% df$name[df$kind == "parameter"])
})
