# Feature extraction is validated against real nlmixr2lib models (gated on the
# library + rxode2 being installed, for portability of the test suite).

test_that("model_features extracts a simple linCmt PK model", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  f <- model_features("PK_1cmt")
  expect_true(f$parse_ok)
  expect_true(f$lin_cmt)
  expect_equal(f$pk_compartments, 1L)
  expect_equal(f$absorption_type, "first_order")
  expect_equal(f$elimination_type, "linear")
  expect_equal(f$pd_type, "none")
  expect_false(f$atypical)
})

test_that("an ODE 2-cmt PK model is not mislabelled as PD (central != effect)", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  f <- model_features("PK_2cmt_des")
  expect_false(f$lin_cmt)
  expect_equal(f$pk_compartments, 2L)
  expect_gte(f$n_ode, 2L)
  expect_equal(f$pd_type, "none")        # regression: d/dt(central) must not match effect-cmt
  expect_equal(f$elimination_type, "linear")
})

test_that("build_feature_table returns one well-formed row per model", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  ft <- build_feature_table(names = c("PK_1cmt", "PK_2cmt", "PK_3cmt"), quiet = TRUE)
  expect_equal(nrow(ft), 3L)
  expect_true(all(ft$parse_ok))
  expect_true(all(c("model", "absorption_type", "elimination_type", "pd_type",
                    "residual_error_type", "n_eta", "n_cov", "atypical") %in% names(ft)))
  expect_setequal(ft$pk_compartments, c(1L, 2L, 3L))
})
