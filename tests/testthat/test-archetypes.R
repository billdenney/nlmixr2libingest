# Archetype logic is deterministic, so it is tested on a synthetic feature table
# (no model loading required -- always runs).

mk_ft <- function() data.frame(
  model = c("a_pk1_iv", "b_pk2_oral", "c_oral_mm", "d_tmdd", "e_turnover",
            "f_pbpk", "g_broken", "h_pkpd"),
  parse_ok = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
  atypical = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
  n_ode = c(1, 3, 2, 4, 2, 40, NA, 2),
  pk_compartments = c(1, 2, 1, 2, NA, NA, NA, 1),
  n_eta = c(0, 3, 1, 7, 7, 5, NA, 2),
  n_endpoint = c(1, 1, 1, 1, 2, 1, NA, 2),
  n_cov = c(0, 0, 0, 0, 7, 0, NA, 1),
  lin_cmt = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
  absorption_type = c("iv", "first_order", "first_order", "first_order", "iv", "iv", NA, "first_order"),
  elimination_type = c("linear", "linear", "michaelis_menten", "tmdd", "none", "linear", NA, "linear"),
  pd_type = c("none", "none", "none", "none", "turnover", "none", NA, "indirect_response"),
  residual_error_type = c("prop", "add_prop", "prop", "add_prop", "mixed", "prop", NA, "add"),
  stringsAsFactors = FALSE)

test_that("assign_archetype produces interpretable labels and routes oddballs to other:*", {
  ft <- mk_ft()
  a <- assign_archetype(ft)
  expect_equal(a[1], "PK1cmt_iv_linear")
  expect_equal(a[2], "PK2cmt_oral_linear")
  expect_equal(a[3], "PK1cmt_oral_mm")
  expect_equal(a[4], "TMDD")
  expect_equal(a[5], "PDonly_turnover")
  expect_equal(a[6], "other:complex_system")   # n_ode > 6
  expect_equal(a[7], "other:parse_error")
  expect_equal(a[8], "PK1cmt_oral_linear+PD_indirect_response")
})

test_that("archetype_taxonomy summarises with frequencies, other flag, and examples", {
  ft <- mk_ft()
  tax <- archetype_taxonomy(ft, min_n = 2L)
  expect_true(all(c("archetype", "n", "pct", "is_other", "rare", "example") %in% names(tax)))
  expect_equal(sum(tax$n), nrow(ft))
  expect_equal(sum(tax$is_other), 2L)          # complex_system + parse_error
  expect_true(all(tax$example %in% ft$model))
  expect_true(all(tax$rare))                    # every archetype has 1 member here
})

test_that("archetype_template gives a piped recipe for PK and an exemplar for other", {
  ft <- mk_ft()
  pk <- archetype_template("PK2cmt_oral_linear", ft)
  expect_equal(pk$strategy, "piped")
  expect_equal(pk$base, "PK_2cmt")

  pkpd <- archetype_template("PK1cmt_oral_linear+PD_indirect_response", ft)
  expect_equal(pkpd$base, "PK_1cmt")
  expect_true(any(grepl("addIndirect", pkpd$steps)))

  oth <- archetype_template("other:complex_system", ft)
  expect_equal(oth$strategy, "exemplar")
  expect_match(oth$note, "adapt")
})

test_that("cluster_features adds a cluster label without extra dependencies", {
  ft <- mk_ft()
  out <- cluster_features(ft, k = 3L)
  expect_true("cluster" %in% names(out))
  expect_equal(nrow(out), nrow(ft))
  expect_lte(length(unique(out$cluster)), 3L)
  expect_false(anyNA(out$cluster))
})
