# Classifier tests use TF-IDF on a synthetic, well-separated corpus -- fully
# deterministic and network-free (no embedding backend is contacted because the
# stores are built with embed = FALSE, so store$emb is NULL).

mk_corpus <- function() {
  pk  <- rep("clearance volume compartment absorption pharmacokinetic concentration plasma", 6)
  pd  <- rep("emax effect response inhibition stimulation pharmacodynamic biomarker", 6)
  tte <- rep("hazard survival weibull time event death cumulative", 6)
  off <- rep("chlorophyll periodontal questionnaire prevalence review crystal structure", 4)
  data.frame(
    model = paste0("m", seq_len(22)),
    text = c(pk, pd, tte, off),
    archetype = c(rep("PK", 6), rep("PD", 6), rep("TTE", 6), rep("other:nonfit", 4)),
    fittable = c(rep(TRUE, 18), rep(FALSE, 4)),
    stringsAsFactors = FALSE)
}

test_that("build_store builds a TF-IDF store without an embedding backend", {
  s <- build_store(mk_corpus(), embed = FALSE)
  expect_s3_class(s, "nli_store")
  expect_equal(s$n, 22L)
  expect_null(s$emb)
  expect_gt(length(s$tfidf$vocab), 5L)
  expect_output(print(s), "nli_store")
})

test_that("nearest retrieves same-archetype neighbours by keyword similarity", {
  s <- build_store(mk_corpus(), embed = FALSE)
  nb <- nearest(s, "clearance volume compartment absorption", k = 5)
  expect_equal(attr(nb, "mode"), "keyword")
  expect_true(mean(nb$label == "PK") >= 0.6)
})

test_that("classify returns fittable + ranked archetypes", {
  s <- build_store(mk_corpus(), embed = FALSE)
  r <- classify(s, "clearance volume compartment pharmacokinetic plasma", k = 5)
  expect_equal(r$mode, "keyword")
  expect_equal(names(r$archetypes)[1], "PK")
  expect_true(r$fittable)

  r2 <- classify(s, "chlorophyll periodontal questionnaire review prevalence", k = 5)
  expect_equal(names(r2$archetypes)[1], "other:nonfit")
  expect_false(r2$fittable)          # neighbours are non-fittable
})

test_that("evaluate_archetype_cv recovers well-separated clusters", {
  s <- build_store(mk_corpus(), embed = FALSE)
  cv <- evaluate_archetype_cv(s, k = 3)
  expect_gte(cv$accuracy, 0.8)
  expect_equal(cv$n, 22L)
})

test_that("classify_fittable_keyword separates popPK from off-topic text", {
  fit <- classify_fittable_keyword(
    "A population pharmacokinetic model with two compartments and first-order absorption; clearance and volume of distribution were estimated in NONMEM with covariates.")
  expect_true(fit$fittable)
  expect_gt(fit$score, 0)

  non <- classify_fittable_keyword(
    "A systematic review and meta-analysis of periodontal disease prevalence from questionnaire data; gene expression and crystal structure were not assessed.")
  expect_false(non$fittable)
  expect_lt(non$score, 0)
})

test_that("save_store / load_store round-trips", {
  s <- build_store(mk_corpus(), embed = FALSE)
  p <- tempfile(fileext = ".rds")
  save_store(s, p)
  s2 <- load_store(p)
  expect_equal(s2$n, s$n)
  expect_equal(s2$label, s$label)
})

test_that("embed = TRUE with no backend degrades to a TF-IDF-only store", {
  withr::local_options(nlmixr2libingest.llm = "none")  # no network contacted
  expect_warning(s <- build_store(mk_corpus(), embed = TRUE))
  expect_null(s$emb)
})
