# Fittable / archetype classifier over a similarity store (R/vectordb.R).
#
# Given a new paper's text, predict (a) is this a fittable popPK/PD model paper?
# and (b) the top archetype priors. Uses embedding k-NN when available, else
# TF-IDF k-NN (the validated default path). Outputs are PRIORS for the agent /
# runner to act on -- annotate-first; auto-skip only after held-out validation.

#' Classify a paper from its text against a labelled store
#'
#' @param store An `nli_store` from [build_store()] / [build_model_store()].
#' @param text The paper text (e.g. a trimmed abstract+methods).
#' @param k Neighbours to poll.
#' @param fittable_threshold Similarity-weighted fittable fraction above which
#'   the paper is called fittable.
#' @return A list: `fittable` (logical/NA), `fittable_score`, `archetypes` (top-3
#'   similarity-weighted votes), `mode` (`"embedding"`/`"keyword"`), `neighbors`.
#' @export
classify <- function(store, text, k = 15L, fittable_threshold = 0.5) {
  nb <- nearest(store, text, k = k)
  w <- pmax(nb$sim, 0)
  if (sum(w) == 0) w <- rep(1, nrow(nb))
  has_f <- !is.na(nb$fittable)
  fscore <- if (!any(has_f)) NA_real_ else
    sum(w[has_f] * nb$fittable[has_f]) / sum(w[has_f])
  votes <- tapply(w, nb$label, sum)
  votes <- sort(votes / sum(w), decreasing = TRUE)
  list(
    fittable = if (is.na(fscore)) NA else fscore >= fittable_threshold,
    fittable_score = fscore,
    archetypes = utils::head(votes, 3L),
    mode = attr(nb, "mode"),
    neighbors = nb
  )
}

# Vocabulary signalling a fittable popPK/PD modelling paper vs an off-topic /
# non-fittable one (review, in-vitro, epidemiology, structural biology, ...).
.popPkTerms <- c(
  "pharmacokinet", "pharmacodynamic", "clearance", "volume of distribution",
  "compartment", "nonmem", "monolix", "pop pk", "poppk", "population pk",
  "absorption", "elimination", "bioavailability", "interindividual",
  "inter-individual", "random effect", "covariate", "concentration-time",
  "first-order", "michaelis", "auc", "cmax", "half-life", "dosing regimen",
  "plasma concentration", "model was developed", "objective function")
.nonPkTerms <- c(
  "in vitro", "crystal structure", "chlorophyll", "periodont", "questionnaire",
  "gene expression", "prevalence", "epidemiolog", "systematic review",
  "meta-analysis", "molecular docking", "molecular dynamics", "case report",
  "in silico screening", "assay development", "biomarker discovery")

#' Heuristic fittable score from popPK vocabulary (no store / no LLM)
#'
#' A standalone fallback for when no labelled corpus is available: counts popPK
#' modelling terms against off-topic terms. Coarse by design -- a prior, not a
#' verdict.
#'
#' @param text Paper text.
#' @param min_score Net term score at/above which the paper is called fittable.
#' @return A list: `fittable`, `score`, `pos_hits`, `neg_hits`.
#' @export
classify_fittable_keyword <- function(text, min_score = 3L) {
  t <- tolower(paste(text, collapse = " "))
  pos <- sum(vapply(.popPkTerms, function(p) grepl(p, t, fixed = TRUE), logical(1L)))
  neg <- sum(vapply(.nonPkTerms, function(p) grepl(p, t, fixed = TRUE), logical(1L)))
  list(fittable = (pos - neg) >= min_score, score = pos - neg,
       pos_hits = pos, neg_hits = neg)
}

#' Leave-one-out archetype accuracy of a store (k-NN cross-validation)
#'
#' Each document's archetype is predicted from its nearest neighbours (excluding
#' itself); accuracy is the fraction correct. Validates the similarity + voting
#' pipeline on real labels before any reliance on its predictions.
#'
#' @param store An `nli_store` (must have `label`).
#' @param k Neighbours.
#' @return A list: `accuracy`, `n`, `k`, `per_class` (accuracy by archetype).
#' @export
evaluate_archetype_cv <- function(store, k = 15L) {
  M <- if (!is.null(store$emb)) store$emb else store$tfidf_matrix
  S <- M %*% t(M)
  diag(S) <- -Inf
  n <- store$n
  pred <- character(n)
  for (i in seq_len(n)) {
    ord <- order(S[i, ], decreasing = TRUE)[seq_len(min(k, n - 1L))]
    votes <- tapply(pmax(S[i, ord], 0), store$label[ord], sum)
    pred[i] <- if (all(is.na(votes))) NA_character_ else names(votes)[which.max(votes)]
  }
  ok <- pred == store$label
  per <- tapply(ok, store$label, mean)
  list(accuracy = mean(ok, na.rm = TRUE), n = n, k = k,
       per_class = sort(per))
}

#' Build a similarity store from the nlmixr2lib model library
#'
#' Assembles a labelled corpus from the model registry (description, label,
#' parameters, DV) with the Phase-1 archetype as the label and `fittable = TRUE`
#' (all library models are fittable). The archetype k-NN over this store gives
#' the archetype prior; adding a negative (non-fittable) paper set enables the
#' fittable decision (wired in the runner integration).
#'
#' @param ft A feature table; built via [build_feature_table()] if `NULL`.
#' @param embed Also store embeddings if a backend is available.
#' @param quiet Suppress progress.
#' @return An `nli_store`.
#' @export
build_model_store <- function(ft = NULL, embed = FALSE, quiet = FALSE) {
  .requireModelPkgs()
  if (is.null(ft)) ft <- build_feature_table(quiet = quiet)
  reg <- nlmixr2lib::modeldb
  reg <- reg[match(ft$model, reg$name), , drop = FALSE]
  blank <- function(x) ifelse(is.na(x), "", as.character(x))
  text <- trimws(paste(blank(reg$description), blank(reg$label),
                       gsub(",", " ", blank(reg$parameters)),
                       gsub(",", " ", blank(reg$DV))))
  df <- data.frame(model = ft$model, text = text,
                   archetype = assign_archetype(ft), fittable = TRUE,
                   stringsAsFactors = FALSE)
  build_store(df, embed = embed)
}
