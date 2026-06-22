# Archetype taxonomy over the structural feature table (R/features.R).
#
# Rather than opaque unsupervised clustering, archetypes are assigned by
# deterministic rules over the structural features, yielding the human-meaningful
# labels pharmacometricians reason in ("PK2cmt_oral_linear",
# "PK1cmt_oral_linear+PD_indirect_response", "TMDD", ...). This makes the
# taxonomy stable, explainable, and directly mappable to a template. Models that
# do not fit a recognised structure are routed to an explicit `other:*` bucket
# and FLAGGED as high-interest -- never dropped. A complementary data-driven
# view is available via [cluster_features()].

.absLabel <- function(x) switch(as.character(x),
  iv = "iv", first_order = "oral", transit = "transit", zero_order = "zero", "absX")

# Archetype label for one model's features.
.archetypeOne <- function(parse_ok, atypical, n_ode, cmt, abs, elim, pd) {
  if (isFALSE(parse_ok)) return("other:parse_error")
  if (isTRUE(atypical) || (!is.na(n_ode) && n_ode > 6L)) return("other:complex_system")
  if (is.na(elim)) return("other:unclassified")
  cmtL <- if (is.na(cmt)) "x" else as.character(cmt)
  base <-
    if (elim == "tmdd") "TMDD"
    else if (elim == "none")
      paste0("PDonly_", if (is.na(pd) || pd == "none") "structural" else pd)
    else if (elim == "michaelis_menten") paste0("PK", cmtL, "cmt_", .absLabel(abs), "_mm")
    else if (elim == "linear") paste0("PK", cmtL, "cmt_", .absLabel(abs), "_linear")
    else "other:unclassified"
  if (startsWith(base, "other") || startsWith(base, "PDonly")) return(base)
  # PD overlay for PK / TMDD bases
  if (!is.na(pd) && pd != "none") paste0(base, "+PD_", pd) else base
}

#' Assign a structural archetype label to each model
#'
#' Deterministic rule-based labels derived from the feature table. Unrecognised
#' or high-complexity models receive an `other:*` label (flagged, not dropped).
#'
#' @param ft A feature table from [build_feature_table()] / [model_features()].
#' @return A character vector of archetype labels, one per row of `ft`.
#' @export
assign_archetype <- function(ft) {
  stopifnot(all(c("parse_ok", "atypical", "n_ode", "pk_compartments",
                  "absorption_type", "elimination_type", "pd_type") %in% names(ft)))
  mapply(.archetypeOne, ft$parse_ok, ft$atypical, ft$n_ode, ft$pk_compartments,
         ft$absorption_type, ft$elimination_type, ft$pd_type,
         SIMPLIFY = TRUE, USE.NAMES = FALSE)
}

#' Summarise the empirical archetype taxonomy
#'
#' @param ft A feature table from [build_feature_table()].
#' @param min_n Archetypes with fewer than `min_n` members are additionally
#'   flagged `rare = TRUE` (candidates for review, still listed).
#' @return A data frame: `archetype`, `n`, `pct`, `is_other`, `rare`, and a
#'   representative `example` model, ordered by frequency.
#' @export
archetype_taxonomy <- function(ft, min_n = 3L) {
  arch <- assign_archetype(ft)
  tab <- sort(table(arch), decreasing = TRUE)
  ex <- vapply(names(tab), function(a) {
    idx <- which(arch == a)
    # representative = the structurally simplest member (fewest covs + etas)
    simple <- order(ft$n_cov[idx] + ifelse(is.na(ft$n_eta[idx]), 99L, ft$n_eta[idx]))
    ft$model[idx[simple[1L]]]
  }, character(1L))
  data.frame(
    archetype = names(tab),
    n = as.integer(tab),
    pct = round(100 * as.integer(tab) / nrow(ft), 1),
    is_other = startsWith(names(tab), "other:"),
    rare = as.integer(tab) < min_n,
    example = ex,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

# Pipe-recipe (add*/convert* steps) to reach an archetype from a PK base, for
# documentation / guided building. Keyed on archetype label fragments.
.archetypeRecipe <- function(archetype) {
  steps <- character()
  if (grepl("_oral_|_transit_|_zero_", archetype) || grepl("cmt_oral", archetype)) {
    if (grepl("_transit_", archetype)) steps <- c(steps, "addTransit(ntransit = <n>)")
    else if (grepl("_zero_", archetype)) steps <- c(steps, "addCmtProp(prop = 'dur')")
  } else if (grepl("cmt_iv", archetype)) {
    steps <- c(steps, "removeDepot()  # IV: no absorption compartment")
  }
  if (grepl("_mm", archetype)) steps <- c(steps, "convertMM()  # linear -> Michaelis-Menten")
  if (grepl("\\+PD_indirect_response", archetype)) steps <- c(steps, "addIndirect()")
  if (grepl("\\+PD_effect_compartment", archetype)) steps <- c(steps, "addEffectCmtLin()")
  if (grepl("\\+PD_direct_emax", archetype)) steps <- c(steps, "addDirectLin() |> convertEmax()")
  steps
}

# Best base model to start a PK archetype from: the canonical PK_<n>cmt root
# template when the archetype is a plain PK model.
.archetypeBase <- function(archetype) {
  m <- regmatches(archetype, regexpr("^PK([0-9x])cmt", archetype))
  if (length(m) && nzchar(m)) {
    n <- sub("^PK([0-9x])cmt", "\\1", m)
    cand <- paste0("PK_", n, "cmt")
    return(cand)
  }
  NA_character_
}

#' Generate a starting template for an archetype
#'
#' Returns a structured template recommendation: the most representative existing
#' model to adapt (always useful), plus -- for plain PK archetypes -- the
#' canonical `PK_<n>cmt` base and the `add*`/`convert*` pipe recipe to reach the
#' target. For `other:*` / atypical archetypes it returns an exemplar pointer
#' with a note to adapt manually rather than auto-template.
#'
#' @param archetype An archetype label (from [assign_archetype()]).
#' @param ft A feature table (to pick the exemplar). If `NULL`, exemplar is
#'   omitted.
#' @return A list with `archetype`, `strategy` (`"piped"`/`"exemplar"`), `base`,
#'   `steps`, `exemplar`, `n_examples`, and `note`.
#' @export
archetype_template <- function(archetype, ft = NULL) {
  is_other <- startsWith(archetype, "other:")
  base <- if (is_other) NA_character_ else .archetypeBase(archetype)
  steps <- if (is_other) character() else .archetypeRecipe(archetype)
  exemplar <- NA_character_; n_ex <- 0L
  if (!is.null(ft)) {
    arch <- assign_archetype(ft)
    idx <- which(arch == archetype)
    n_ex <- length(idx)
    if (n_ex) {
      simple <- order(ft$n_cov[idx] + ifelse(is.na(ft$n_eta[idx]), 99L, ft$n_eta[idx]))
      exemplar <- ft$model[idx[simple[1L]]]
    }
  }
  strategy <- if (is_other || is.na(base)) "exemplar" else "piped"
  note <- if (is_other)
    "Atypical structure: adapt the closest existing model; do not auto-template."
  else if (strategy == "exemplar")
    "No canonical PK base for this archetype; start from the exemplar model."
  else
    sprintf("Start from %s and apply: %s",
            base, if (length(steps)) paste(steps, collapse = " |> ") else "(no changes)")
  list(archetype = archetype, strategy = strategy, base = base, steps = steps,
       exemplar = exemplar, n_examples = n_ex, note = note)
}

#' Data-driven archetype view: cluster models on a Gower distance
#'
#' Complementary to [assign_archetype()]: an unsupervised cross-check that can
#' surface sub-structure and outliers. Uses a base-R Gower distance over the
#' categorical + numeric structural features and hierarchical clustering; no
#' extra package dependency.
#'
#' @param ft A feature table from [build_feature_table()].
#' @param k Number of clusters to cut the tree into.
#' @return `ft` with an added integer `cluster` column.
#' @export
cluster_features <- function(ft, k = 12L) {
  num <- c("pk_compartments", "n_ode", "n_eta", "n_endpoint", "n_cov")
  cat_ <- c("lin_cmt", "absorption_type", "elimination_type", "pd_type",
            "residual_error_type")
  num <- intersect(num, names(ft)); cat_ <- intersect(cat_, names(ft))
  d <- .gowerDist(ft, num, cat_)
  hc <- stats::hclust(stats::as.dist(d), method = "ward.D2")
  ft$cluster <- stats::cutree(hc, k = min(k, nrow(ft) - 1L))
  ft
}

# Base-R Gower distance: mean over features of per-feature dissimilarity
# (normalised |x-y| for numerics, 0/1 mismatch for categoricals); NA-tolerant.
.gowerDist <- function(ft, num, cat_) {
  n <- nrow(ft)
  contrib <- vector("list", length(num) + length(cat_))
  wsum <- matrix(0, n, n)
  dsum <- matrix(0, n, n)
  add <- function(dij, present) { dsum <<- dsum + ifelse(present, dij, 0); wsum <<- wsum + present }
  for (v in num) {
    x <- suppressWarnings(as.numeric(ft[[v]]))
    rng <- diff(range(x, na.rm = TRUE)); if (!is.finite(rng) || rng == 0) rng <- 1
    dij <- abs(outer(x, x, "-")) / rng
    present <- outer(!is.na(x), !is.na(x), "&") * 1
    dij[is.na(dij)] <- 0
    add(dij, present)
  }
  for (v in cat_) {
    x <- as.character(ft[[v]])
    dij <- outer(x, x, function(a, b) as.numeric(a != b))
    present <- outer(!is.na(x), !is.na(x), "&") * 1
    dij[is.na(dij)] <- 0
    add(dij, present)
  }
  wsum[wsum == 0] <- 1
  dsum / wsum
}
