# Deterministic structural feature extraction for nlmixr2lib models.
#
# Two sources, both consumed (never reimplemented):
#   1. nlmixr2lib::modeldb  -- the curated registry: name/category/linCmt/
#      algebraic/parameters/DV/dosing/vignette/description (cheap, reliable).
#   2. the parsed rxUi (rxode2::rxode2(readModelDb(name))) -- compartments, IIV
#      structure, residual-error type, and the covariates actually used.
#
# Output is the substrate for archetype clustering (R/archetypes.R) and the
# fittable classifier (later phases). It does NO validation -- that stays in
# nlmixr2lib::checkModelConventions(). The API is kept small and relocatable
# (it may later move to nlmixr2lib for model reporting).

# Feature extraction needs nlmixr2lib (the model registry) and rxode2 (parsing).
# These are Suggests so the package's log-filter / register-lookup features still
# install without them; fail clearly if a caller asks for features regardless.
.requireModelPkgs <- function() {
  for (p in c("nlmixr2lib", "rxode2")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg {p}} is required for structural feature extraction.",
        i = "Install it, or use the log-filter / register-lookup tools, which do not need it."))
    }
  }
}

# Comma-joined registry string -> character vector (NA/empty-safe).
.splitReg <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(x)) return(character())
  trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
}

.hasAny <- function(x, pattern) length(x) > 0L && any(grepl(pattern, x, ignore.case = TRUE, perl = TRUE))

# Deparse a model's rxUi model-block expressions into one searchable string.
.modelText <- function(ui) {
  ex <- tryCatch(ui$lstExpr, error = function(e) NULL)
  if (is.null(ex)) return("")
  paste(vapply(ex, function(e) paste(deparse(e), collapse = " "), character(1L)), collapse = " ; ")
}

# Count d/dt() ODE states in the model text.
.countOde <- function(txt) {
  if (!nzchar(txt)) return(0L)
  length(attr(gregexpr("d/dt\\(", txt)[[1L]], "match.length")[gregexpr("d/dt\\(", txt)[[1L]] > 0L])
}

# --- categorical detectors (operate on a context list) -------------------------
# ctx fields: params, dv, dosing, states, txt, desc, lin_cmt, n_ode

.absorptionType <- function(ctx) {
  if (.hasAny(ctx$params, "^k?tr[0-9]*$|^mtt$|^ntr|^n_?tr") ||
      grepl("transit\\(|\\bktr\\b|\\bmtt\\b", ctx$txt, ignore.case = TRUE)) return("transit")
  has_depot <- .hasAny(ctx$dosing, "^depot") || .hasAny(ctx$states, "^depot")
  has_ka <- .hasAny(ctx$params, "^l?ka([0-9]|$)|^l?kabs")
  zero_ord <- .hasAny(ctx$params, "^l?dur|^d2$|^l?rate$|^l?tk0|^l?d1$") ||
    grepl("zero.?order", ctx$desc, ignore.case = TRUE)
  if (zero_ord && (has_depot || has_ka)) return("zero_order")
  if (has_depot || has_ka) return("first_order")
  if (length(ctx$dosing) && all(grepl("^central$", ctx$dosing, ignore.case = TRUE))) return("iv")
  if (!has_depot && !has_ka) return("iv")
  "unknown"
}

.eliminationType <- function(ctx) {
  if (.hasAny(ctx$states, "^target|^complex|^tmdd|^rec$|^drug_target") ||
      .hasAny(ctx$params, "^l?kon$|^l?koff$|^l?kint$|^l?ksyn$|^l?kdeg$|^l?kss$|^l?kdeg") ||
      grepl("tmdd|target.?mediated", paste(ctx$txt, ctx$desc), ignore.case = TRUE)) return("tmdd")
  if (.hasAny(ctx$params, "^l?vm(ax)?$|^l?km$|^l?vmax") ||
      grepl("\\bvmax\\b|michaelis|\\bmenten\\b", paste(ctx$txt, ctx$desc), ignore.case = TRUE)) return("michaelis_menten")
  if (.hasAny(ctx$params, "^l?cl([0-9]|$)|^l?clr$|^l?cl_|^l?kel$|^l?ke$")) return("linear")
  # No disposition parameters at all -> a pure PD / turnover / endogenous model.
  if (!.hasAny(ctx$params, "^l?vc?$|^l?v[0-9]$|^l?cl|^l?q[0-9]*$|^l?kel?$")) return("none")
  "unknown"
}

.pdType <- function(ctx) {
  if (grepl("\\btte\\b|hazard|survival|gompertz|weibull|d/dt\\(\\s*surv", ctx$txt, ignore.case = TRUE) ||
      .hasAny(ctx$dv, "surv|tte|hazard")) return("tte")
  if (grepl("d/dt\\(\\s*(tumou?r|damaged|cells?)\\b", ctx$txt, ignore.case = TRUE) ||
      .hasAny(ctx$params, "^l?kg$|^l?kgrow|^l?kkill|^l?kg(lin|exp)$") ||
      .hasAny(ctx$dv, "tumou?r|psa")) return("tumor_growth")
  if (.hasAny(ctx$params, "^l?ke0$|^l?keo$") ||
      grepl("d/dt\\(\\s*(ce|effect[0-9]*|biophase)\\s*\\)", ctx$txt, ignore.case = TRUE)) return("effect_compartment")
  if (.hasAny(ctx$params, "^l?kin$|^l?kout$") || grepl("\\bkin\\b.*\\bkout\\b", ctx$txt, ignore.case = TRUE)) return("indirect_response")
  if (.hasAny(ctx$params, "^l?emax$|^l?imax$|^l?ec50$|^l?ic50$|^l?e0$")) return("direct_emax")
  if (grepl("turnover|precursor", paste(ctx$txt, ctx$desc), ignore.case = TRUE)) return("turnover")
  "none"
}

# Residual-error type across endpoints, from rxUi predDf$errType.
.residualType <- function(ui) {
  pd <- tryCatch(ui$predDf, error = function(e) NULL)
  if (is.null(pd) || !nrow(pd)) return("none")
  one <- function(e) {
    e <- tolower(e)
    addp <- grepl("add", e); prop <- grepl("prop", e)
    if (grepl("lnorm|logn", e)) "lnorm"
    else if (addp && prop) "add_prop"
    else if (prop) "prop"
    else if (addp) "add"
    else if (nzchar(e)) e
    else "other"
  }
  types <- unique(vapply(pd$errType, one, character(1L)))
  if (length(types) > 1L) "mixed" else types
}

# Number of PK disposition compartments from peripheral-volume params (works for
# both linCmt and ODE models): 1 (central) + #peripherals. NA if no clear vc.
.pkCompartments <- function(params) {
  if (!.hasAny(params, "^l?vc$|^l?v$|^l?v1$|^l?vc[0-9]")) return(NA_integer_)
  nper <- sum(grepl("^l?vp[0-9]*$|^l?v[2-9]$", params, ignore.case = TRUE))
  1L + nper
}

# rxUi-derived structural features (list).
.uiFeatures <- function(ui) {
  ini <- ui$iniDf
  eta <- !is.na(ini$neta1)
  txt <- .modelText(ui)
  list(
    states = ui$state,
    n_state = length(ui$state),
    n_ode = .countOde(txt),
    n_theta = sum(!is.na(ini$ntheta)),
    n_eta = sum(eta & ini$neta1 == ini$neta2),
    n_eta_corr = sum(eta & ini$neta1 != ini$neta2),
    n_endpoint = {
      pd <- tryCatch(ui$predDf, error = function(e) NULL)
      if (is.null(pd)) 0L else nrow(pd)
    },
    residual_error_type = .residualType(ui),
    covariates = {
      cv <- tryCatch(ui$allCovs, error = function(e) character())
      if (is.null(cv)) character() else cv
    },
    txt = txt
  )
}

#' Structural features for a single nlmixr2lib model
#'
#' @param name Model name (as in `nlmixr2lib::modeldb$name`).
#' @param registry Optional pre-fetched `nlmixr2lib::modeldb` (passed by
#'   [build_feature_table()] to avoid repeated lookups).
#' @return A one-row data frame of features. On a parse failure the structural
#'   columns are `NA`, `parse_ok` is `FALSE`, and `atypical` is `TRUE` (the model
#'   is flagged, never dropped).
#' @export
model_features <- function(name, registry = NULL) {
  if (is.null(registry)) { .requireModelPkgs(); registry <- nlmixr2lib::modeldb }
  row <- registry[match(name, registry$name), , drop = FALSE]
  params <- .splitReg(row$parameters)
  dv <- .splitReg(row$DV)
  dosing <- .splitReg(row$dosing)
  desc <- if (length(row$description) && !is.na(row$description)) row$description else ""
  lin_cmt <- isTRUE(row$linCmt)

  ui <- tryCatch(rxode2::rxode2(nlmixr2lib::readModelDb(name)), error = function(e) e)
  parse_ok <- !inherits(ui, "error")

  if (parse_ok) {
    uf <- .uiFeatures(ui)
    states <- uf$states; covs <- uf$covariates; txt <- uf$txt
    n_state <- uf$n_state; n_ode <- uf$n_ode
    n_theta <- uf$n_theta; n_eta <- uf$n_eta; n_eta_corr <- uf$n_eta_corr
    n_endpoint <- uf$n_endpoint; resid <- uf$residual_error_type
  } else {
    states <- character(); covs <- character(); txt <- ""
    n_state <- n_ode <- n_theta <- n_eta <- n_eta_corr <- n_endpoint <- NA_integer_
    resid <- NA_character_
  }

  ctx <- list(params = params, dv = dv, dosing = dosing, states = states,
              txt = txt, desc = desc, lin_cmt = lin_cmt, n_ode = n_ode)
  absorption <- if (parse_ok || length(params)) .absorptionType(ctx) else NA_character_
  elimination <- if (parse_ok || length(params)) .eliminationType(ctx) else NA_character_
  pd <- if (parse_ok || length(params)) .pdType(ctx) else NA_character_
  pk_cmt <- .pkCompartments(params)

  # Reserve the atypical flag for parse failures and high-complexity systems
  # (PBPK / QSP / mechanistic); the archetype clusterer surfaces other oddballs.
  atypical <- !parse_ok || (!is.na(n_ode) && n_ode > 6L)

  data.frame(
    model = name,
    category = if (length(row$category)) as.character(row$category) else NA_character_,
    parse_ok = parse_ok,
    lin_cmt = lin_cmt,
    algebraic = isTRUE(row$algebraic),
    has_vignette = length(row$vignette) > 0L && !is.na(row$vignette) && nzchar(row$vignette),
    n_param = length(params),
    pk_compartments = pk_cmt,
    n_state = n_state,
    n_ode = n_ode,
    n_theta = n_theta,
    n_eta = n_eta,
    n_eta_corr = n_eta_corr,
    has_corr_iiv = if (is.na(n_eta_corr)) NA else n_eta_corr > 0L,
    n_endpoint = n_endpoint,
    residual_error_type = resid,
    absorption_type = absorption,
    elimination_type = elimination,
    pd_type = pd,
    has_pd = if (is.na(pd)) NA else pd != "none",
    n_cov = length(covs),
    cov_ids = paste(covs, collapse = ";"),
    atypical = atypical,
    error = if (parse_ok) NA_character_ else conditionMessage(ui),
    stringsAsFactors = FALSE
  )
}

#' Build the structural feature table for the nlmixr2lib model library
#'
#' Parses every model (or a given subset) into the feature table consumed by
#' archetype clustering. Models that fail to parse are kept as `atypical` rows
#' with `parse_ok = FALSE` rather than dropped.
#'
#' @param names Model names; defaults to all of `nlmixr2lib::modeldb$name`.
#' @param quiet Suppress the progress bar.
#' @return A data frame, one row per model (see [model_features()]).
#' @export
build_feature_table <- function(names = NULL, quiet = FALSE) {
  .requireModelPkgs()
  registry <- nlmixr2lib::modeldb
  if (is.null(names)) names <- registry$name
  n <- length(names)
  if (!quiet) cli::cli_progress_bar("Extracting features", total = n)
  rows <- vector("list", n)
  for (i in seq_len(n)) {
    rows[[i]] <- tryCatch(
      model_features(names[[i]], registry = registry),
      error = function(e) data.frame(model = names[[i]], parse_ok = FALSE,
                                     atypical = TRUE, error = conditionMessage(e),
                                     stringsAsFactors = FALSE))
    if (!quiet) cli::cli_progress_update()
  }
  if (!quiet) cli::cli_progress_done()
  # Fill-bind: rows from the error fallback have fewer columns.
  cols <- names(rows[[which.max(vapply(rows, ncol, 1L))]])
  do.call(rbind, lapply(rows, function(r) {
    miss <- setdiff(cols, names(r))
    for (m in miss) r[[m]] <- if (m %in% c("model","error","residual_error_type","cov_ids",
                                           "absorption_type","elimination_type","pd_type","category")) NA_character_ else NA
    r[, cols, drop = FALSE]
  }))
}
