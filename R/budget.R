# Soft, complexity-tiered token-budget backstop + RunRecord analytics.
#
# Graceful escalation by design: a budget overrun never hard-kills. The advisor
# returns an ACTION (continue / checkpoint / checkpoint_and_escalate) that the
# runner/skill acts on (commit WIP, then sidecar-ask the operator). Tiers scale
# the budget to model complexity (a TMDD/PBPK multi-endpoint model legitimately
# costs more than a 1-cmt linear fit), using the Phase-1 structural features.

.budgetDefaults <- function() {
  getOption("nlmixr2libingest.tier_budgets",
            c(low = 40000L, medium = 90000L, high = 180000L))
}

#' Complexity tier for a model from its structural features
#'
#' @param n_ode,n_cov,n_endpoint Counts (NA tolerated for `n_ode`).
#' @param pd_type,elimination_type Feature strings (e.g. from [model_features()]).
#' @param atypical Logical; high-complexity / PBPK / parse-failure flag.
#' @return `"low"`, `"medium"`, or `"high"`.
#' @export
complexity_tier <- function(n_ode = NA, n_cov = 0, n_endpoint = 1,
                            pd_type = "none", elimination_type = "linear",
                            atypical = FALSE) {
  score <- 0L
  if (isTRUE(atypical)) score <- score + 3L
  if (!is.na(n_ode)) score <- score + (n_ode >= 4L) + 2L * (n_ode >= 7L)
  score <- score + (n_cov >= 3L) + (n_cov >= 8L)
  score <- score + (n_endpoint >= 2L)
  if (!is.na(pd_type) && pd_type != "none") score <- score + 1L
  if (elimination_type %in% c("tmdd", "michaelis_menten")) score <- score + 1L
  if (score >= 4L) "high" else if (score >= 2L) "medium" else "low"
}

#' Token budget for a complexity tier
#' @param tier `"low"`/`"medium"`/`"high"`.
#' @param budgets Named tier->tokens vector (default via option
#'   `nlmixr2libingest.tier_budgets`).
#' @return Integer token budget.
#' @export
tier_budget <- function(tier, budgets = .budgetDefaults()) {
  if (!tier %in% names(budgets)) cli::cli_abort("Unknown tier {.val {tier}}.")
  unname(budgets[[tier]])
}

#' Soft budget advisor (never a hard kill)
#'
#' @param spent Output tokens spent so far.
#' @param tier Complexity tier (used if `budget` is `NULL`).
#' @param budget Explicit token budget (overrides `tier`).
#' @param soft Fraction at which to recommend a checkpoint (default 0.8).
#' @return A list: `status` (`ok`/`approaching`/`over`), `action`
#'   (`continue`/`checkpoint`/`checkpoint_and_escalate`), `spent`, `budget`,
#'   `remaining`, `frac`.
#' @export
budget_advisor <- function(spent, tier = NULL, budget = NULL, soft = 0.8) {
  if (is.null(budget)) {
    if (is.null(tier)) cli::cli_abort("Provide either {.arg tier} or {.arg budget}.")
    budget <- tier_budget(tier)
  }
  frac <- spent / budget
  status <- if (frac < soft) "ok" else if (frac < 1) "approaching" else "over"
  action <- switch(status,
    ok = "continue",
    approaching = "checkpoint",
    over = "checkpoint_and_escalate")
  list(status = status, action = action, spent = spent, budget = budget,
       remaining = max(0, budget - spent), frac = round(frac, 3))
}

.numOr0 <- function(x) {
  if (is.null(x) || length(x) != 1L) return(0)
  v <- suppressWarnings(as.numeric(x))
  if (is.finite(v)) v else 0
}

#' Token / cost analytics over the runner's RunRecords
#'
#' Parses the task-runner state YAMLs (one per task, each with a `runs` list
#' carrying `usage` and `cost_usd`) into a tidy per-run data frame, so the
#' measured cost economics (output and cache-read tokens, $/run) can be tracked.
#'
#' @param state_dir Directory of `<task_id>.yaml` state files (e.g. the runner's
#'   `.claude_task_runner/state`).
#' @return A data frame: `task`, `attempt`, `output`, `input`, `cache_read`,
#'   `cost`. Its `summary` attribute holds median/mean/max per numeric column.
#' @export
run_token_stats <- function(state_dir) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg yaml} is required to parse runner state files.")
  }
  files <- list.files(state_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  rows <- list()
  for (f in files) {
    d <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (is.null(d)) next
    runs <- d$runs
    if (is.null(runs)) next
    tid <- if (!is.null(d$id)) d$id else sub("\\.ya?ml$", "", basename(f))
    for (k in seq_along(runs)) {
      r <- runs[[k]]; u <- if (is.null(r$usage)) list() else r$usage
      rows[[length(rows) + 1L]] <- data.frame(
        task = tid, attempt = k,
        output = .numOr0(u$output_tokens),
        input = .numOr0(u$input_tokens),
        cache_read = .numOr0(u$cache_read_input_tokens),
        cost = .numOr0(r$cost_usd),
        stringsAsFactors = FALSE)
    }
  }
  df <- if (length(rows)) do.call(rbind, rows) else
    data.frame(task = character(), attempt = integer(), output = numeric(),
               input = numeric(), cache_read = numeric(), cost = numeric())
  num <- c("output", "input", "cache_read", "cost")
  attr(df, "summary") <- if (nrow(df))
    vapply(df[num], function(x) c(median = stats::median(x), mean = mean(x), max = max(x)),
           numeric(3)) else NULL
  df
}
