# Sidecar policy auto-responder.
#
# A "sidecar" is the runner's stop-and-ask protocol: when a dispatched extraction
# agent cannot proceed alone it writes a request (summary + questions + offered
# options) and waits. A human normally answers. Each such pause that resolves to
# a RULE-LIKE answer the agent could have acted on alone is a cold retry waiting
# to happen -- the run stops, the cache goes cold, and a later attempt re-reads
# everything. This module auto-answers exactly those cases from a vetted policy
# table, on the next tick, so the run continues on a warm cache.
#
# SAFETY MODEL (deliberately narrow):
#   1. The responder can only ever select an option the sidecar ALREADY OFFERED
#      the agent -- it never invents an answer. Worst case is picking a wrong
#      offered option, which is bounded and audited.
#   2. A policy fires only when (a) one of its keywords appears in the request,
#      AND (b) an offered option resolves to the policy's canonical answer.
#   3. Exactly one policy must fire; zero or conflicting matches -> escalate.
#   4. Everything scientific / judgement-dependent (units, value selection,
#      covariate naming, model structure, missing PDFs, ...) is in escalate_types
#      and is NEVER auto-answered. A wrong auto-answer would poison an extraction.
# Every auto-answer is written to an audit log.

.scOr <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 1L && is.character(a) && !nzchar(a)) return(b)
  if (length(a) == 1L && is.na(a)) return(b)
  a
}

#' Load the sidecar auto-answer policy table
#'
#' @param path Policy YAML path; defaults to the table shipped with the package.
#' @return The parsed policy list (`policies`, `escalate_types`).
#' @export
sidecar_policy <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("policy", "sidecar-policy.yaml",
                        package = "nlmixr2libingest")
  }
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort("Sidecar policy file not found: {.path {path}}")
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg yaml} is required to read the sidecar policy.")
  }
  yaml::read_yaml(path)
}

.scOptions <- function(q) {
  o <- q$options
  if (is.null(o)) list() else o
}
.scPrompt <- function(q) .scOr(q$prompt, .scOr(q$question, .scOr(q$text, "")))
.scQId <- function(q, i) .scOr(q$id, paste0("q", i))

# All text a keyword could legitimately match against, lower-cased.
.scText <- function(request, q) {
  opts <- .scOptions(q)
  labs <- vapply(opts, function(o)
    paste(c(.scOr(o$label, ""), .scOr(o$description, "")), collapse = " "),
    character(1L))
  tolower(paste(c(.scOr(request$summary, ""), .scOr(request$context, ""),
                  .scOr(request$trigger, ""), .scPrompt(q), labs),
                collapse = " "))
}

# Resolve a policy's canonical answer_text to one of the OFFERED options, by
# key-word overlap. Returns NULL unless a clear majority of the answer's key
# words appear in an option's label -- so we never answer with an option the
# agent was not given.
.scResolveOption <- function(q, answer_text) {
  opts <- .scOptions(q)
  if (!length(opts)) return(NULL)
  aw <- strsplit(tolower(trimws(answer_text)), "\\s+")[[1L]]
  aw <- unique(aw[nchar(aw) >= 4L])
  if (!length(aw)) return(NULL)
  score <- vapply(opts, function(o) {
    lab <- tolower(paste(c(.scOr(o$label, ""), .scOr(o$description, "")),
                         collapse = " "))
    mean(vapply(aw, function(w) grepl(w, lab, fixed = TRUE), logical(1L)))
  }, numeric(1L))
  if (max(score) < 0.5) return(NULL)
  best <- which.max(score)
  list(value = .scOr(opts[[best]]$value, opts[[best]]$label),
       label = .scOr(opts[[best]]$label, opts[[best]]$value))
}

#' Decide, per question, whether a sidecar request is auto-answerable
#'
#' @param request A parsed sidecar request (list with `summary`/`context` and
#'   `questions`, each with `prompt` and offered `options`).
#' @param policy A policy table from [sidecar_policy()].
#' @return An `nli_sidecar_decision`: per-question decisions plus `auto` (TRUE
#'   only if every question is auto-answerable).
#' @export
sidecar_match <- function(request, policy = sidecar_policy()) {
  qs <- request$questions
  if (is.null(qs)) qs <- list()
  pols <- policy$policies
  decisions <- lapply(seq_along(qs), function(i) {
    q <- qs[[i]]
    txt <- .scText(request, q)
    fired <- list()
    for (p in pols) {
      kw <- tolower(unlist(p$match_keywords))
      if (!length(kw) ||
          !any(vapply(kw, function(k) grepl(k, txt, fixed = TRUE), logical(1L)))) {
        next
      }
      opt <- .scResolveOption(q, .scOr(p$answer_text, ""))
      if (is.null(opt)) next
      fired[[length(fired) + 1L]] <- list(
        policy_id = p$id, confidence = .scOr(p$confidence, "medium"),
        value = opt$value, label = opt$label)
    }
    if (length(fired) == 1L) {
      f <- fired[[1L]]
      list(id = .scQId(q, i), decision = "auto", value = f$value,
           label = f$label, policy_id = f$policy_id,
           confidence = f$confidence,
           reason = paste0("matched policy '", f$policy_id, "'"))
    } else {
      reason <- if (!length(fired)) "no policy matched" else
        paste0(length(fired), " policies conflicted: ",
               paste(vapply(fired, `[[`, character(1L), "policy_id"),
                     collapse = ", "))
      list(id = .scQId(q, i), decision = "escalate", value = NA_character_,
           label = NA_character_, policy_id = NA_character_,
           confidence = NA_character_, reason = reason)
    }
  })
  auto <- length(decisions) > 0L &&
    all(vapply(decisions, function(d) d$decision == "auto", logical(1L)))
  structure(list(questions = decisions, auto = auto, n = length(decisions)),
            class = "nli_sidecar_decision")
}

.scAudit <- function(log_file, dec, pids) {
  rec <- list(time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
              request = dec$request_file, policies = as.list(pids),
              answers = lapply(dec$questions, function(d)
                list(id = d$id, value = d$value, policy = d$policy_id)))
  cat(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null"), "\n",
      file = log_file, append = TRUE, sep = "")
}

#' Auto-answer a sidecar request when policy permits
#'
#' Matches a request against the policy table; if EVERY question is
#' auto-answerable, optionally writes the runner's response file (selecting only
#' options the agent was offered) and an audit-log line. Otherwise it writes
#' nothing and the request escalates to a human.
#'
#' @param request A request file path, or a parsed request list.
#' @param response_file Where to write the response JSON when `apply = TRUE`.
#' @param policy A policy table from [sidecar_policy()].
#' @param apply Actually write the response/log (default `FALSE` = dry run).
#' @param log_file Append an audit record here when an answer is written.
#' @return The `nli_sidecar_decision` (with `request_file` and `applied`).
#' @export
sidecar_respond <- function(request, response_file = NULL,
                            policy = sidecar_policy(), apply = FALSE,
                            log_file = NULL) {
  req <- if (is.character(request) && length(request) == 1L &&
             file.exists(request)) {
    jsonlite::fromJSON(request, simplifyVector = FALSE)
  } else request
  dec <- sidecar_match(req, policy)
  dec$request_file <- if (is.character(request) && length(request) == 1L)
    request else NA_character_
  dec$applied <- FALSE
  if (dec$auto && apply) {
    answers <- lapply(dec$questions, function(d) list(id = d$id, value = d$value))
    pids <- unique(vapply(dec$questions, `[[`, character(1L), "policy_id"))
    payload <- list(answers = answers,
                    notes = paste0("auto-answered by nlmixr2libingest sidecar ",
                                   "policy: ", paste(pids, collapse = ", ")))
    if (!is.null(response_file)) {
      jsonlite::write_json(payload, response_file, auto_unbox = TRUE,
                           pretty = TRUE)
    }
    if (!is.null(log_file)) .scAudit(log_file, dec, pids)
    dec$applied <- TRUE
  }
  dec
}

#' @export
print.nli_sidecar_decision <- function(x, ...) {
  if (isTRUE(x$auto)) {
    cli::cli_alert_success("auto-answerable ({x$n} question{?s})")
  } else {
    cli::cli_alert_warning("escalate to human ({x$n} question{?s})")
  }
  for (d in x$questions) {
    if (d$decision == "auto") {
      cli::cli_text("  {.field {d$id}} -> {.val {d$label}}  [{d$policy_id}, {d$confidence}]")
    } else {
      cli::cli_text("  {.field {d$id}} -> escalate ({d$reason})")
    }
  }
  invisible(x)
}
