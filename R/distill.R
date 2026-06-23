# LLM-optional distillation: a structured extraction "sheet" from a paper.
#
# Runs the local chat model (llama3.1 via ollama) with forced-JSON output to
# pre-extract the model's parameters/units/RSE/IIV/covariate-equations/error/
# dosing. The agent BUILDS FROM and VERIFIES AGAINST this sheet -- it is an
# advisory accelerator, never authoritative (small local models can be wrong;
# the source-trace firewall catches it). With no LLM backend, returns NULL and
# the agent reads the trimmed paper directly.

.distillModel <- function() {
  getOption("nlmixr2libingest.chat_model", "llama3.1:8b-instruct-q4_K_M")
}

.distillSystemPrompt <- function() {
  paste(
    "You extract a structured summary of a population PK/PD model from a paper.",
    "Output ONLY a JSON object with these keys:",
    "structure (object: compartments, absorption, elimination, pd_type),",
    "parameters (array of {name, value, unit, rse}),",
    "iiv (array of {parameter, value, unit}),",
    "covariate_effects (array of {covariate, parameter, relationship}),",
    "residual_error (array of {endpoint, type, value}),",
    "dosing (string).",
    "Use null or empty arrays when a field is not reported.",
    "Copy values verbatim from the paper; NEVER invent or infer numbers.")
}

# One ollama /api/chat call with format=json; returns the JSON content string.
.ollamaChatJSON <- function(host, model, system, prompt, timeout = 300) {
  tryCatch({
    r <- httr2::req_perform(httr2::req_timeout(httr2::req_body_json(
      httr2::request(paste0(host, "/api/chat")),
      list(model = model, stream = FALSE, format = "json",
           options = list(temperature = 0),
           messages = list(
             list(role = "system", content = system),
             list(role = "user", content = prompt)))), timeout))
    httr2::resp_body_json(r)$message$content
  }, error = function(e) NULL)
}

#' Distill a structured extraction sheet from a paper (LLM-optional)
#'
#' @param text Paper text, or a path to a (trimmed) paper file.
#' @param model Chat model name (default `llama3.1` via option
#'   `nlmixr2libingest.chat_model`).
#' @param backend `"ollama"`/`"none"`/`"auto"`.
#' @param max_chars Truncate the paper to this many characters for the prompt.
#' @return An `nli_distill` list (parsed sheet; the raw JSON is in attribute
#'   `raw`), or `NULL` when no backend is available / extraction fails.
#' @export
distill_paper <- function(text, model = .distillModel(), backend = .llmBackend(),
                          max_chars = 12000L) {
  if (identical(backend, "none")) return(NULL)
  if (!requireNamespace("httr2", quietly = TRUE)) return(NULL)
  if (!embeddings_available()) {            # same server ping as the embed path
    cli::cli_warn("No local LLM reachable; skipping distillation (agent reads the paper directly).")
    return(NULL)
  }
  txt <- if (length(text) == 1L && !grepl("\n", text) && file.exists(text)) {
    paste(readLines(text, warn = FALSE), collapse = " ")
  } else paste(text, collapse = " ")
  txt <- substr(txt, 1L, max_chars)
  js <- .ollamaChatJSON(.ollamaHost(), model, .distillSystemPrompt(),
                        paste0("Paper text:\n", txt))
  if (is.null(js) || !nzchar(js)) return(NULL)
  sheet <- tryCatch(jsonlite::fromJSON(js, simplifyVector = TRUE),
                    error = function(e) NULL)
  if (is.null(sheet)) return(NULL)
  structure(sheet, class = "nli_distill", raw = js)
}

#' @export
print.nli_distill <- function(x, ...) {
  cli::cli_h2("Distillation sheet (advisory -- verify every value against the paper)")
  st <- x$structure
  if (!is.null(st)) {
    cli::cli_text("structure: {paste(unlist(st), collapse = ' / ')}")
  }
  np <- if (is.data.frame(x$parameters)) nrow(x$parameters) else length(x$parameters)
  cli::cli_text("parameters: {np} | iiv: {if (is.data.frame(x$iiv)) nrow(x$iiv) else length(x$iiv)} | covariate_effects: {if (is.data.frame(x$covariate_effects)) nrow(x$covariate_effects) else length(x$covariate_effects)}")
  cli::cli_text("{.emph These are unverified LLM extractions; source_trace() + manual check required.}")
  invisible(x)
}
