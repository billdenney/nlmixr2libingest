# Pluggable local-LLM backend (embeddings).
#
# The package is usable with NO LLM: every deterministic feature (log filtering,
# register lookup, structural features, archetypes, the keyword classifier) runs
# without this. Embeddings are an optional accelerator for the similarity-based
# classifier (R/classify.R). The backend is ollama (the local-llm MCP's Docker
# server) reached over HTTP; selection is via options/env, default "auto".
#
#   options(nlmixr2libingest.llm = "ollama" | "none" | "auto")
#   options(nlmixr2libingest.ollama_url  = "http://localhost:11434")
#   options(nlmixr2libingest.embed_model = "nomic-embed-text")

.llmBackend <- function() {
  getOption("nlmixr2libingest.llm", Sys.getenv("NLMIXR2LIBINGEST_LLM", "auto"))
}
.ollamaHost <- function() {
  getOption("nlmixr2libingest.ollama_url", Sys.getenv("OLLAMA_HOST", "http://localhost:11434"))
}
.embedModel <- function() {
  getOption("nlmixr2libingest.embed_model", "nomic-embed-text")
}

#' Is a local embedding backend available?
#'
#' Returns `TRUE` only if a backend is selected (not `"none"`), `httr2` is
#' installed, and the ollama server answers. Used to choose embedding vs keyword
#' classification automatically.
#'
#' @return Logical scalar.
#' @export
embeddings_available <- function() {
  if (identical(.llmBackend(), "none")) return(FALSE)
  if (!requireNamespace("httr2", quietly = TRUE)) return(FALSE)
  tryCatch({
    resp <- httr2::req_perform(httr2::req_timeout(
      httr2::request(paste0(.ollamaHost(), "/api/tags")), 5))
    httr2::resp_status(resp) == 200L
  }, error = function(e) FALSE)
}

# Embed a single string via ollama; tries the new /api/embed then legacy
# /api/embeddings. Returns a numeric vector, or NULL on failure.
.ollamaEmbedOne <- function(host, model, text, timeout = 120) {
  text <- if (!nzchar(text)) " " else text
  new <- tryCatch({
    r <- httr2::req_perform(httr2::req_timeout(httr2::req_body_json(
      httr2::request(paste0(host, "/api/embed")),
      list(model = model, input = text)), timeout))
    emb <- httr2::resp_body_json(r)$embeddings
    if (length(emb)) as.numeric(emb[[1]]) else NULL
  }, error = function(e) NULL)
  if (!is.null(new) && length(new)) return(new)
  tryCatch({
    r <- httr2::req_perform(httr2::req_timeout(httr2::req_body_json(
      httr2::request(paste0(host, "/api/embeddings")),
      list(model = model, prompt = text)), timeout))
    emb <- httr2::resp_body_json(r)$embedding
    if (length(emb)) as.numeric(emb) else NULL
  }, error = function(e) NULL)
}

#' Embed text with the local embedding model
#'
#' @param texts Character vector of documents to embed.
#' @param model Embedding model name (default `nomic-embed-text`).
#' @param backend `"ollama"`, `"none"`, or `"auto"` (default from options).
#' @param quiet Suppress the progress bar.
#' @return A numeric matrix with one row per input text (embedding dimension
#'   columns). Rows that fail to embed are `NA`.
#' @export
embed_text <- function(texts, model = .embedModel(), backend = .llmBackend(),
                       quiet = FALSE) {
  texts <- as.character(texts)
  if (identical(backend, "none")) {
    cli::cli_abort(c("No embedding backend is configured.",
      i = 'Set {.code options(nlmixr2libingest.llm = "ollama")} and run the local ollama server, or use the keyword classifier.'))
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg httr2} is required to embed text.")
  }
  host <- .ollamaHost()
  n <- length(texts)
  if (!quiet) cli::cli_progress_bar("Embedding", total = n)
  vecs <- vector("list", n)
  dim <- NA_integer_
  for (i in seq_len(n)) {
    v <- .ollamaEmbedOne(host, model, texts[[i]])
    if (!is.null(v)) { vecs[[i]] <- v; if (is.na(dim)) dim <- length(v) }
    if (!quiet) cli::cli_progress_update()
  }
  if (!quiet) cli::cli_progress_done()
  if (is.na(dim)) cli::cli_abort("Embedding failed for all inputs (is the ollama server running?).")
  out <- matrix(NA_real_, nrow = n, ncol = dim)
  for (i in seq_len(n)) if (!is.null(vecs[[i]]) && length(vecs[[i]]) == dim) out[i, ] <- vecs[[i]]
  out
}

# L2-normalise rows of a matrix (so dot product == cosine similarity).
.l2norm <- function(m) {
  nr <- sqrt(rowSums(m * m))
  nr[nr == 0 | is.na(nr)] <- 1
  m / nr
}
