# Similarity store for the model/paper corpus used by the classifier.
#
# Holds a labelled corpus (id, archetype label, fittable flag) with two parallel
# representations: a base-R TF-IDF matrix (always; no LLM needed) and, when an
# embedding backend is available, an embedding matrix. Both are L2-normalised so
# cosine similarity is a dot product. The corpus is small (~1-2k rows) so a
# brute-force in-R matrix multiply is more than fast enough; the store persists
# as an RDS rather than DuckDB (the register FTS is where DuckDB earns its keep).

# --- TF-IDF (base R) -----------------------------------------------------------

.tokenize <- function(text) {
  toks <- unlist(strsplit(tolower(as.character(text)), "[^a-z0-9]+"))
  toks[nchar(toks) >= 3L]
}

.tfidfFit <- function(toklist, max_features = 4000L, min_df = 2L) {
  docfreq <- table(unlist(lapply(toklist, unique)))
  docfreq <- docfreq[docfreq >= min_df]
  docfreq <- sort(docfreq, decreasing = TRUE)
  vocab <- names(docfreq)[seq_len(min(max_features, length(docfreq)))]
  list(vocab = vocab,
       idf = as.numeric(log(length(toklist) / docfreq[vocab])))
}

.tfidfTransform <- function(toklist, model) {
  vocab <- model$vocab
  vi <- stats::setNames(seq_along(vocab), vocab)
  m <- matrix(0, length(toklist), length(vocab))
  for (i in seq_along(toklist)) {
    tt <- table(toklist[[i]])
    idx <- vi[names(tt)]
    keep <- !is.na(idx)
    if (any(keep)) m[i, idx[keep]] <- as.numeric(tt[keep])
  }
  .l2norm(sweep(m, 2L, model$idf, "*"))
}

# --- store ---------------------------------------------------------------------

#' Build a similarity store from a labelled corpus
#'
#' @param df A data frame with a text column, an archetype/label column, and a
#'   logical `fittable` column.
#' @param text_col,label_col,fittable_col,id_col Column names.
#' @param embed If `TRUE` and an embedding backend is available, also store
#'   embeddings (falls back to TF-IDF-only otherwise).
#' @return An `nli_store` object.
#' @export
build_store <- function(df, text_col = "text", label_col = "archetype",
                        fittable_col = "fittable", id_col = "model",
                        embed = FALSE) {
  stopifnot(text_col %in% names(df))
  texts <- as.character(df[[text_col]])
  toklist <- lapply(texts, .tokenize)
  tfidf <- .tfidfFit(toklist)
  emb <- NULL
  if (isTRUE(embed)) {
    if (embeddings_available()) {
      emb <- tryCatch(.l2norm(embed_text(texts, quiet = TRUE)),
                      error = function(e) { cli::cli_warn("Embedding failed; TF-IDF only ({conditionMessage(e)})."); NULL })
    } else {
      cli::cli_warn("No embedding backend available; building a TF-IDF-only store.")
    }
  }
  structure(list(
    id = if (id_col %in% names(df)) as.character(df[[id_col]]) else as.character(seq_len(nrow(df))),
    label = if (label_col %in% names(df)) as.character(df[[label_col]]) else rep(NA_character_, nrow(df)),
    fittable = if (fittable_col %in% names(df)) as.logical(df[[fittable_col]]) else rep(NA, nrow(df)),
    text = texts,
    tfidf = tfidf,
    tfidf_matrix = .tfidfTransform(toklist, tfidf),
    emb = emb,
    n = nrow(df)
  ), class = "nli_store")
}

#' @export
print.nli_store <- function(x, ...) {
  cat(sprintf("<nli_store> %d docs | %d TF-IDF terms | embeddings: %s\n",
              x$n, length(x$tfidf$vocab),
              if (is.null(x$emb)) "no" else paste0(ncol(x$emb), "-dim")))
  cat(sprintf("  archetypes: %d | fittable: %d / %d\n",
              length(unique(stats::na.omit(x$label))),
              sum(x$fittable, na.rm = TRUE), x$n))
  invisible(x)
}

#' Save / load a similarity store
#'
#' @param store An `nli_store`.
#' @param path File path.
#' @export
save_store <- function(store, path) { saveRDS(store, path); invisible(path) }
#' @rdname save_store
#' @export
load_store <- function(path) readRDS(path)

# Similarity of a query string to every stored doc. Uses embeddings when the
# store has them AND a backend is reachable; otherwise TF-IDF. Returns a numeric
# vector of cosine similarities plus the mode used (as an attribute).
.storeSim <- function(store, text) {
  if (!is.null(store$emb) && embeddings_available()) {
    qv <- tryCatch(.l2norm(embed_text(text, quiet = TRUE)), error = function(e) NULL)
    if (!is.null(qv) && !anyNA(qv)) {
      sim <- as.numeric(store$emb %*% t(qv))
      attr(sim, "mode") <- "embedding"
      return(sim)
    }
  }
  qt <- .tfidfTransform(list(.tokenize(text)), store$tfidf)
  sim <- as.numeric(store$tfidf_matrix %*% t(qt))
  attr(sim, "mode") <- "keyword"
  sim
}

#' Nearest neighbours of a query string in a store
#'
#' @param store An `nli_store`.
#' @param text Query string.
#' @param k Number of neighbours to return.
#' @return A data frame: `id`, `label`, `fittable`, `sim`, ordered by similarity.
#' @export
nearest <- function(store, text, k = 15L) {
  sim <- .storeSim(store, text)
  ord <- order(sim, decreasing = TRUE)[seq_len(min(k, store$n))]
  out <- data.frame(id = store$id[ord], label = store$label[ord],
                    fittable = store$fittable[ord], sim = sim[ord],
                    stringsAsFactors = FALSE)
  attr(out, "mode") <- attr(sim, "mode")
  out
}
