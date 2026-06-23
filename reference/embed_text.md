# Embed text with the local embedding model

Embed text with the local embedding model

## Usage

``` r
embed_text(
  texts,
  model = .embedModel(),
  backend = .llmBackend(),
  batch = 64L,
  quiet = FALSE
)
```

## Arguments

- texts:

  Character vector of documents to embed.

- model:

  Embedding model name (default `nomic-embed-text`).

- backend:

  `"ollama"`, `"none"`, or `"auto"` (default from options).

- batch:

  Texts to embed per request (batched via `/api/embed`); falls back to
  per-item embedding if the batch endpoint is unavailable.

- quiet:

  Suppress the progress bar.

## Value

A numeric matrix with one row per input text (embedding dimension
columns). Rows that fail to embed are `NA`.
