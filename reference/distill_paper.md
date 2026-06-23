# Distill a structured extraction sheet from a paper (LLM-optional)

Distill a structured extraction sheet from a paper (LLM-optional)

## Usage

``` r
distill_paper(
  text,
  model = .distillModel(),
  backend = .llmBackend(),
  max_chars = 12000L
)
```

## Arguments

- text:

  Paper text, or a path to a (trimmed) paper file.

- model:

  Chat model name (default `llama3.1` via option
  `nlmixr2libingest.chat_model`).

- backend:

  `"ollama"`/`"none"`/`"auto"`.

- max_chars:

  Truncate the paper to this many characters for the prompt.

## Value

An `nli_distill` list (parsed sheet; the raw JSON is in attribute
`raw`), or `NULL` when no backend is available / extraction fails.
