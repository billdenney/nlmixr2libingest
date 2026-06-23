# Build a similarity store from a labelled corpus

Build a similarity store from a labelled corpus

## Usage

``` r
build_store(
  df,
  text_col = "text",
  label_col = "archetype",
  fittable_col = "fittable",
  id_col = "model",
  embed = FALSE
)
```

## Arguments

- df:

  A data frame with a text column, an archetype/label column, and a
  logical `fittable` column.

- text_col, label_col, fittable_col, id_col:

  Column names.

- embed:

  If `TRUE` and an embedding backend is available, also store embeddings
  (falls back to TF-IDF-only otherwise).

## Value

An `nli_store` object.
