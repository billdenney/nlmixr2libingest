# Classify a paper from its text against a labelled store

Classify a paper from its text against a labelled store

## Usage

``` r
classify(store, text, k = 15L, fittable_threshold = 0.5)
```

## Arguments

- store:

  An `nli_store` from [`build_store()`](build_store.md) /
  [`build_model_store()`](build_model_store.md).

- text:

  The paper text (e.g. a trimmed abstract+methods).

- k:

  Neighbours to poll.

- fittable_threshold:

  Similarity-weighted fittable fraction above which the paper is called
  fittable.

## Value

A list: `fittable` (logical/NA), `fittable_score`, `archetypes` (top-3
similarity-weighted votes), `mode` (`"embedding"`/`"keyword"`),
`neighbors`.
