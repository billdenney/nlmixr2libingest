# Build a similarity store from the nlmixr2lib model library

Assembles a labelled corpus from the model registry (description, label,
parameters, DV) with the Phase-1 archetype as the label and
`fittable = TRUE` (all library models are fittable). The archetype k-NN
over this store gives the archetype prior; adding a negative
(non-fittable) paper set enables the fittable decision (wired in the
runner integration).

## Usage

``` r
build_model_store(ft = NULL, embed = FALSE, quiet = FALSE)
```

## Arguments

- ft:

  A feature table; built via
  [`build_feature_table()`](build_feature_table.md) if `NULL`.

- embed:

  Also store embeddings if a backend is available.

- quiet:

  Suppress progress.

## Value

An `nli_store`.
