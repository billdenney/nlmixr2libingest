# Leave-one-out archetype accuracy of a store (k-NN cross-validation)

Each document's archetype is predicted from its nearest neighbours
(excluding itself); accuracy is the fraction correct. Validates the
similarity + voting pipeline on real labels before any reliance on its
predictions.

## Usage

``` r
evaluate_archetype_cv(store, k = 15L)
```

## Arguments

- store:

  An `nli_store` (must have `label`).

- k:

  Neighbours.

## Value

A list: `accuracy`, `n`, `k`, `per_class` (accuracy by archetype).
