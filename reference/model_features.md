# Structural features for a single nlmixr2lib model

Structural features for a single nlmixr2lib model

## Usage

``` r
model_features(name, registry = NULL)
```

## Arguments

- name:

  Model name (as in `nlmixr2lib::modeldb$name`).

- registry:

  Optional pre-fetched
  [`nlmixr2lib::modeldb`](https://nlmixr2.github.io/nlmixr2lib/reference/modeldb.html)
  (passed by [`build_feature_table()`](build_feature_table.md) to avoid
  repeated lookups).

## Value

A one-row data frame of features. On a parse failure the structural
columns are `NA`, `parse_ok` is `FALSE`, and `atypical` is `TRUE` (the
model is flagged, never dropped).
