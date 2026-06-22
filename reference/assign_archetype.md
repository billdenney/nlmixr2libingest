# Assign a structural archetype label to each model

Deterministic rule-based labels derived from the feature table.
Unrecognised or high-complexity models receive an `other:*` label
(flagged, not dropped).

## Usage

``` r
assign_archetype(ft)
```

## Arguments

- ft:

  A feature table from [`build_feature_table()`](build_feature_table.md)
  / [`model_features()`](model_features.md).

## Value

A character vector of archetype labels, one per row of `ft`.
