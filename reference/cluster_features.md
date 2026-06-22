# Data-driven archetype view: cluster models on a Gower distance

Complementary to [`assign_archetype()`](assign_archetype.md): an
unsupervised cross-check that can surface sub-structure and outliers.
Uses a base-R Gower distance over the categorical + numeric structural
features and hierarchical clustering; no extra package dependency.

## Usage

``` r
cluster_features(ft, k = 12L)
```

## Arguments

- ft:

  A feature table from
  [`build_feature_table()`](build_feature_table.md).

- k:

  Number of clusters to cut the tree into.

## Value

`ft` with an added integer `cluster` column.
