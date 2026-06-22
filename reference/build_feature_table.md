# Build the structural feature table for the nlmixr2lib model library

Parses every model (or a given subset) into the feature table consumed
by archetype clustering. Models that fail to parse are kept as
`atypical` rows with `parse_ok = FALSE` rather than dropped.

## Usage

``` r
build_feature_table(names = NULL, quiet = FALSE)
```

## Arguments

- names:

  Model names; defaults to all of `nlmixr2lib::modeldb$name`.

- quiet:

  Suppress the progress bar.

## Value

A data frame, one row per model (see
[`model_features()`](model_features.md)).
