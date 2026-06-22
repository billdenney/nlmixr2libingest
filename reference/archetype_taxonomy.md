# Summarise the empirical archetype taxonomy

Summarise the empirical archetype taxonomy

## Usage

``` r
archetype_taxonomy(ft, min_n = 3L)
```

## Arguments

- ft:

  A feature table from
  [`build_feature_table()`](build_feature_table.md).

- min_n:

  Archetypes with fewer than `min_n` members are additionally flagged
  `rare = TRUE` (candidates for review, still listed).

## Value

A data frame: `archetype`, `n`, `pct`, `is_other`, `rare`, and a
representative `example` model, ordered by frequency.
