# Heuristic fittable score from popPK vocabulary (no store / no LLM)

A standalone fallback for when no labelled corpus is available: counts
popPK modelling terms against off-topic terms. Coarse by design – a
prior, not a verdict.

## Usage

``` r
classify_fittable_keyword(text, min_score = 3L)
```

## Arguments

- text:

  Paper text.

- min_score:

  Net term score at/above which the paper is called fittable.

## Value

A list: `fittable`, `score`, `pos_hits`, `neg_hits`.
