# Complexity tier for a model from its structural features

Complexity tier for a model from its structural features

## Usage

``` r
complexity_tier(
  n_ode = NA,
  n_cov = 0,
  n_endpoint = 1,
  pd_type = "none",
  elimination_type = "linear",
  atypical = FALSE
)
```

## Arguments

- n_ode, n_cov, n_endpoint:

  Counts (NA tolerated for `n_ode`).

- pd_type, elimination_type:

  Feature strings (e.g. from [`model_features()`](model_features.md)).

- atypical:

  Logical; high-complexity / PBPK / parse-failure flag.

## Value

`"low"`, `"medium"`, or `"high"`.
