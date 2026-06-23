# Decide, per question, whether a sidecar request is auto-answerable

Decide, per question, whether a sidecar request is auto-answerable

## Usage

``` r
sidecar_match(request, policy = sidecar_policy())
```

## Arguments

- request:

  A parsed sidecar request (list with `summary`/`context` and
  `questions`, each with `prompt` and offered `options`).

- policy:

  A policy table from [`sidecar_policy()`](sidecar_policy.md).

## Value

An `nli_sidecar_decision`: per-question decisions plus `auto` (TRUE only
if every question is auto-answerable).
