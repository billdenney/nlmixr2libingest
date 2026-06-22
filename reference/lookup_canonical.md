# Look up canonical names in the nlmixr2lib registers

Returns the handful of register entries most relevant to `term` without
ever loading the multi-hundred-thousand-token register files into
context. Exact name and alias matches are surfaced first, then full-text
(BM25) or token matches. The long `example_models` list is summarised to
a count by default.

## Usage

``` r
lookup_canonical(
  term,
  kind = NULL,
  top_k = 5,
  full = FALSE,
  db_path = register_db_path(),
  dir = registers_dir()
)
```

## Arguments

- term:

  Search term: a canonical name, an alias, or a free-text concept (e.g.
  `"body weight"`).

- kind:

  Optionally restrict to `"covariate"`, `"compartment"`, or
  `"parameter"`.

- top_k:

  Maximum rows to return.

- full:

  If `TRUE`, include the full `example_models` list and `text`.

- db_path, dir:

  Index location and register directory.

## Value

A data frame of matching entries (0 rows if none match), ordered by
relevance.
