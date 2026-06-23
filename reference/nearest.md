# Nearest neighbours of a query string in a store

Nearest neighbours of a query string in a store

## Usage

``` r
nearest(store, text, k = 15L)
```

## Arguments

- store:

  An `nli_store`.

- text:

  Query string.

- k:

  Number of neighbours to return.

## Value

A data frame: `id`, `label`, `fittable`, `sim`, ordered by similarity.
