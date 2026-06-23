# Programmatic source-trace pre-check for a model against its paper

For every estimated `ini()` value, searches the paper text for a
supporting number (back-transform- and rounding-tolerant) and flags
values with none. Also lists structural `model()` equations and
hardcoded constants to verify. This assists – never replaces – the
mandatory manual source-trace.

## Usage

``` r
source_trace(model, paper, tol = 0.05)
```

## Arguments

- model:

  An `rxUi`, a model function, or an `nlmixr2lib` model name.

- paper:

  Paper text, or a path to a (trimmed) paper file.

- tol:

  Relative tolerance for a numeric match (default 0.05 = 5%).

## Value

An `nli_sourcetrace` list with `ini` (per-parameter trace), `equations`
(per-line model() classification), and `summary` counts.
