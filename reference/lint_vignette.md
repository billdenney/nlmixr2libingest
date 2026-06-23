# Statically pre-lint a validation vignette for common render-killers

Scans the vignette's R chunks (and, when supplied, the model) for the
recurring failures that otherwise only surface during an expensive
render: an oversized simulation cohort, a missing `cmt =` for an
algebraic observable (the most common slot-renumbering failure), a
named-vector scalar passed to `amt =`, and a PKNCA input filter that
drops the `time = 0` row.

## Usage

``` r
lint_vignette(rmd, model = NULL, max_per_arm = 200L)
```

## Arguments

- rmd:

  Path to the vignette `.Rmd`.

- model:

  Optional `rxUi` / model function / `nlmixr2lib` name; enables the
  algebraic-observable `cmt =` check.

- max_per_arm:

  Maximum simulated participants per arm (default 200).

## Value

An `nli_vignette_lint` list with `issues` (data frame) and `n`.
