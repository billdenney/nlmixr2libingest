# Validate a model (and optionally its vignette) into one terse result

Runs the whole validation chain and returns a compact pass/fix object
instead of the verbose logs that otherwise re-cache on every turn of the
build-check-fix loop. The fast tier (parse, conventions, source-trace)
is pure R and meant for iteration; the full tier adds R CMD check and a
vignette render. Convention checking is delegated to
`nlmixr2lib::checkModelConventions()` – this function never reimplements
it.

## Usage

``` r
validate_model(
  model,
  paper = NULL,
  level = c("fast", "full"),
  pkg = NULL,
  vignette = NULL,
  tol = 0.05
)
```

## Arguments

- model:

  An `rxUi`, a model function, or an `nlmixr2lib` model name.

- paper:

  Optional paper text or path; enables the source-trace stage.

- level:

  `"fast"` (parse + conventions + source-trace) or `"full"` (adds R CMD
  check and vignette render).

- pkg:

  Package directory for the full-tier R CMD check.

- vignette:

  Optional path to the model's validation vignette to render.

- tol:

  Relative tolerance for source-trace numeric matches.

## Value

An `nli_validation` object: `status` (`"success"`/`"issues"`), `issues`
(data frame), `counts`, and the `stages` run.
