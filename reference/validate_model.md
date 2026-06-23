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
  level = c("fast", "full", "model"),
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

  `"fast"` (parse + conventions + source-trace), `"model"` (adds a
  one-session `load_all` of `pkg` + the model's vignette render – the
  per-iteration combined gate, *no* whole-package check), or `"full"`
  (adds a whole-package R CMD check + render; the pre-commit gate).

- pkg:

  Package directory: `load_all`-ed for the `model` tier, R-CMD-checked
  for the `full` tier.

- vignette:

  Optional path to the model's validation vignette to render.

- tol:

  Relative tolerance for source-trace numeric matches.

## Value

An `nli_validation` object: `status` (`"success"`/`"issues"`), `issues`
(data frame), `counts`, and the `stages` run.
