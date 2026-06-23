# Build a model-specific naming pre-brief from a paper

Resolves the covariates (and optionally parameters/compartments) a paper
uses to their canonical `nlmixr2lib` names up front, so the agent
ingests one small report instead of loading the register or looking
terms up turn-by-turn. Candidates come from a deterministic register
scan plus, when a local LLM is configured,
[`distill_paper()`](distill_paper.md); each is resolved with
[`lookup_canonical()`](lookup_canonical.md). Unmatched candidates
(possible new canonical names) are flagged. This is a prior, not a gate:
the agent still source-traces every value against the paper.

## Usage

``` r
naming_prebrief(
  paper,
  kinds = "covariate",
  backend = .llmBackend(),
  top_k = 3L,
  dir = registers_dir(),
  db_path = register_db_path()
)
```

## Arguments

- paper:

  Paper text, or a path to a (trimmed) paper file.

- kinds:

  Which register kinds to resolve: any of `"covariate"`, `"parameter"`,
  `"compartment"`.

- backend:

  LLM backend for the candidate-augmenting distillation
  (`"ollama"`/`"none"`/`"auto"`); `"none"` uses the deterministic scan
  only.

- top_k:

  Rows to consider per lookup (the best is used).

- dir:

  Register directory (defaults to
  [`registers_dir()`](registers_dir.md)).

- db_path:

  Lookup index location (defaults to
  [`register_db_path()`](register_db_path.md)).

## Value

An `nli_prebrief` list: one entry per kind (`matched`/`unmatched`), plus
`backend` and `n` (matched count). Render with
[`render_prebrief()`](render_prebrief.md).
