# nlmixr2libingest

<!-- badges: start -->
[![R-CMD-check](https://github.com/billdenney/nlmixr2libingest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/billdenney/nlmixr2libingest/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/billdenney/nlmixr2libingest/graph/badge.svg)](https://app.codecov.io/gh/billdenney/nlmixr2libingest)
<!-- badges: end -->

Token-efficient, quality-preserving tooling for ingesting published
population-PK/PD models into [nlmixr2lib](https://github.com/nlmixr2/nlmixr2lib)
at scale.

The cost of literature-model extraction is dominated by `cache_read × output`
tokens across the build→check→fix loop — not by the paper itself. This package
attacks that cost without weakening the **quality firewall**: an agent still
source-traces every final `ini()` value *and any nonstandard `model()`-block
equation* against the original paper. Every feature here is a cheaper input or a
prior, never a gate on what the agent sees of the paper.

## Package boundary

`nlmixr2libingest` is **ingestion only**. It *consumes* `nlmixr2lib` — its
canonical-name registers, compiled model database, rxUi parse, and authoring
API — but never reimplements model **validation**, which stays in
`nlmixr2lib::checkModelConventions()`.

## Phase 0 (current)

- **`filter_check_log()` / `filter_render_log()`** + `inst/scripts/rcheck.sh` —
  deterministically reduce `devtools::check()` / vignette-render logs to the
  ERROR/WARNING/NOTE lines and failing chunks. Deterministic on purpose: a
  dropped error is a quality regression, so no LLM summarisation here.
- **`lookup_canonical()`** + `inst/scripts/lookup.R` — a DuckDB full-text index
  over the three `nlmixr2lib` registers (covariate ≈284k tokens, compartment,
  parameter). Returns the few relevant entries instead of the whole file. The
  index rebuilds only when a source register changes (size/mtime/md5), cached
  per session with a 1-day TTL.

## Portability

Hard dependencies are CRAN-only (`DBI`, `duckdb`, `cli`, `stringr`, `jsonlite`).
The optional local-LLM features (later phases: distillation, embedding-based
classification) degrade gracefully — with no LLM configured, the deterministic
levers (log filtering, register lookup, model features, source-trace) all still
run, and classification falls back to keyword matching.

## Roadmap

See `inst/design/ARCHITECTURE.md` (engineering) and
`vignettes/articles/ingestion-challenges.Rmd` (the seed of a methods paper on
ingesting four decades of non-standardised popPK literature). Phases 1–3 add
model features + archetype templates, a fittable/archetype classifier, and
local-LLM-assisted distillation with a soft budget backstop.
