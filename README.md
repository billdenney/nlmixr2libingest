# nlmixr2libingest

<!-- badges: start -->
[![R-CMD-check](https://github.com/billdenney/nlmixr2libingest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/billdenney/nlmixr2libingest/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/billdenney/nlmixr2libingest/graph/badge.svg)](https://app.codecov.io/gh/billdenney/nlmixr2libingest)
<!-- badges: end -->

Token-efficient, quality-preserving tooling for ingesting published
population-PK/PD models into [nlmixr2lib](https://github.com/nlmixr2/nlmixr2lib)
at scale.

The cost of agentic literature-model extraction is dominated by
`cache_read × output` tokens across the build→check→fix loop — not by the paper
itself. This package attacks that cost without weakening the **quality
firewall**: the agent still source-traces every final `ini()` value *and any
nonstandard `model()`-block equation* against the original paper. Every feature
here is a cheaper input or a prior — never a gate on what the agent sees of the
paper.

## Package boundary

`nlmixr2libingest` is **ingestion only**. It *consumes* `nlmixr2lib` — its
canonical-name registers, compiled model database, rxUi parse, and authoring
API — but never reimplements model **validation**, which stays in
`nlmixr2lib::checkModelConventions()`.

## What it provides

- **Output filtering** — `filter_check_log()` / `filter_render_log()` and
  `inst/scripts/rcheck.sh` deterministically reduce `devtools::check()` /
  vignette-render logs to ERROR/WARNING/NOTE lines and failing chunks
  (deterministic on purpose: a dropped error is a quality regression).
- **Register lookup** — `lookup_canonical()` and `inst/scripts/lookup.R`: a
  DuckDB full-text index over the three `nlmixr2lib` registers returns the few
  relevant entries (~1–2k tokens) instead of reading the ~284k-token covariate
  file. The index rebuilds only when a source register changes (size/mtime/md5),
  with a per-session TTL.
- **Structural features & archetypes** — `model_features()` /
  `build_feature_table()` parse every library model into a structural feature
  table; `assign_archetype()` / `archetype_taxonomy()` / `archetype_template()`
  derive an interpretable archetype taxonomy (with an explicit `other` bucket)
  and starting templates; `cluster_features()` gives a data-driven cross-check.
- **Fittable / archetype classifier** — `build_store()` / `classify()` over a
  labelled corpus (TF-IDF always; embeddings when a local LLM is available, via
  `embed_text()`), predicting whether a paper is a fittable popPK/PD model and a
  top-archetype prior. Outputs are priors for annotate-first screening, not
  auto-decisions.
- **Source-trace pre-check** — `source_trace()` searches the paper for a
  supporting number for each final `ini()` value (back-transform- and
  rounding-tolerant) and flags those with none, plus the structural `model()`
  equations and hardcoded constants to verify. It assists, never replaces, the
  mandatory manual source-trace.
- **Soft budget backstop** — `complexity_tier()` / `budget_advisor()` give a
  complexity-scaled, gracefully-escalating token budget (continue → checkpoint →
  checkpoint-and-escalate, never a hard kill); `run_token_stats()` summarises the
  runner's per-run token/cost records.
- **Distillation** — `distill_paper()` produces an advisory structured
  extraction sheet via a local LLM (LLM-optional; `NULL` with no backend).

## Portability

Hard dependencies are CRAN-only (`DBI`, `duckdb`, `cli`, `jsonlite`). The
local-LLM features (embedding-based classification, distillation) are optional
and degrade gracefully — with no LLM configured, the deterministic levers (log
filtering, register lookup, features/archetypes, source-trace) all still run and
classification falls back to keyword matching.

## Status

Phases 0–3 are implemented and tested (`devtools::check` clean). The remaining
work is integration — wiring `lookup_canonical()`, `rcheck.sh`, `source_trace()`,
and the fittable classifier into the `extract-literature-model` skill and the
ingestion runner's pre-dispatch hook.

## Design

See `inst/design/ARCHITECTURE.md` for the engineering design and
`vignettes/articles/ingestion-challenges.Rmd` for the methods-paper narrative on
ingesting four decades of non-standardised popPK literature.
