# nlmixr2libingest

Token-efficient, quality-preserving tooling for ingesting published
population-PK/PD models into
[nlmixr2lib](https://github.com/nlmixr2/nlmixr2lib) at scale.

The cost of agentic literature-model extraction is dominated by
`cache_read × output` tokens across the build→check→fix loop — not by
the paper itself. This package attacks that cost without weakening the
**quality firewall**: the agent still source-traces every final `ini()`
value *and any nonstandard `model()`-block equation* against the
original paper. Every feature here is a cheaper input or a prior — never
a gate on what the agent sees of the paper.

## Package boundary

`nlmixr2libingest` is **ingestion only**. It *consumes* `nlmixr2lib` —
its canonical-name registers, compiled model database, rxUi parse, and
authoring API — but never reimplements model **validation**, which stays
in `nlmixr2lib::checkModelConventions()`.

## What it provides

- **Output filtering** —
  [`filter_check_log()`](reference/filter_check_log.md) /
  [`filter_render_log()`](reference/filter_render_log.md) and
  `inst/scripts/rcheck.sh` deterministically reduce `devtools::check()`
  / vignette-render logs to ERROR/WARNING/NOTE lines and failing chunks
  (deterministic on purpose: a dropped error is a quality regression).
- **Register lookup** —
  [`lookup_canonical()`](reference/lookup_canonical.md) and
  `inst/scripts/lookup.R`: a DuckDB full-text index over the three
  `nlmixr2lib` registers returns the few relevant entries (~1–2k tokens)
  instead of reading the ~284k-token covariate file. The index rebuilds
  only when a source register changes (size/mtime/md5), with a
  per-session TTL.
- **Structural features & archetypes** —
  [`model_features()`](reference/model_features.md) /
  [`build_feature_table()`](reference/build_feature_table.md) parse
  every library model into a structural feature table;
  [`assign_archetype()`](reference/assign_archetype.md) /
  [`archetype_taxonomy()`](reference/archetype_taxonomy.md) /
  [`archetype_template()`](reference/archetype_template.md) derive an
  interpretable archetype taxonomy (with an explicit `other` bucket) and
  starting templates;
  [`cluster_features()`](reference/cluster_features.md) gives a
  data-driven cross-check.
- **Fittable / archetype classifier** —
  [`build_store()`](reference/build_store.md) /
  [`classify()`](reference/classify.md) over a labelled corpus (TF-IDF
  always; embeddings when a local LLM is available, via
  [`embed_text()`](reference/embed_text.md)), predicting whether a paper
  is a fittable popPK/PD model and a top-archetype prior. Outputs are
  priors for annotate-first screening, not auto-decisions.
- **Source-trace pre-check** —
  [`source_trace()`](reference/source_trace.md) searches the paper for a
  supporting number for each final `ini()` value (back-transform- and
  rounding-tolerant) and flags those with none, plus the structural
  `model()` equations and hardcoded constants to verify. It assists,
  never replaces, the mandatory manual source-trace.
- **Soft budget backstop** —
  [`complexity_tier()`](reference/complexity_tier.md) /
  [`budget_advisor()`](reference/budget_advisor.md) give a
  complexity-scaled, gracefully-escalating token budget (continue →
  checkpoint → checkpoint-and-escalate, never a hard kill);
  [`run_token_stats()`](reference/run_token_stats.md) summarises the
  runner’s per-run token/cost records.
- **Distillation** — [`distill_paper()`](reference/distill_paper.md)
  produces an advisory structured extraction sheet via a local LLM
  (LLM-optional; `NULL` with no backend).
- **Unified validation** —
  [`validate_model()`](reference/validate_model.md) runs the whole chain
  (parse → `checkModelConventions()` →
  [`source_trace()`](reference/source_trace.md), plus optional R CMD
  check and a filtered vignette render) and returns one terse *Success /
  fix-list* instead of the multi-thousand-line logs that otherwise
  re-cache every turn. CLI: `inst/scripts/validate.R` (exit 0 = success,
  1 = issues).
- **Naming pre-brief** —
  [`naming_prebrief()`](reference/naming_prebrief.md) resolves a paper’s
  covariates (and optionally parameters/compartments) to canonical names
  *once*, up front, so the agent ingests one small report instead of
  loading the ~136k-token register or looking terms up turn-by-turn.
  Deterministic register scan ∪ optional LLM augmentation; a prior, not
  a gate. CLI: `inst/scripts/prebrief.R`.
- **Sidecar policy responder** —
  [`sidecar_respond()`](reference/sidecar_respond.md) auto-answers
  rule-like runner stop-and-ask pauses from a vetted policy table
  (`inst/policy/sidecar-policy.yaml`, mined from 1760 sidecar records)
  so a run continues on a warm cache instead of a cold retry. It only
  ever selects an option the agent was already offered, fires on a
  single policy, and escalates everything judgement-dependent to a
  human; every auto-answer is audited. CLI:
  `inst/scripts/sidecar_respond.R`.

## Portability

Hard dependencies are CRAN-only (`DBI`, `duckdb`, `cli`, `jsonlite`).
The local-LLM features (embedding-based classification, distillation)
are optional and degrade gracefully — with no LLM configured, the
deterministic levers (log filtering, register lookup,
features/archetypes, source-trace) all still run and classification
falls back to keyword matching.

## Status

Phases 0–3 plus the three input-side levers above (unified validation,
naming pre-brief, sidecar policy responder) are implemented and tested
(`devtools::check` clean). The remaining work is integration — wiring
[`naming_prebrief()`](reference/naming_prebrief.md) /
[`lookup_canonical()`](reference/lookup_canonical.md),
[`validate_model()`](reference/validate_model.md) / `rcheck.sh`, and
[`source_trace()`](reference/source_trace.md) into the
`extract-literature-model` skill; the fittable classifier into the
runner’s pre-dispatch hook; and `sidecar_respond.R` into the runner’s
sidecar watcher.

## Design

See `inst/design/ARCHITECTURE.md` for the engineering design and
`vignettes/articles/ingestion-challenges.Rmd` for the methods-paper
narrative on ingesting four decades of non-standardised popPK
literature.
