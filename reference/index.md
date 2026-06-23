# Package index

## All functions

- [`archetype_taxonomy()`](archetype_taxonomy.md) : Summarise the
  empirical archetype taxonomy
- [`archetype_template()`](archetype_template.md) : Generate a starting
  template for an archetype
- [`assign_archetype()`](assign_archetype.md) : Assign a structural
  archetype label to each model
- [`budget_advisor()`](budget_advisor.md) : Soft budget advisor (never a
  hard kill)
- [`build_feature_table()`](build_feature_table.md) : Build the
  structural feature table for the nlmixr2lib model library
- [`build_model_store()`](build_model_store.md) : Build a similarity
  store from the nlmixr2lib model library
- [`build_register_index()`](build_register_index.md) : Build (or
  rebuild) the canonical-name register index
- [`build_store()`](build_store.md) : Build a similarity store from a
  labelled corpus
- [`classify()`](classify.md) : Classify a paper from its text against a
  labelled store
- [`classify_fittable_keyword()`](classify_fittable_keyword.md) :
  Heuristic fittable score from popPK vocabulary (no store / no LLM)
- [`cluster_features()`](cluster_features.md) : Data-driven archetype
  view: cluster models on a Gower distance
- [`complexity_tier()`](complexity_tier.md) : Complexity tier for a
  model from its structural features
- [`distill_paper()`](distill_paper.md) : Distill a structured
  extraction sheet from a paper (LLM-optional)
- [`embed_text()`](embed_text.md) : Embed text with the local embedding
  model
- [`embeddings_available()`](embeddings_available.md) : Is a local
  embedding backend available?
- [`evaluate_archetype_cv()`](evaluate_archetype_cv.md) : Leave-one-out
  archetype accuracy of a store (k-NN cross-validation)
- [`filter_check_log()`](filter_check_log.md) : Filter an R CMD check /
  devtools::check log to the parts that matter
- [`filter_render_log()`](filter_render_log.md) : Filter an rmarkdown /
  knitr render log to the failure and its context
- [`lint_vignette()`](lint_vignette.md) : Statically pre-lint a
  validation vignette for common render-killers
- [`lookup_canonical()`](lookup_canonical.md) : Look up canonical names
  in the nlmixr2lib registers
- [`model_features()`](model_features.md) : Structural features for a
  single nlmixr2lib model
- [`naming_prebrief()`](naming_prebrief.md) : Build a model-specific
  naming pre-brief from a paper
- [`nearest()`](nearest.md) : Nearest neighbours of a query string in a
  store
- [`parse_registers()`](parse_registers.md) : Parse the nlmixr2lib
  canonical-name registers into a tidy data frame
- [`register_db_path()`](register_db_path.md) : Default on-disk location
  of the cached register index (a DuckDB file)
- [`registers_dir()`](registers_dir.md) : Locate the nlmixr2lib
  canonical-name register directory
- [`render_lookup()`](render_lookup.md) : Render lookup results as
  compact markdown (for CLI / agent consumption)
- [`render_prebrief()`](render_prebrief.md) : Render a naming pre-brief
  as compact markdown
- [`run_token_stats()`](run_token_stats.md) : Token / cost analytics
  over the runner's RunRecords
- [`save_store()`](save_store.md) [`load_store()`](save_store.md) : Save
  / load a similarity store
- [`sidecar_match()`](sidecar_match.md) : Decide, per question, whether
  a sidecar request is auto-answerable
- [`sidecar_policy()`](sidecar_policy.md) : Load the sidecar auto-answer
  policy table
- [`sidecar_respond()`](sidecar_respond.md) : Auto-answer a sidecar
  request when policy permits
- [`source_trace()`](source_trace.md) : Programmatic source-trace
  pre-check for a model against its paper
- [`tier_budget()`](tier_budget.md) : Token budget for a complexity tier
- [`validate_model()`](validate_model.md) : Validate a model (and
  optionally its vignette) into one terse result
