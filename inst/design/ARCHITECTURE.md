# nlmixr2libingest — architecture

Engineering counterpart to `vignettes/articles/ingestion-challenges.Rmd` (the
methods-paper narrative). This document is the durable design context; keep it
current as phases land.

## Problem and economics

Literature-model extraction into `nlmixr2lib` is run at scale by an
agent-driven pipeline (the `extract-literature-model` skill dispatched by the
`nlmixr2lib_ingestion` runner). Across ~2,335 completed runs the cost is
dominated by **`cache_read × output`** tokens (median cache-read ≈1.3M
tokens/run), *not* by the paper (trimmed papers ≈8.6k tokens). The three levers
that move that product:

1. **Shrink the cached per-turn context.** The single largest avoidable item is
   the canonical covariate register (`covariate-columns.md`, 1.14 MB ≈ 284k
   tokens) — one accidental whole-file read is a budget bomb.
2. **Cut the number of turns** in the build→check→fix loop. The biggest source
   is re-reading multi-thousand-line `devtools::check()` / render logs.
3. **Never dispatch for non-fittable papers** (~50% of indexed batches:
   GastroPlus/QSP exports, posters, reviews).

## The quality firewall (the one invariant)

Every optimization here is quality-neutral **iff**:

1. The agent still **source-traces every final `ini()` value AND any nonstandard
   equation in the `model()` block** against the original paper. Cheap
   pre-processing may be wrong; it is caught here.
2. Retrieval gates only the **registers/large supplements**, never the (small)
   paper — the paper stays fully in the agent's context.
3. Classifier / cluster / distillation outputs are **priors, not decisions**
   (they seed a template and flag for screening; the agent verifies/overrides).
4. **Check/render logs are filtered deterministically, not LLM-summarised** — a
   hallucinated-away ERROR is a quality loss.

## Package boundary

- **nlmixr2libingest = ingestion** (this package): retrieval, classification,
  archetypes, distillation, source-trace prep, output filtering, budget.
  Operates over *many* papers.
- **nlmixr2lib = library + validation**: the registers (`inst/references/*.md`),
  `checkModelConventions()`, `buildModelDb()`, the `add*`/`convert*` authoring
  API. These stay in `nlmixr2lib`.
- nlmixr2libingest **consumes** nlmixr2lib (registers via `system.file()`, the
  compiled `modeldb`, rxUi parse, the piping API) and never reimplements
  validation. Newly-questionable overlaps are surfaced and decided, not assumed.

## Portability

- Hard dependencies are CRAN-only and portable: `DBI`, `duckdb`, `cli`,
  `stringr`, `jsonlite`.
- The local LLM is an **optional, pluggable backend** behind one abstraction
  (`R/llm.R`, Phase 3): embeddings via ollama `nomic-embed-text` at
  `http://localhost:11434`; chat/extract via the `local-llm` MCP, ollama, or a
  documented cloud hook; selected by `getOption("nlmixr2libingest.llm")`,
  default `none`.
- **Degradation matrix** — with no LLM, the deterministic levers all run:

  | capability                         | needs LLM? | no-LLM fallback                                   |
  |------------------------------------|:----------:|---------------------------------------------------|
  | log filtering (`rfilter`)          |     no     | —                                                 |
  | register lookup (`lookup`)         |     no     | —                                                 |
  | model features / archetypes        |     no     | —                                                 |
  | source-trace pre-check             |     no     | —                                                 |
  | fittable / archetype classifier    |  optional  | DuckDB-FTS / TF-IDF keyword + feature rules       |
  | distillation sheet                 |    yes     | skipped — agent reads the trimmed paper directly  |

## Module map

```
R/
  utils.R        # register-dir / db-path resolution, source signatures   [P0]
  registers.R    # parse the 3 markdown registers -> tidy data frame       [P0]
  lookup.R       # DuckDB FTS index + freshness + lookup_canonical()        [P0]
  rfilter.R      # deterministic check/render log filters                   [P0]
  llm.R          # pluggable LLM/embedding backend (ollama | mcp | none)    [P3]
  features.R     # rxUi -> structural feature table (relocatable API)       [P1]
  archetypes.R   # cluster features -> taxonomy (+ 'other') + templates     [P1]
  vectordb.R     # embed corpus into DuckDB (LLM) | keyword index (no-LLM)  [P2]
  classify.R     # paper -> {fittable?, top-3 archetype prior}              [P2]
  distill.R      # LLM extraction sheet (params/units/RSE/IIV/cov/dosing)   [P3]
  sourcetrace.R  # grep ini() values + flag nonstandard model() equations   [P3]
  budget.R       # soft token/turn budget -> checkpoint + sidecar escalate  [P3]
inst/scripts/    # rcheck.sh (lever 1), lookup.R (lever 2) CLIs             [P0]
```

## Lookup index lifecycle (Phase 0)

`lookup_canonical()` queries a DuckDB file (`tools::R_user_dir(...,'cache')`)
holding the parsed registers and an FTS index. `build_register_index()` records
each source file's size/mtime/md5; before a lookup, `.ensureIndex()` rebuilds
only if a register changed (size/mtime fast-path, md5 authority), gated by a
per-session TTL (default 1 day) so the stat is not repeated each call. If the
DuckDB `fts` extension is unavailable (offline), the index still builds and
lookups fall back to token `LIKE` matching.

## Learning subsystem (Phases 1–2)

Train on the 981 models in `nlmixr2lib/inst/modeldb/` (positives, with
parseable structure + `description`/`reference`/`vignette` text) plus a negative
set (sidecar-skipped + index-categorised non-fittable papers). Deterministic
features → archetype clusters (with an explicit **`other`/atypical** bucket for
unique-but-interesting models, flagged not discarded) → templates. The
classifier (embedding k-NN, or keyword fallback) yields a fittable decision and
an archetype prior. Each new extraction is appended as a labelled example: the
classifier and templates improve over time.

## Integration (Phase 3)

The skill swaps register browsing for `lookup_canonical()`, consumes the distill
sheet + archetype template, and runs `sourcetrace` + `rcheck.sh`. The runner's
pre-dispatch hook calls `classify()` to annotate fittable-skip + effort tier
(annotate-first; auto-skip only after held-out validation), with `budget.R` as a
soft, checkpoint-and-sidecar backstop.
