# Ingesting four decades of population-PK literature: challenges and a token-efficient, quality-preserving pipeline

## Abstract

Reproducing a published population pharmacokinetic / pharmacodynamic
(popPK/PD) model in executable form is a deceptively hard
information-extraction problem. The source corpus spans roughly four
decades, multiple modelling tools and notational traditions, and no
enforced reporting standard. We describe `nlmixr2libingest`, tooling
that lets a large-language-model agent extract such models into the
`nlmixr2lib` library at scale while holding a strict quality invariant:
every estimated value and every nonstandard structural equation in the
final model is source-traced to the original paper. We frame the cost of
agentic extraction (dominated by cached-context re-reads across a
build–check–fix loop, not by the paper), catalogue the heterogeneity
that makes extraction hard, and present a deterministic-first
architecture — log filtering, register lookup, structural archetypes,
and optional local-LLM distillation — whose every acceleration is a
cheaper *input* or a *prior*, never a gate on what the agent reads of
the paper.

## 1. Introduction

Population PK/PD models are published as a mix of prose, equations,
tables, and (sometimes) control streams. A reader who wants to *run* the
model must recover its compartmental structure, parameterisation,
inter-individual and residual variability, covariate relationships, and
dosing/observation conventions, then re-express them in a modelling
language. Doing this faithfully for one paper is routine expert work;
doing it for thousands of papers is an industrial-scale extraction
problem with an unusual constraint — the output must be *numerically
faithful to a fixed ground truth* (the paper), so “plausible but wrong”
is a failure, not a minor error.

LLM agents can do this extraction, but at a cost structure that is
unintuitive: the paper is cheap (a trimmed paper is on the order of 8.6k
tokens); the expense is the **cached context re-read on every turn of
the build–check–fix loop**, multiplied by the number of turns. Reducing
cost therefore means shrinking the per-turn context, cutting the turn
count, and not dispatching the agent at all on the large fraction of
indexed documents that are not fittable popPK papers.

The central design tension is that the cheapest way to shrink context —
letting a retrieval system or a small model decide what the agent sees —
is exactly the way to silently lose fidelity. Our resolution is a
**quality firewall**: cheap pre-processing may propose and prioritise,
but the agent always verifies the final numbers and nonstandard
equations against the full paper, which is never gated.

## 2. The heterogeneity problem (why this is hard)

Notes toward a taxonomy of what varies across the corpus. Each item is a
concrete extraction hazard, not just trivia.

- **No standardised reporting.** There is no enforced equivalent of
  CONSORT for popPK. Structural model, parameter table, variability
  model, and covariate equations may each be in text, a table, a figure,
  an appendix, or only in a supplementary control stream.
- **Tooling and notation drift over ~40 years.** NONMEM (ADVAN/TRANS
  macros, `THETA`/`ETA`/`EPS`), Monolix (mlxtran), ADAPT, WinNonlin,
  SAAM II, Phoenix, Stan, and hand-derived closed-form solutions all
  appear. The *same* model can be written as ODEs, as a closed-form
  `linCmt()`-style solution, or as ADVAN subroutine references the
  reader must expand.
- **Parameterisation ambiguity.** Clearance/volume vs micro-rate
  constants; `CL`/`V` vs `k`/`V`; absorption as first-order `ka`,
  transit chains, or zero-order; central volume reported as `V`, `V1`,
  `Vc`, or `V/F`. Apparent (`/F`) vs absolute parameters are frequently
  implicit.
- **Variability conventions.** IIV reported as variance, SD, CV%, or %;
  on log, logit, or additive scale; full vs block vs diagonal Ω;
  shrinkage sometimes conflated with estimates. Residual error as
  additive, proportional, combined, or log-normal, with units that must
  be inferred.
- **Units and scaling.** Mixed time units (h vs day), amount vs
  concentration, molar vs mass, allometric reference weights left
  unstated, BSA/FFM/IBW derivations (DuBois/Mosteller, Janmahasatian,
  Devine) often uncited.
- **Covariate model expression.** Centering references, fixed vs
  estimated exponents, indicator-variable encodings, and study-specific
  covariates whose meaning is not portable across papers (the motivation
  for a *scope* field in the covariate register).
- **Document quality.** Older papers are scanned images requiring OCR;
  tables break across pages; Greek letters and subscripts are mangled;
  errata and supplements live in separate files.
- **Non-fittable look-alikes.** GastroPlus/PBPK-QSP exports, posters,
  reviews, and methods papers are textually similar to fittable popPK
  papers but should not be dispatched for extraction.

## 3. Methods (the pipeline)

### 3.1 Cost model and quality firewall

We treat per-run cost as `cache_read × output`, where `cache_read` is
roughly (cached context size) × (number of agent turns). The
optimisation targets are therefore (a) per-turn context size, (b) turn
count, and (c) the share of dispatches spent on non-fittable papers.
Every mechanism below is admissible only under the firewall of Section
1: source-trace every final `ini()` value and any nonstandard `model()`
equation; never gate the paper; treat machine outputs as priors.

### 3.2 Deterministic levers (no LLM required)

- **Log filtering.** `devtools::check()` and vignette-render logs are
  reduced deterministically to ERROR/WARNING/NOTE steps and failing
  chunks. Determinism is the point: an LLM summary that drops a real
  error would corrupt the loop.
- **Register lookup.** The three `nlmixr2lib` canonical-name registers
  (covariate ≈284k tokens, compartment, parameter) are parsed into a
  DuckDB full-text index; the agent retrieves the few relevant entries
  by name, alias, or concept instead of reading the file. The index
  rebuilds only when a source register changes.
- **Structural features and archetypes.** Each existing model is parsed
  (via its rxUi) into a structural feature vector — compartments,
  absorption, elimination, PD type, variability, covariates, residual
  error — augmented with its `description`/`reference`/`vignette` text.
  Clustering yields an empirical archetype taxonomy, with an explicit
  *other* bucket for unique-but-interesting structures that should be
  flagged rather than forced into a template.

### 3.3 Optional, pluggable LLM acceleration

When a local LLM is configured, embeddings (`nomic-embed-text`) drive a
fittable classifier and an archetype prior, and a small instruct model
emits a structured distillation sheet (parameters, values, units, RSE,
IIV, covariate equations, residual error, dosing) that the agent builds
from and verifies against. With no LLM, classification falls back to
keyword/feature rules and distillation is skipped — the agent reads the
trimmed paper directly. The acceleration is never load-bearing for
correctness.

### 3.4 Scale orchestration

A pre-dispatch classifier annotates likely-non-fittable papers and sets
an effort tier (annotate-first; auto-skip only after held-out
validation). A soft, complexity-tiered budget checkpoints work and
escalates to a human via a sidecar question on overrun rather than
hard-killing.

## 4. Results

*To be completed as the pipeline is built and benchmarked.* Planned
measurements, reported from the runner’s per-run token/cost records:

- Token economics: cache_read and output per run, before vs after each
  lever, on a fixed paper set (not anecdotes).
- Register lookup: context tokens avoided per covariate/parameter
  resolution.
- Classifier: held-out precision/recall for the fittable decision;
  archetype top-3 hit rate.
- Archetype taxonomy: cluster inventory and frequencies across the 981
  models, including the size and contents of the *other* bucket.

## 5. Discussion

Open questions and limitations to develop: where deterministic structure
extraction stops and judgement begins; how much an archetype prior helps
vs anchors; the failure modes of small-model distillation and why
verification remains mandatory; the portability cost of depending on
local infrastructure; and the boundary discipline between an ingestion
package and the validated library it feeds.

## 6. Conclusion

A token-efficient extraction pipeline need not trade away fidelity. By
keeping every acceleration on the *input* and *prior* side of a hard
source-trace firewall, and by making the LLM optional rather than
central, the same deterministic core that lowers cost also keeps the
agent honest.
