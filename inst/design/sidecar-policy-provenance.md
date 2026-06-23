# Sidecar Stop-and-Ask Mining Report

**Source data:** `/home/bill/gitlab/nlmixr2lib_ingestion/.claude_task_runner/sidecar/`
**Purpose:** Mine the operator's historical answers to the runner's stop-and-ask
(sidecar) questions to draft an auto-answer policy for an automated sidecar responder.
**Companion output:** `sidecar_policy_draft.yaml` (the draft policy table).

---

## 1. Corpus overview

| Metric | Count |
|---|---:|
| Task directories | 1364 |
| `request-*.json` files | 1386 |
| `response-*.json` files | 1341 |
| Individual question records (a request may hold 1-5 questions) | 1760 |
| Answered question records | 1724 |
| Requests with **no** matching response (unanswered) | 45 |
| Malformed-JSON requests (could not parse) | 2 |
| Questions per request (distribution) | 1q: 1025, 2q: 154, 3q: 81, 4q: 22, 5q: 3, 0q: 99 |

Schema is dominated by `schema_version: 2` (1284 requests); ~100 older/ad-hoc requests
use free-form top-level keys instead of a `questions[]` array — these were folded in where
a `question`/`options` pair was present.

**Operator vs. agent recommendation:** where the agent offered a `recommended` option,
the operator agreed **1294 / 1651 times (78%)**. The 22% override rate is the headline
reason to be conservative: roughly one in five "recommended" answers was wrong in the
operator's judgement, so the agent's own confidence is not a sufficient basis to automate.

### Important structural caveat (drove the whole analysis)

The answer `value` (`"A"`/`"B"`/`"C"`) is **positional within each record** and carries no
cross-task meaning — option A is "skip" in one task and "approve" in another. All
consistency measurements below therefore operate on the **chosen option's label/semantics**,
normalised into coarse ACTION classes (SKIP, OPERATOR_PROVIDES/REDISPATCH,
VERIFY_EXIT/ALREADY_DONE, APPROVE_AS_PROPOSED, CONFIRM_METADATA, ALTERNATIVE/MULTI, etc.),
**not** on the letter. Any real responder must likewise resolve a directive to the matching
option at runtime, never hardcode a letter.

---

## 2. Taxonomy: recurring question types, frequency, consistency

Classification is keyword-based on `prompt + summary + trigger + option-labels`
(first-match-wins, most-specific-first). Counts are over all 1760 records.

| # | Type | Records | Dominant operator action | Consistency | Verdict |
|--:|---|--:|---|---|---|
| 1 | **pdf_not_on_disk** | 538 | OPERATOR_PROVIDES/REDISPATCH 49% / SKIP 25% | Split, both need operator | ESCALATE |
| 2 | *other_uncategorized* (model structure/scope/encoding) | 305 | bespoke per task | Inconsistent | ESCALATE |
| 3 | **review_or_methodology_paper** | 281 | **SKIP (+queue primaries) 86%** of non-blank | **Consistent** | **POLICY (high)** |
| 4 | **new_canonical_covariate** | 174 | APPROVE 46% + large bespoke tail | Inconsistent | ESCALATE |
| 5 | **paper_identity_mismatch** | 143 | SKIP 25% / APPROVE 17% / CONFIRM 9% | Scattered | ESCALATE |
| 6 | **value_abstract_vs_table** | 69 | APPROVE 27% / SKIP 19% / bespoke | Inconsistent | ESCALATE |
| 7 | **already_merged_resumption** | 61 | VERIFY-and-EXIT 63% of non-blank | Mostly consistent | **POLICY (medium)** |
| 8 | **infeasible_for_nlmixr** | 59 | SKIP 49% + extract-sub-model tail | Mixed | ESCALATE |
| 9 | **proceed_confirmation** (mostly skill-not-registered) | 56 | OPERATOR installs skill 71% | Consistent but operator-action | ESCALATE |
| 10 | **missing_parameter_or_supplement** | 40 | SKIP 56% + operator-provides tail | Mixed | ESCALATE |
| 11 | **covariate_encoding_naming** | 9 | full-fidelity / drop / anchor (split) | Inconsistent | ESCALATE |
| 12 | **pknca_mismatch** | 9 | verify / skip / ack-bug (split) | Inconsistent | ESCALATE |
| 13 | **multiple_candidate_models** | 7 | extract-all vs single (split) | Inconsistent | ESCALATE |
| 14 | **unit_convention** | 6 | skip / multiplicative / recode | Inconsistent | ESCALATE |
| 15 | **initial_vs_final_estimate** | 3 | use published FINAL estimates 100% | Too few | ESCALATE (watch) |

Two infrastructure sub-types pulled out of the `other_uncategorized` bucket:

- **worktree `working_dir: null`** — deterministic queue-YAML misconfiguration; operator
  answer was unanimous ("fill in `working_dir`, re-dispatch"). Promoted to a MEDIUM policy.
- The remainder of `other_uncategorized` (~300 answered) is the **model-structure / scope /
  encoding** cluster (how to encode a CL/PD relationship, split into N files, IIV/residual
  structure, placebo/dropout sub-models, allometry references, depends_on ordering). These
  are bespoke per paper — the canonical MUST-ESCALATE bucket.

---

## 3. The decisive filter: "consistent" is necessary but not sufficient

A sidecar is raised precisely because the dispatched agent decided it **could not proceed
autonomously**. So a high-consistency answer is only auto-answerable if the canonical
answer is a **self-contained directive the agent can execute with no operator input**.
Several high-consistency types fail this test:

- **pdf_not_on_disk** (538, the biggest type): even the dominant answers
  ("operator acquires/drops the PDF and re-dispatches", "skip") need a human — only a person
  can fetch the paper or decide it is not worth chasing. Auto-skipping would silently drop
  extractable papers. **NOT auto-answerable.**
- **proceed_confirmation / skill-not-registered** (71% one answer): the answer is an
  *operator action* ("install the skill, re-dispatch"), not something the agent can do to
  unblock itself. Auto-acknowledging unblocks nothing and masks a possible harness
  regression. **NOT auto-answerable** (treat as an infra alert instead).

By contrast the two types that *passed* the filter both rest on a fact the agent can
**verify itself**: a review/methodology paper has no fittable model (skill's own scope
rule), and an already-merged deliverable is checkable via git.

---

## 4. Policies proposed (3) — what they cover and why

### 4.1 `review_or_methodology_paper_skip` — confidence **HIGH**
- **Frequency:** 281 records; **219/255 non-blank answers (86%) chose skip-family.**
- **Canonical answer:** "Skip this task (and queue cited primary popPK/PD papers if they
  contain real models)."
- **Why safe:** self-contained, matches the extract-literature-model skill's out-of-scope
  rule. The agent already detected the paper is a review/meta-analysis; the operator almost
  always confirms skip.
- **Residual risk / guard:** the rare review that embeds a *novel* fitted popPK model. The
  responder must escalate when an "extract anyway" option is offered and evidence of a
  novel model is present.
- **Example prompts:**
  - "How should this task proceed given that Zhang 2016 is a DDI study reporting (not
    developing) the popPK model from upstream Zhu 2014?"
  - "How should this task proceed given that the Sharma 2024 PBPK model is not reproducible
    from on-disk sources...?"

### 4.2 `already_merged_verify_and_exit` — confidence **MEDIUM**
- **Frequency:** 61 records; **27/43 non-blank answers (63%) chose verify-and-exit;** 17 blank.
- **Canonical answer:** "Verify the merged/pushed content and exit cleanly without re-extracting."
- **Why safe (conditionally):** the triggering fact (deliverable already on `origin/main`)
  is git-verifiable by the agent.
- **Residual risk / guard:** a real tail asked the agent to ADD a companion model (e.g. a
  separate PD file) absent from the merged commit. **Auto-answer ONLY after the agent
  confirms the merged commit holds the full requested deliverable; otherwise escalate.**
- **Example prompts:**
  - "The Bista 2015 fentanyl model and vignette are already in origin/main (merged via
    PR #417). Should this dispatch re-extract or exit cleanly?"
  - "Plock_2014_ferumoxytol is already in origin/main... How should this task proceed?"

### 4.3 `worktree_working_dir_null` — confidence **MEDIUM**
- **Frequency:** small but unanimous in observed cases.
- **Canonical answer:** "Operator fills in `working_dir` and lets the pre-dispatch hook
  create the worktree."
- **Why safe / low-risk:** deterministic infra misconfiguration, not a scientific decision;
  the recorded answer does not itself let the agent proceed (operator still edits the YAML),
  so a wrong auto-answer cannot poison an extraction. Medium only because the sample is small.

---

## 5. Types flagged MUST-ESCALATE (12)

`pdf_not_on_disk`, `new_canonical_covariate`, `paper_identity_mismatch`,
`value_abstract_vs_table`, `infeasible_for_nlmixr`, `missing_parameter_or_supplement`,
`covariate_encoding_naming`, `pknca_mismatch`, `multiple_candidate_models`,
`unit_convention`, `initial_vs_final_estimate`, and the catch-all
`model_structure_scope_and_encoding` (the bulk of `other_uncategorized`).
Plus `skill_not_registered_proceed_confirmation` as an infra alert.

Reasons fall into three buckets:
1. **Operator-action-required** (the answer is the human doing work the agent cannot):
   `pdf_not_on_disk`, `skill_not_registered`, parts of `missing_parameter_or_supplement`.
2. **Scientific-fidelity judgement** (a wrong answer silently poisons the extraction):
   `value_abstract_vs_table`, `unit_convention`, `new_canonical_covariate`,
   `covariate_encoding_naming`, `model_structure_scope_and_encoding`,
   `initial_vs_final_estimate`.
3. **Genuinely inconsistent** historical answers: `paper_identity_mismatch`,
   `multiple_candidate_models`, `pknca_mismatch`, `infeasible_for_nlmixr`.

A near-miss worth re-evaluating once more data accrues: **`initial_vs_final_estimate`** —
all 3 observed answers chose "use the published FINAL parameter estimates", a plausible
future high-confidence rule, but n=3 is too small now.

---

## 6. Caveats & method notes

- 45 unanswered requests and 2 malformed-JSON requests were excluded from consistency stats.
- Classification is keyword-heuristic; a record matching multiple type-patterns is assigned
  to the most specific (first-listed) rule. Boundary cases (e.g. an already-merged paper that
  is *also* a review) may be mis-bucketed by a few records — material enough to keep the
  medium-confidence policies gated on an agent-verifiable fact rather than on the keyword
  match alone.
- "Consistency" was measured on normalised semantic ACTION of the chosen option, not on the
  raw A/B/C letter (which is positional). The responder must do the same resolution.

---

## 7. Bottom line

- **1724** answered sidecar question records analysed across **1386** requests.
- **Top recurring types:** pdf_not_on_disk (538), model-structure/scope/encoding (~305),
  review/methodology paper (281), new_canonical_covariate (174), paper_identity_mismatch (143).
- **Policies proposed: 3** — 1 high-confidence (review-paper skip), 2 medium-confidence
  (already-merged verify-and-exit; worktree working_dir-null), each gated on an
  agent-verifiable fact.
- **Types flagged MUST-ESCALATE: 12** (plus a skill-registration infra alert).
- The conservative stance is deliberate: with a 22% operator-override rate on the agent's
  own recommendations, and a sidecar by definition marking the point the agent could not
  decide alone, only directives the agent can self-verify and self-execute were automated.
