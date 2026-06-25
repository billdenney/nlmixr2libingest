# Per-task extraction economics: a token & cost benchmark across the optimization rollout

## Purpose

This article measures the **per-task token and dollar cost** of
literature model extraction, split by outcome (a model was **built** vs
the paper was a **no-extraction** skip), and compares a **recent**
cohort against an older **baseline** cohort. The motivating question:
*how much benefit are the token optimizations delivering?*

The cost model (see `ingestion-challenges.Rmd`) is dominated by
`cache_read x agent-turns`, so we report the cost drivers — cache-read
and output tokens — alongside total tokens and the runner’s dollar
estimate.

## Data and method

- **Source.** Per-task aggregates summed across all runs of each
  `completed` task, read from the runner state files (frozen snapshot).
- **Cohorts.**
  - `recent_24h` — completed in the 24 h before the snapshot (2026-06-25
    11:28 UTC). This is **after** the agent-side *policy self-check*
    (merged 2026-06-23) and its *strengthening* (2026-06-24).
  - `baseline_48h+` — the 50 most-recent tasks completed **more than 48
    h** before the snapshot that carry a deliverable report (so they are
    classifiable). This is **before** the policy self-check.
  - `preopt_pre0622` — **all** tasks completed **before 2026-06-22** (n
    ≈ 1565), i.e. **before Phase 0** (register-lookup + R-log-filter).
    These predate the deliverable-report convention, so they cannot be
    split extraction-vs-skip; they are reported **overall (dollar +
    tokens) only**. Cost is reliable here — **100%** of these records
    carry a populated `cost_usd` (vs gaps in the report-era cohorts) —
    and the cohort is **extraction-dominated** (≈76% have \>12k output
    tokens).
- **Outcome classifier.** Report-based: a task is a `no_extraction` skip
  if its `reports/<task>.md` deliverable contains skip language
  (e.g. “NOT a fittable model”, “(skip)”, “methodology paper”,
  “conference poster … not”); otherwise `extraction`.

|                |  all | extraction | no_extraction | total |
|:---------------|-----:|-----------:|--------------:|------:|
| baseline_48h+  |    0 |          9 |            41 |    50 |
| preopt_pre0622 | 1565 |          0 |             0 |  1565 |
| recent_24h     |    0 |         14 |            92 |   106 |

Task counts by cohort and outcome {.table}

> **Read the confounds before the numbers.** Both cohorts are
> **post-Phase-0**: the register-lookup (`lookup_canonical`) and
> R-log-filter optimizations landed 2026-06-22, so *both* cohorts
> already have them. The cohort difference is therefore mainly the
> **policy self-check**, which targets *sidecar frequency*, not per-task
> tokens. To reach **before** Phase 0 we add the `preopt_pre0622` cohort
> — but only at the **dollar/overall** level (it cannot be split
> extraction-vs-skip), and its task *composition* differs from the
> report-era cohorts, so cross-era comparison is confounded (see the
> dollar-baseline section).

## Results

### Extraction (a model was built)

| metric | cohort | n | mean | sd | median | p25 | p75 |
|:---|:---|---:|:---|:---|:---|:---|:---|
| cost (USD) | recent (post-policy) | 14 | \$10.20 | \$9.06 | \$7.49 | \$1.86 | \$17.69 |
| cost (USD) | baseline (\>48h) | 9 | \$12.37 | \$7.16 | \$14.95 | \$7.72 | \$17.28 |
| total tokens | recent (post-policy) | 14 | 14433k | 14418k | 10003k | 1773k | 24972k |
| total tokens | baseline (\>48h) | 9 | 10427k | 11617k | 1980k | 1793k | 20309k |
| cache-read tokens | recent (post-policy) | 14 | 14117k | 14230k | 9724k | 1660k | 24458k |
| cache-read tokens | baseline (\>48h) | 9 | 10175k | 11392k | 1965k | 1786k | 19815k |
| output tokens | recent (post-policy) | 14 | 62k | 45k | 47k | 23k | 106k |
| output tokens | baseline (\>48h) | 9 | 52k | 60k | 13k | 5k | 104k |

Extraction tasks — mean / sd / median / p25 / p75 (small n; medians
unstable) {.table style="width:100%;"}

### No-extraction (skip)

| metric            | cohort               |   n | mean   | sd     | median | p25    | p75    |
|:------------------|:---------------------|----:|:-------|:-------|:-------|:-------|:-------|
| cost (USD)        | recent (post-policy) |  92 | \$0.88 | \$0.66 | \$0.77 | \$0.62 | \$0.92 |
| cost (USD)        | baseline (\>48h)     |  41 | \$0.77 | \$0.27 | \$0.69 | \$0.61 | \$0.80 |
| total tokens      | recent (post-policy) |  92 | 705k   | 784k   | 566k   | 464k   | 716k   |
| total tokens      | baseline (\>48h)     |  41 | 595k   | 222k   | 526k   | 461k   | 610k   |
| cache-read tokens | recent (post-policy) |  92 | 636k   | 754k   | 505k   | 415k   | 650k   |
| cache-read tokens | baseline (\>48h)     |  41 | 531k   | 207k   | 471k   | 404k   | 551k   |
| output tokens     | recent (post-policy) |  92 | 7k     | 5k     | 5k     | 4k     | 8k     |
| output tokens     | baseline (\>48h)     |  41 | 6k     | 3k     | 5k     | 4k     | 7k     |

No-extraction (skip) tasks — mean / sd / median / p25 / p75 {.table
style="width:100%;"}

### Pre-Phase-0 dollar baseline (before 2026-06-22)

These tasks predate the report convention, so they are reported
**overall** (no extraction/skip split). The estimated dollar cost is the
requested baseline.

| era                   |    n | mean   | sd     | median | p25    | p75    | tok median |
|:----------------------|-----:|:-------|:-------|:-------|:-------|:-------|:-----------|
| pre-Phase-0 (\<06-22) | 1565 | \$6.04 | \$5.78 | \$3.22 | \$1.45 | \$9.66 | 2777k      |
| baseline (06-22..23)  |   50 | \$2.86 | \$5.36 | \$0.73 | \$0.65 | \$1.19 | 549k       |
| recent (post-policy)  |  106 | \$2.11 | \$4.54 | \$0.82 | \$0.63 | \$1.02 | 594k       |

Overall per-task COST (USD) & token median by era — UNCLASSIFIED (mixes
extraction + skip; composition differs across eras) {.table}

> **Composition warning — do not read the overall drop as savings.** The
> eras differ in task *mix*: pre-Phase-0 is extraction-dominated (older
> popPK papers), while the two report-era cohorts are skip-dominated
> (the Metrum/vendor index batch). So the overall fall from a ~\$3
> median to ~\$0.8 is mostly the cheaper skip mix, **not** an
> optimization effect. The only within-class read available —
> pre-Phase-0 overall (≈extraction-dominated, median ~\$3.2) vs the
> report-era *extraction* medians (\$7.5 recent / \$15 baseline) — runs
> the *other* way; but those post-extractions are more complex Metrum
> models (TMDD, survival, multi-analyte), so model difficulty confounds
> that read too. Net: **no clean dollar evidence either way** — the
> cross-era signal is composition, not tooling.

## Interpretation

1.  **Skips — the bulk of both cohorts (133 of 1721 tasks) — are flat.**
    Median cost is ~\$0.7–0.8 and median total tokens ~0.5 M in both
    cohorts (recent slightly *higher*). This is the expected result: the
    skip path (read the paper → decide not-fittable → write a note) is
    **not a target** of the per-task token optimizations, and the policy
    self-check changes *whether a sidecar is filed*, not the cost of
    reaching a skip decision.

2.  **Extractions are too small-n to conclude (23 tasks total).** Mean
    cost is modestly lower in the recent cohort, but the token medians
    move the other way — a signature of small-sample instability, not a
    real effect. The baseline extraction sample (n≈9) is also atypically
    light. No reliable per-task token *reduction* is demonstrated here.

3.  **The dominant driver — cache-read tokens — is essentially
    unchanged** between cohorts. The cached per-turn context has not
    shrunk measurably, which is consistent with point 1: the policy work
    did not touch what the agent carries per turn.

4.  **The realized benefit of the recent work is elsewhere: sidecar
    reduction.** Across the recent cohort the new-sidecar rate is ~1–2%
    of completions (vs a large historical backlog), i.e. far fewer
    stop/restart cycles. That is an operator-time and throughput win,
    **not** a per-task-token win — and this benchmark, which holds
    per-task tokens fixed, is exactly where you would *not* see it.

## Limitations

- **Small extraction n** (recent ≈14, baseline ≈9); medians swing on
  individual tasks. Treat the extraction rows as directional at best.
- **`cost_usd` is partially under-populated** in the run records (cost
  and token medians land on different tasks), so **token counts are the
  firmer metric**; dollar figures are indicative.
- **Cohorts are Metrum/vendor-index-dominated**, which is heavily
  skip-weighted — few extraction samples to show the build-loop
  optimizations in.
- **Report-based classifier**; the baseline is restricted to
  report-bearing (post-2026-06-22) tasks, so it cannot reach pre-Phase-0
  work.
- **Both cohorts are post-Phase-0**, so this isolates the *policy* work,
  not the register-lookup / R-log-filter optimizations.

## Conclusion

In this window the policy optimizations did **not** reduce per-task
tokens — and they were not expected to; their value is fewer sidecars.
Reaching back to a **pre-Phase-0 dollar baseline** (median ≈\$3.2/task
overall, extraction-dominated) does not change the verdict: the
cross-era cost signal is dominated by task *composition*
(extraction-heavy → skip-heavy) and *difficulty* (post-era Metrum models
are more complex), not by tooling, so there is **no clean dollar
evidence of per-task savings** in either direction. **This snapshot is
therefore best used as the baseline** against which future Phase 1–3
work (archetype templates, distillation, budget escalation) should be
A/B-measured — ideally with a per-task `POLICY_SELFCHECK` / optimization
marker so the extraction-vs-skip split and the optimization cohort are
recorded at the source rather than reconstructed.
