# Token / cost analytics over the runner's RunRecords

Parses the task-runner state YAMLs (one per task, each with a `runs`
list carrying `usage` and `cost_usd`) into a tidy per-run data frame, so
the measured cost economics (output and cache-read tokens, \$/run) can
be tracked.

## Usage

``` r
run_token_stats(state_dir)
```

## Arguments

- state_dir:

  Directory of `<task_id>.yaml` state files (e.g. the runner's
  `.claude_task_runner/state`).

## Value

A data frame: `task`, `attempt`, `output`, `input`, `cache_read`,
`cost`. Its `summary` attribute holds median/mean/max per numeric
column.
