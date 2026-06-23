# Soft budget advisor (never a hard kill)

Soft budget advisor (never a hard kill)

## Usage

``` r
budget_advisor(spent, tier = NULL, budget = NULL, soft = 0.8)
```

## Arguments

- spent:

  Output tokens spent so far.

- tier:

  Complexity tier (used if `budget` is `NULL`).

- budget:

  Explicit token budget (overrides `tier`).

- soft:

  Fraction at which to recommend a checkpoint (default 0.8).

## Value

A list: `status` (`ok`/`approaching`/`over`), `action`
(`continue`/`checkpoint`/`checkpoint_and_escalate`), `spent`, `budget`,
`remaining`, `frac`.
