# Filter an R CMD check / devtools::check log to the parts that matter

Keeps every check step that is not `OK` (NOTE/WARNING/ERROR), in full,
together with its indented detail block, plus the run-status summary and
any hard build/install failure lines. Drops the `... OK` steps. Nothing
that signals a problem is removed.

## Usage

``` r
filter_check_log(x, keep_notes = TRUE)
```

## Arguments

- x:

  A path to a log file, or the log text.

- keep_notes:

  Keep `NOTE`-level steps (default `TRUE`).

## Value

Invisibly, the filtered lines (character). Printed by default.
