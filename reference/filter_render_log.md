# Filter an rmarkdown / knitr render log to the failure and its context

Vignette renders fail with a `Quitting from lines ...` / `Error ...` /
`Execution halted` cluster. This returns a window around each such
marker so the failing chunk is visible without the full render
transcript.

## Usage

``` r
filter_render_log(x, context = 20L)
```

## Arguments

- x:

  A path to a log file, or the log text.

- context:

  Lines of context to keep on each side of a failure marker.

## Value

Invisibly, the filtered lines (character). Printed by default.
