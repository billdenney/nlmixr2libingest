# Auto-answer a sidecar request when policy permits

Matches a request against the policy table; if EVERY question is
auto-answerable, optionally writes the runner's response file (selecting
only options the agent was offered) and an audit-log line. Otherwise it
writes nothing and the request escalates to a human.

## Usage

``` r
sidecar_respond(
  request,
  response_file = NULL,
  policy = sidecar_policy(),
  apply = FALSE,
  log_file = NULL
)
```

## Arguments

- request:

  A request file path, or a parsed request list.

- response_file:

  Where to write the response JSON when `apply = TRUE`.

- policy:

  A policy table from [`sidecar_policy()`](sidecar_policy.md).

- apply:

  Actually write the response/log (default `FALSE` = dry run).

- log_file:

  Append an audit record here when an answer is written.

## Value

The `nli_sidecar_decision` (with `request_file` and `applied`).
