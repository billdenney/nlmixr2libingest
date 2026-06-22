# Build (or rebuild) the canonical-name register index

Parses the three `nlmixr2lib` registers into a DuckDB file with a
full-text index plus a source signature (size, mtime, md5) used for
freshness checks. If the DuckDB `fts` extension cannot be loaded (e.g.
offline), the index is still built and lookups fall back to token `LIKE`
matching.

## Usage

``` r
build_register_index(
  db_path = register_db_path(),
  dir = registers_dir(),
  quiet = FALSE
)
```

## Arguments

- db_path:

  DuckDB file to write. Defaults to
  [`register_db_path()`](register_db_path.md).

- dir:

  Register directory. Defaults to [`registers_dir()`](registers_dir.md).

- quiet:

  Suppress the FTS-unavailable warning.

## Value

Invisibly, the parsed register data frame.
