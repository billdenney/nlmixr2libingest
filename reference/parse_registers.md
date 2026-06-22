# Parse the nlmixr2lib canonical-name registers into a tidy data frame

Reads the covariate, compartment, and parameter registers shipped by
`nlmixr2lib` and returns one row per canonical entry. This is the
deterministic substrate for
[`build_register_index()`](build_register_index.md) /
[`lookup_canonical()`](lookup_canonical.md); it does no validation (that
stays in `nlmixr2lib::checkModelConventions()`).

## Usage

``` r
parse_registers(dir = registers_dir())
```

## Arguments

- dir:

  Directory holding the register markdown files. Defaults to
  [`registers_dir()`](registers_dir.md).

## Value

A data frame with columns `kind`, `name`, `description`, `units`,
`type`, `scope`, `reference_category`, `role`, `source_aliases`,
`example_models`, `notes`, plus `text` (FTS document) and `id`.
