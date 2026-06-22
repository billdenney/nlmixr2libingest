# Generate a starting template for an archetype

Returns a structured template recommendation: the most representative
existing model to adapt (always useful), plus – for plain PK archetypes
– the canonical `PK_<n>cmt` base and the `add*`/`convert*` pipe recipe
to reach the target. For `other:*` / atypical archetypes it returns an
exemplar pointer with a note to adapt manually rather than
auto-template.

## Usage

``` r
archetype_template(archetype, ft = NULL)
```

## Arguments

- archetype:

  An archetype label (from [`assign_archetype()`](assign_archetype.md)).

- ft:

  A feature table (to pick the exemplar). If `NULL`, exemplar is
  omitted.

## Value

A list with `archetype`, `strategy` (`"piped"`/`"exemplar"`), `base`,
`steps`, `exemplar`, `n_examples`, and `note`.
