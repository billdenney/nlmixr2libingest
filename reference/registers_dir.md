# Locate the nlmixr2lib canonical-name register directory

Resolves, in order of preference: the `nlmixr2libingest.refs_dir`
option, the installed `nlmixr2lib` package
(`system.file("references", ...)`), then a development checkout under
the parent of this package. The directory is the authority that
[`lookup_canonical()`](lookup_canonical.md) indexes; it is owned by
`nlmixr2lib`, never by this package.

## Usage

``` r
registers_dir()
```

## Value

Absolute path to the references directory, or `""` if none found.
