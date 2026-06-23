# Is a local embedding backend available?

Returns `TRUE` only if a backend is selected (not `"none"`), `httr2` is
installed, and the ollama server answers. Used to choose embedding vs
keyword classification automatically.

## Usage

``` r
embeddings_available()
```

## Value

Logical scalar.
