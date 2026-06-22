#!/usr/bin/env Rscript
# lookup.R -- CLI over the nlmixr2lib canonical-name registers.
#
# Returns the few register entries relevant to a term, as compact markdown,
# so the extract-literature-model skill never reads the 1.14 MB covariate
# register (~284k tokens) into context.
#
# Usage:
#   Rscript lookup.R <term> [kind] [top_k]
#     <term>   search term: canonical name, alias, or concept ("body weight")
#     [kind]   covariate | compartment | parameter   (optional; "" = all)
#     [top_k]  max rows (default 5)
#
# Works whether or not the package is installed: falls back to sourcing the
# package R/ sources from the script's source tree.

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || !nzchar(args[[1]])) {
  cat("usage: Rscript lookup.R <term> [kind] [top_k]\n")
  quit(status = 2)
}
term  <- args[[1]]
kind  <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else NULL
top_k <- if (length(args) >= 3 && nzchar(args[[3]])) as.integer(args[[3]]) else 5L

have_pkg <- requireNamespace("nlmixr2libingest", quietly = TRUE)
if (have_pkg) {
  res <- nlmixr2libingest::lookup_canonical(term, kind = kind, top_k = top_k)
  nlmixr2libingest::render_lookup(res)
} else {
  # Source the package functions directly from the source tree.
  this <- tryCatch(normalizePath(sub("^--file=", "",
            grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
            error = function(e) NA_character_)
  root <- if (!is.na(this)) normalizePath(file.path(dirname(this), "..", ".."))
          else normalizePath(".")
  for (f in c("utils.R", "registers.R", "lookup.R")) {
    source(file.path(root, "R", f))
  }
  res <- lookup_canonical(term, kind = kind, top_k = top_k)
  render_lookup(res)
}
