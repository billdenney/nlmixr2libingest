#!/usr/bin/env Rscript
# prebrief.R -- batch-resolve a list of paper terms to canonical nlmixr2lib
# names in ONE call (one index load, one turn), instead of a lookup-per-term.
#
# The agent identifies the covariates (and any non-obvious parameter /
# compartment names) while reading the paper, then resolves them all at once.
# This avoids reading the ~284k-token covariate register AND avoids a separate
# lookup turn per name -- the saving grows with the covariate count.
#
# Usage:
#   Rscript prebrief.R <kind> <term> [<term> ...]
#     <kind>  covariate | parameter | compartment
#     <term>  the paper's name/phrase for each item (quote multi-word phrases)
#
# Prints a compact "term -> CANONICAL [units, scope]" brief; an unresolved term
# is flagged UNMATCHED (a possible new canonical -> stop-and-ask). PRIOR ONLY:
# source-trace every value against the paper.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || !nzchar(args[[1L]])) {
  cat("usage: Rscript prebrief.R <kind> <term> [<term> ...]\n")
  quit(status = 2L)
}
if (!requireNamespace("nlmixr2libingest", quietly = TRUE)) {
  cat("error: install the nlmixr2libingest package first.\n")
  quit(status = 2L)
}
kind  <- args[[1L]]
terms <- args[-1L]

cat(sprintf("# Naming pre-brief: %d %s term(s) (prior only -- source-trace every value)\n",
            length(terms), kind))
for (t in terms) {
  r <- tryCatch(nlmixr2libingest::lookup_canonical(t, kind = kind, top_k = 1L),
                error = function(e) NULL)
  if (is.null(r) || !nrow(r)) {
    cat(sprintf("- %s -> UNMATCHED (possible new canonical -> stop-and-ask)\n", t))
    next
  }
  tag <- paste(stats::na.omit(c(r$units[1L], r$scope[1L])), collapse = ", ")
  tag <- if (nzchar(tag)) paste0("  [", substr(tag, 1L, 90L), "]") else ""
  arrow <- if (tolower(t) == tolower(r$name[1L])) r$name[1L]
           else sprintf("%s -> %s", t, r$name[1L])
  cat(sprintf("- %s%s\n", arrow, tag))
}
