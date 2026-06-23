#!/usr/bin/env Rscript
# prebrief.R -- CLI: a model-specific naming pre-brief in compact markdown, so
# the skill resolves a paper's covariates/parameters/compartments to canonical
# names ONCE instead of loading the register or looking terms up turn-by-turn.
#
# Usage:
#   Rscript prebrief.R <paper> [kinds]
#     <paper>   paper text file (the trimmed paper)
#     [kinds]   comma-separated: covariate,parameter,compartment
#               (default: covariate)
#
# Uses the local LLM to widen candidate recall when one is configured; with no
# LLM it falls back to a deterministic register scan. Either way the result is a
# PRIOR -- the agent still source-traces every value against the paper.

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || !nzchar(args[[1L]])) {
  cat("usage: Rscript prebrief.R <paper> [kinds]\n")
  quit(status = 2)
}
if (!requireNamespace("nlmixr2libingest", quietly = TRUE)) {
  cat("error: install the nlmixr2libingest package first.\n")
  quit(status = 2)
}
paper <- args[[1L]]
kinds <- if (length(args) >= 2L && nzchar(args[[2L]])) {
  trimws(strsplit(args[[2L]], ",", fixed = TRUE)[[1L]])
} else "covariate"

res <- nlmixr2libingest::naming_prebrief(paper, kinds = kinds)
nlmixr2libingest::render_prebrief(res)
