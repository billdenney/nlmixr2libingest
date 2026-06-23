#!/usr/bin/env Rscript
# lint_vignette.R -- CLI: static, pre-render lint of a validation vignette for
# the common render-killers (oversized cohort, missing cmt= for an algebraic
# observable, named-vector amt=, PKNCA time=0-dropping filter). Catches them
# BEFORE the expensive render. Exit: 0 = clean, 1 = issues, 2 = usage error.
#
# Usage:
#   Rscript lint_vignette.R <vignette.Rmd> [model] [--max-per-arm N]
#     <vignette.Rmd>   path to the validation vignette
#     [model]          nlmixr2lib model name or path to its .R (enables the
#                      algebraic-observable cmt= check)
#     --max-per-arm N  per-arm simulation cap (default 200)

args <- commandArgs(trailingOnly = TRUE)
optval <- function(name, default = NULL) {
  i <- which(args == name)
  if (length(i) && i[[1L]] < length(args)) args[[i[[1L]] + 1L]] else default
}
pos <- args[!grepl("^--", args)]
pos <- setdiff(pos, optval("--max-per-arm"))
if (!length(pos) || !nzchar(pos[[1L]]) || !file.exists(pos[[1L]])) {
  cat("usage: Rscript lint_vignette.R <vignette.Rmd> [model] [--max-per-arm N]\n")
  quit(status = 2)
}
if (!requireNamespace("nlmixr2libingest", quietly = TRUE)) {
  cat("error: install the nlmixr2libingest package first.\n")
  quit(status = 2)
}
rmd   <- pos[[1L]]
model <- if (length(pos) >= 2L && nzchar(pos[[2L]])) pos[[2L]] else NULL
if (!is.null(model) && file.exists(model)) {
  e <- new.env(); sys.source(model, envir = e)
  fns <- Filter(is.function, mget(ls(e), envir = e))
  if (length(fns)) model <- fns[[1L]]
}
mpa <- suppressWarnings(as.integer(optval("--max-per-arm", "200")))
if (is.na(mpa)) mpa <- 200L

res <- nlmixr2libingest::lint_vignette(rmd, model = model, max_per_arm = mpa)
print(res)
quit(status = if (res$n == 0L) 0L else 1L)
