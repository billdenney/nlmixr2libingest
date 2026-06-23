#!/usr/bin/env Rscript
# validate.R -- CLI: run the whole model+vignette validation chain and print ONE
# terse result ("Success" or a short fix-list) instead of multi-thousand-line
# logs. Exit status: 0 = success, 1 = issues found, 2 = usage error.
#
# Usage:
#   Rscript validate.R <model> [paper] [--full] [--pkg DIR] [--vignette FILE]
#     <model>        nlmixr2lib model name, or path to a model .R file
#     [paper]        paper text file (enables the source-trace stage)
#     --full         also run R CMD check (needs --pkg) and vignette render
#     --pkg DIR      package dir for the full-tier check (default: cwd)
#     --vignette F   vignette .Rmd to render-check in the full tier

args <- commandArgs(trailingOnly = TRUE)
flag <- function(name) name %in% args
optval <- function(name, default = NULL) {
  i <- which(args == name)
  if (length(i) && i[[1L]] < length(args)) args[[i[[1L]] + 1L]] else default
}
pos <- args[!grepl("^--", args)]
# Drop values that belong to --pkg/--vignette from the positional list.
pos <- setdiff(pos, c(optval("--pkg"), optval("--vignette")))

if (!length(pos) || !nzchar(pos[[1L]])) {
  cat("usage: Rscript validate.R <model> [paper] [--full] [--pkg DIR] [--vignette FILE]\n")
  quit(status = 2)
}
if (!requireNamespace("nlmixr2libingest", quietly = TRUE)) {
  cat("error: install the nlmixr2libingest package first.\n")
  quit(status = 2)
}

model    <- pos[[1L]]
# A path to a model file is read into a function via source(); a bare name is
# passed through and resolved against nlmixr2lib::modeldb.
if (file.exists(model)) {
  e <- new.env(); sys.source(model, envir = e)
  fns <- Filter(is.function, mget(ls(e), envir = e))
  if (length(fns)) model <- fns[[1L]]
}
paper    <- if (length(pos) >= 2L && nzchar(pos[[2L]])) pos[[2L]] else NULL
level    <- if (flag("--full")) "full" else "fast"
pkg      <- optval("--pkg", ".")
vignette <- optval("--vignette")

res <- nlmixr2libingest::validate_model(
  model, paper = paper, level = level,
  pkg = if (level == "full") pkg else NULL, vignette = vignette)
print(res)
quit(status = if (identical(res$status, "success")) 0L else 1L)
