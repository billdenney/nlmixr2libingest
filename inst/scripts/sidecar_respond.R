#!/usr/bin/env Rscript
# sidecar_respond.R -- auto-answer a runner sidecar request from the vetted
# policy table, or escalate to a human. Intended to be called by the runner's
# sidecar watcher once per new request, on the next tick, so a policy-decidable
# pause continues the run on a warm cache instead of a cold retry.
#
# Exit status: 0 = auto-answered (response written), 10 = escalate (left for a
# human; nothing written), 2 = usage error.
#
# Usage:
#   Rscript sidecar_respond.R <request.json> [--apply] [--response FILE] [--log FILE]
#     --apply        actually write the response/log (default: dry run, decide only)
#     --response F   response path (default: request-NNN.json -> response-NNN.json)
#     --log F        append an audit-log line for each auto-answer

args <- commandArgs(trailingOnly = TRUE)
optval <- function(name, default = NULL) {
  i <- which(args == name)
  if (length(i) && i[[1L]] < length(args)) args[[i[[1L]] + 1L]] else default
}
pos <- args[!grepl("^--", args)]
pos <- setdiff(pos, c(optval("--response"), optval("--log")))
if (!length(pos) || !nzchar(pos[[1L]]) || !file.exists(pos[[1L]])) {
  cat("usage: Rscript sidecar_respond.R <request.json> [--apply] [--response FILE] [--log FILE]\n")
  quit(status = 2)
}
if (!requireNamespace("nlmixr2libingest", quietly = TRUE)) {
  cat("error: install the nlmixr2libingest package first.\n")
  quit(status = 2)
}
req_file <- pos[[1L]]
apply <- "--apply" %in% args
response <- optval("--response",
  file.path(dirname(req_file), sub("request", "response", basename(req_file))))
log_file <- optval("--log")

dec <- nlmixr2libingest::sidecar_respond(
  req_file, response_file = response, apply = apply, log_file = log_file)
print(dec)
quit(status = if (isTRUE(dec$auto)) 0L else 10L)
