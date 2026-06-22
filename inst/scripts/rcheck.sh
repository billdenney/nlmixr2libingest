#!/usr/bin/env bash
# rcheck.sh -- run a command, tee its FULL output to a log file, and print only
# the filtered ERROR/WARNING/NOTE summary (via nlmixr2libingest::filter_check_log).
# The dominant ingestion cost is re-reading multi-thousand-line check logs every
# turn; this keeps Claude's context to the lines that matter while preserving the
# full log on disk for drill-down.
#
# Usage:
#   rcheck.sh [--render] [--no-notes] -- <command> [args...]
#   rcheck.sh devtools::check ...      # '--' optional when no flags
#
# Env:
#   RCHECK_LOGDIR   directory for the full log (default: a mktemp dir)
#
# Works whether or not nlmixr2libingest is installed: falls back to sourcing
# R/rfilter.R from the package source tree (so it can filter the package's own
# `devtools::check()` before the package is installed).
set -uo pipefail

MODE="check"
KEEP_NOTES="TRUE"
while [[ $# -gt 0 ]]; do
  case "$1" in
    --render)   MODE="render"; shift ;;
    --no-notes) KEEP_NOTES="FALSE"; shift ;;
    --)         shift; break ;;
    *)          break ;;
  esac
done

if [[ $# -eq 0 ]]; then
  echo "rcheck.sh: no command given" >&2
  exit 2
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# inst/scripts -> package root is two levels up in the source tree.
PKG_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RFILTER_SRC="$PKG_ROOT/R/rfilter.R"

LOGDIR="${RCHECK_LOGDIR:-$(mktemp -d)}"
mkdir -p "$LOGDIR"
LOG="$LOGDIR/rcheck-$$.log"

"$@" >"$LOG" 2>&1
status=$?

FN="filter_check_log"
[[ "$MODE" == "render" ]] && FN="filter_render_log"

RFILTER_SRC="$RFILTER_SRC" Rscript --vanilla -e '
  args <- commandArgs(TRUE)
  fn <- args[[1]]; logp <- args[[2]]; keep <- as.logical(args[[3]])
  f <- tryCatch(get(fn, envir = asNamespace("nlmixr2libingest")),
                error = function(e) NULL)
  if (is.null(f)) {
    src <- Sys.getenv("RFILTER_SRC")
    if (nzchar(src) && file.exists(src)) { source(src); f <- get(fn) }
  }
  if (is.null(f)) { writeLines(readLines(logp)); quit(status = 0) }
  if (identical(fn, "filter_check_log")) f(logp, keep_notes = keep) else f(logp)
' "$FN" "$LOG" "$KEEP_NOTES"

echo ""
echo "[rcheck] exit=$status | full log: $LOG"
exit $status
