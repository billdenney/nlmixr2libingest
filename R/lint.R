# Static, pre-render vignette linter.
#
# Rendering is the single most expensive / most-iterated operation in an
# extraction (each failed render adds a turn and a large render log to the
# re-read-every-turn context). This linter catches the most common, regex- and
# rxUi-detectable render-killers WITHOUT a render round-trip, so they are fixed
# before the first (expensive) render rather than discovered by it. It is a
# best-effort PRE-check -- it does not replace the render gate, and a clean lint
# does not guarantee a clean render.

# The R code of each ```{r ...} chunk of an Rmd, one character vector per
# chunk (chunks are parsed separately so one that does not parse cannot hide
# the others).
.lintRchunks <- function(rmd) {
  lines <- readLines(rmd, warn = FALSE)
  inchunk <- FALSE
  chunks <- list()
  for (ln in lines) {
    if (grepl("^```\\{[rR]", ln)) {
      inchunk <- TRUE
      chunks[[length(chunks) + 1L]] <- character()
      next
    }
    if (inchunk && grepl("^```\\s*$", ln)) { inchunk <- FALSE; next }
    if (inchunk) {
      i <- length(chunks)
      chunks[[i]] <- c(chunks[[i]], ln)
    }
  }
  chunks
}

# Extract the R code from ```{r ...} chunks of an Rmd.
.lintRcode <- function(rmd) {
  as.character(unlist(.lintRchunks(rmd), use.names = FALSE))
}

# Algebraic observables: prediction variables that are not ODE compartment
# states (e.g. `Cc <- central / vc; Cc ~ prop(.)`). Best-effort via rxUi.
.lintObservables <- function(model) {
  if (is.null(model)) return(character())
  ui <- tryCatch(.stAsUi(model), error = function(e) NULL)
  if (is.null(ui)) return(character())
  tryCatch({
    pd <- ui$predDf
    nm <- if (is.data.frame(pd) && "var" %in% names(pd)) pd$var
          else if (is.data.frame(pd) && "cond" %in% names(pd)) pd$cond
          else character()
    st <- tryCatch(as.character(ui$state), error = function(e) character())
    unique(setdiff(as.character(nm), st))
  }, error = function(e) character())
}

.lintIssue <- function(check, severity, message, fix) {
  data.frame(check = check, severity = severity, message = message, fix = fix,
             stringsAsFactors = FALSE)
}

# --- lagged-dose f() covariates ------------------------------------------------
#
# A dose delayed by alag() is applied when it ARRIVES, so rxode2 evaluates its
# f() at that later time. By default (addlKeepsCov = FALSE) the covariates f()
# reads are therefore the ones in effect at the arrival time -- typically a
# pre-dose sample at the dose time, or a record inside the lag window, that
# still carries the previous occasion's covariate value. NONMEM instead uses the
# covariates on the dose record (addlKeepsCov = TRUE, which nonmem = TRUE also
# sets). The two give different bioavailability whenever f() depends on a
# covariate that changes between occasions, so the article has to choose.

# Record every assignment of a model() block into `acc`:
#   acc$deps    named list, variable -> the names its right-hand sides read,
#               over ALL of its assignments, plus the names of every enclosing
#               if() condition (which decide whether the assignment happens);
#   acc$special one entry per f() / alag() statement: the function ("f" or
#               "alag") and the names its right-hand side reads, likewise.
# `guard` carries the enclosing if() conditions' names down the recursion.
# lstExpr keeps the source spellings F() and lag(), so they are normalised here.
.lintCollectAssignments <- function(x, guard, acc) {
  if (!is.call(x)) return(invisible(NULL))
  op <- if (is.symbol(x[[1L]])) as.character(x[[1L]]) else ""
  if (op == "{") {
    for (el in as.list(x)[-1L]) .lintCollectAssignments(el, guard, acc)
  } else if (op == "if") {
    guard <- c(guard, all.vars(x[[2L]]))
    for (el in as.list(x)[-(1:2)]) .lintCollectAssignments(el, guard, acc)
  } else if (op %in% c("<-", "=") && length(x) == 3L) {
    lhs <- x[[2L]]
    reads <- c(all.vars(x[[3L]]), guard)
    if (is.symbol(lhs)) {
      nm <- as.character(lhs)
      acc$deps[[nm]] <- union(acc$deps[[nm]], reads)
    } else if (is.call(lhs) && is.symbol(lhs[[1L]]) && length(lhs) == 2L) {
      fn <- switch(as.character(lhs[[1L]]), f = , F = "f", alag = , lag = "alag",
                   NA_character_)
      if (!is.na(fn)) {
        acc$special[[length(acc$special) + 1L]] <- list(fn = fn, reads = reads)
      }
    }
  }
  invisible(NULL)
}

# Every name `roots` can depend on, directly or through `deps` (a fixed point,
# so a cycle between variables terminates).
.lintReachable <- function(roots, deps) {
  seen <- character()
  todo <- roots
  while (length(todo)) {
    nm <- todo[[1L]]
    todo <- todo[-1L]
    if (nm %in% seen) next
    seen <- c(seen, nm)
    todo <- c(todo, deps[[nm]])
  }
  seen
}

# The covariates a model supplies: its covariateData entries plus whatever
# rxode2 itself treats as a data-supplied covariate, so a model whose
# documentation list is incomplete is still covered.
.lintModelCovariates <- function(ui) {
  documented <- tryCatch(names(ui$meta$covariateData), error = function(e) NULL)
  detected <- tryCatch(ui$allCovs, error = function(e) NULL)
  union(as.character(documented), as.character(detected))
}

# Covariates the model's f() statements depend on, when the model also has an
# alag() statement; character(0) otherwise. Dependence is traced through
# intermediate assignments and if() conditions (see .lintCollectAssignments).
.lintLaggedFCovariates <- function(ui) {
  acc <- new.env(parent = emptyenv())
  acc$deps <- list()
  acc$special <- list()
  for (e in ui$lstExpr) .lintCollectAssignments(e, character(), acc)
  fns <- vapply(acc$special, function(s) s$fn, character(1L))
  if (!"alag" %in% fns) return(character())
  covs <- .lintModelCovariates(ui)
  hit <- character()
  for (s in acc$special[fns == "f"]) {
    hit <- c(hit, intersect(.lintReachable(s$reads, acc$deps), covs))
  }
  sort(unique(hit))
}

# The function a call invokes, by name: `f(...)`, `pkg::f(...)` and
# `pkg:::f(...)` all give "f"; an anonymous function gives "".
.lintIsNsCall <- function(x) {
  is.call(x) && length(x) == 3L && is.symbol(x[[1L]]) &&
    as.character(x[[1L]]) %in% c("::", ":::")
}
.lintCallName <- function(x) {
  h <- x[[1L]]
  if (is.symbol(h)) as.character(h)
  else if (.lintIsNsCall(h)) as.character(h[[3L]])
  else ""
}

# TRUE when the call (or a call nested in its arguments, as in
# `control = rxControl(addlKeepsCov = TRUE)`) names addlKeepsCov, or passes
# nonmem = TRUE. A bare `nonmem = flag` is not accepted: only the literal shows
# that the dose-record covariates are in force.
.lintCallStates <- function(x) {
  if (!is.call(x)) return(FALSE)
  parts <- as.list(x)
  nms <- names(parts)
  if (is.null(nms)) nms <- rep("", length(parts))
  if (any(nms == "addlKeepsCov")) return(TRUE)
  is_true <- function(v) isTRUE(v) || identical(v, quote(T))
  if (any(vapply(parts[nms == "nonmem"], is_true, logical(1L)))) return(TRUE)
  any(vapply(parts, .lintCallStates, logical(1L)))
}

# One-line, length-capped rendering of a call (or a line of text) for a message.
.lintTrunc <- function(s, width = 70L) {
  s <- gsub("\\s+", " ", s)
  if (nchar(s) > width) paste0(substr(s, 1L, width - 3L), "...") else s
}
.lintCallText <- function(x, width = 70L) {
  .lintTrunc(paste(deparse(x, width.cutoff = 500L), collapse = " "), width)
}

# --- telling a model solve from base::solve() ---------------------------------
#
# rxode2 gives solve() a method for models (solve(mod, events), the same
# computation as rxSolve()), but base::solve() on a matrix is the commonest use
# of the name, so a solve() call counts as a simulation only when an argument
# traces to a model. Names are tracked through the article's assignments, in
# document order, in acc$modelnames (names known to hold a model) and
# acc$eventnames (names known to hold an event table).

# Functions that return a model whatever their arguments, functions that return
# their first argument's model modified, and the event-table builders.
.lintModelMakers <- c("readModelDb", "rxode2", "rxode")
.lintModelWrappers <- c("model", "ini", "zeroRe", "rxFixPop", "rxRmIni",
                        "rxAppendModel", "suppressMessages", "suppressWarnings",
                        "invisible", "(")
.lintEventMakers <- c("et", "eventTable", "etRep", "etSeq", "etExpand",
                      "add.dosing", "add.sampling")
# Arguments of rxSolve() that base::solve() (a, b, tol) does not have.
.lintSolveArgs <- c("events", "params", "inits", "nSub", "nsub", "nStud", "nstud",
                    "omega", "sigma", "thetaMat", "dfSub", "dfObs", "atol",
                    "rtol", "keep", "cores", "seed", "returnType",
                    "addlKeepsCov", "nonmem")

# `lhs %>% f(args)` is `f(lhs, args)`, or `f(args)` with lhs where a bare `.` is
# an argument; magrittr pipes are rewritten so they resolve like the native pipe
# the parser has already rewritten.
.lintIsPipe <- function(x) {
  is.call(x) && length(x) == 3L && identical(x[[1L]], quote(`%>%`))
}
.lintDePipe <- function(x) {
  lhs <- x[[2L]]
  rhs <- x[[3L]]
  if (!is.call(rhs)) return(as.call(list(rhs, lhs)))
  args <- as.list(rhs)[-1L]
  placeholder <- vapply(args, identical, logical(1L), quote(.))
  args <- if (any(placeholder)) replace(args, placeholder, list(lhs)) else c(list(lhs), args)
  as.call(c(list(rhs[[1L]]), args))
}

# TRUE when `x` evaluates to a model: a name or string already known to hold
# one (including a model's literal library name), a readModelDb()/rxode2()/
# rxode() call, or a model()/ini()/zeroRe()/... call on one.
.lintIsModel <- function(x, acc) {
  if (.lintIsPipe(x)) x <- .lintDePipe(x)
  if (is.symbol(x) || is.character(x)) {
    return(length(x) == 1L && as.character(x) %in% acc$modelnames)
  }
  if (!is.call(x)) return(FALSE)
  nm <- .lintCallName(x)
  if (nm %in% .lintModelMakers) return(TRUE)
  # an event table built on a model, et(mod, ...), still carries the model
  if ((nm %in% .lintModelWrappers || nm %in% .lintEventMakers) && length(x) >= 2L) {
    return(.lintIsModel(x[[2L]], acc))
  }
  FALSE
}

# TRUE when `x` visibly builds or holds an event table: an et()-family call, a
# dosing-record column (amt = / evid =), or a name already known to hold one
# (which covers a user function whose body builds event rows).
.lintMentionsEvents <- function(x, acc) {
  if (.lintIsPipe(x)) x <- .lintDePipe(x)
  if (is.symbol(x)) return(as.character(x) %in% acc$eventnames)
  if (!is.call(x)) return(FALSE)
  parts <- as.list(x)
  nms <- names(parts)
  if (is.null(nms)) nms <- rep("", length(parts))
  if (.lintCallName(x) %in% .lintEventMakers || any(nms %in% c("amt", "evid"))) {
    return(TRUE)
  }
  any(vapply(parts, .lintMentionsEvents, logical(1L), acc = acc))
}

# Classify a solve() call: "rxSolve" when an argument is a model (so it is a
# simulation whose arguments can be read like rxSolve()'s); "solve-unresolved"
# when no argument is a model but the call has event-table-like arguments or
# rxSolve-only arguments (a simulation we cannot trace, never to be passed
# silently); NA when it looks like base::solve() on a matrix.
.lintSolveKind <- function(x, acc) {
  args <- as.list(x)[-1L]
  if (any(vapply(args, .lintIsModel, logical(1L), acc = acc))) return("rxSolve")
  if (any(vapply(args, .lintMentionsEvents, logical(1L), acc = acc)) ||
      any(names(args) %in% .lintSolveArgs)) {
    return("solve-unresolved")
  }
  NA_character_
}

# Note what a name is assigned from, for later solve() calls.
.lintRecordAssignment <- function(name, rhs, acc) {
  if (.lintIsModel(rhs, acc)) {
    acc$modelnames <- union(acc$modelnames, name)
  } else if (.lintMentionsEvents(rhs, acc)) {
    acc$eventnames <- union(acc$eventnames, name)
  }
  invisible(NULL)
}

# Walk parsed article code and record into `acc`:
#   acc$sites   every place the model is solved: kind "rxSolve" (a call to
#               rxSolve, or to solve() on a model, however spelled or piped --
#               the native pipe is already rewritten to a plain call by the
#               parser), "est-rxSolve" (nlmixr2(..., est = "rxSolve")),
#               "simulate" (simulate()), "solve-unresolved" (solve() that looks
#               like a simulation but whose model cannot be traced), or
#               "indirect" (rxSolve handed to another function, as in
#               do.call(rxSolve, args)); `stated` says whether the call itself
#               names the setting;
#   acc$models  the literal names given to readModelDb();
#   acc$modelnames / acc$eventnames  names known to hold a model / event table
#               (see .lintIsModel()), grown by assignments as the walk goes.
# Only a rxSolve() or model solve() call can be checked argument by argument;
# the others pass their arguments on in ways static code cannot follow.
.lintWalkSolves <- function(x, acc) {
  if (is.pairlist(x)) {
    lapply(as.list(x), .lintWalkSolves, acc = acc)
    return(invisible(NULL))
  }
  if (is.symbol(x)) {
    if (identical(as.character(x), "rxSolve")) {
      acc$sites[[length(acc$sites) + 1L]] <- list(
        kind = "indirect", stated = FALSE, text = "rxSolve passed as a function")
    }
    return(invisible(NULL))
  }
  if (!is.call(x)) return(invisible(NULL))
  if (.lintIsPipe(x)) x <- .lintDePipe(x)
  nm <- .lintCallName(x)
  parts <- as.list(x)
  if (nm %in% c("<-", "=") && length(parts) == 3L && is.symbol(parts[[2L]])) {
    .lintRecordAssignment(as.character(parts[[2L]]), parts[[3L]], acc)
  }
  is_rxsolve_chr <- vapply(parts[-1L], identical, logical(1L), "rxSolve")
  kind <- if (identical(nm, "rxSolve")) "rxSolve"
          else if (identical(nm, "solve")) .lintSolveKind(x, acc)
          else if (nm %in% c("nlmixr2", "nlmixr") && any(is_rxsolve_chr)) "est-rxSolve"
          else if (identical(nm, "simulate")) "simulate"
          else if (nm %in% c("do.call", "match.fun", "get", "getExportedValue") &&
                   any(is_rxsolve_chr)) "indirect"
          else NA_character_
  if (!is.na(kind)) {
    acc$sites[[length(acc$sites) + 1L]] <- list(
      kind = kind,
      stated = kind != "indirect" && .lintCallStates(x),
      text = .lintCallText(x))
  }
  if (identical(nm, "readModelDb") && length(parts) >= 2L &&
      is.character(parts[[2L]])) {
    acc$models <- c(acc$models, parts[[2L]])
    acc$modelnames <- union(acc$modelnames, parts[[2L]])
  }
  # The function position is not walked when it is a plain name or pkg::name:
  # the name itself is not a reference to rxSolve as a value.
  head_is_name <- is.symbol(parts[[1L]]) || .lintIsNsCall(parts[[1L]])
  lapply(if (head_is_name) parts[-1L] else parts, .lintWalkSolves, acc = acc)
  invisible(NULL)
}

# Scan the chunks for the places the model is solved. A chunk that does not
# parse but mentions a solver is recorded in acc$unparsed rather than skipped,
# because a solve that cannot be read cannot be shown to state the setting.
.lintScanSolves <- function(chunks, known = character()) {
  acc <- new.env(parent = emptyenv())
  acc$sites <- list()
  acc$models <- character()
  acc$modelnames <- known
  acc$eventnames <- character()
  acc$unparsed <- character()
  for (ch in chunks) {
    txt <- paste(ch, collapse = "\n")
    ex <- tryCatch(parse(text = txt, keep.source = FALSE), error = function(e) NULL)
    if (is.null(ex)) {
      hit <- grep("rxSolve|simulate\\s*\\(|nlmixr2?\\s*\\(", ch, value = TRUE)
      if (length(hit)) acc$unparsed <- c(acc$unparsed, .lintTrunc(hit[[1L]], 60L))
    } else {
      lapply(as.list(ex), .lintWalkSolves, acc = acc)
    }
  }
  acc$models <- unique(acc$models)
  list(sites = acc$sites, models = acc$models, unparsed = acc$unparsed)
}

# The rule: when the model has alag() and an f() that depends on a covariate,
# every place the article solves the model must state addlKeepsCov or
# nonmem = TRUE. Returns a list of .lintIssue() rows.
#
# The model is the `model` argument when given (a name, path-loaded function or
# rxUi, exactly as the other model checks take it), else every literal name the
# article passes to readModelDb(). With neither, no model is known and the rule
# cannot apply. A model that is named but cannot be parsed is reported, never
# skipped: a silent skip is how an unchecked article looks like a checked one.
.lintLaggedDose <- function(chunks, model) {
  known <- if (is.character(model) && length(model) == 1L) model else character()
  scan <- .lintScanSolves(chunks, known)
  if (!length(scan$sites) && !length(scan$unparsed)) return(list())
  srcs <- if (is.null(model)) as.list(scan$models) else list(model)
  if (!length(srcs)) return(list())

  iss <- list()
  covs <- character()
  for (src in srcs) {
    ui <- tryCatch(.stAsUi(src), error = function(e) NULL)
    if (is.null(ui)) {
      label <- if (is.character(src)) paste0("`", src, "`") else "the supplied model"
      iss[[length(iss) + 1L]] <- .lintIssue(
        "addlkeepscov-model", "warning",
        paste0("cannot parse ", label, ", so whether the article's simulations ",
               "need addlKeepsCov (a model with alag() and a covariate-dependent ",
               "f()) was not checked"),
        "install rxode2 and nlmixr2lib, or pass the model's .R path as the second argument, so the model can be read")
    } else {
      covs <- union(covs, .lintLaggedFCovariates(ui))
    }
  }
  if (!length(covs)) return(iss)

  what <- paste0("the model has alag() and an f() that depends on covariate(s) ",
                 paste(covs, collapse = ", "), ". ")
  why <- paste0("rxode2 reads a lagged dose's f() covariates at its arrival time ",
                "by default (addlKeepsCov = FALSE). NONMEM uses the dose record's ",
                "covariates (addlKeepsCov = TRUE). State the setting and say in ",
                "the article which you chose.")
  for (s in scan$sites) {
    if (s$stated) next
    if (s$kind == "rxSolve") {
      iss[[length(iss) + 1L]] <- .lintIssue(
        "addlkeepscov-unstated", "warning",
        paste0("`", s$text, "` states neither addlKeepsCov nor nonmem = TRUE, but ",
               what, why),
        "add addlKeepsCov = TRUE (NONMEM) or addlKeepsCov = FALSE (rxode2 default), or nonmem = TRUE, to this call, and say in the article which you chose")
    } else if (s$kind == "solve-unresolved") {
      iss[[length(iss) + 1L]] <- .lintIssue(
        "addlkeepscov-unverifiable", "warning",
        paste0("cannot verify `", s$text, "`: it looks like a simulation (solve() ",
               "with event-table-like arguments) but no argument traces to a model ",
               "(a name assigned from readModelDb() or rxode2(), or a model()/ini() ",
               "pipe of one), and ", what, why),
        "assign the model from readModelDb() or rxode2() and solve it by that name, or put addlKeepsCov = / nonmem = TRUE in this call, and say in the article which you chose")
    } else {
      iss[[length(iss) + 1L]] <- .lintIssue(
        "addlkeepscov-unverifiable", "warning",
        paste0("cannot verify `", s$text, "`: it solves the model without a ",
               "rxSolve() call whose arguments can be read, and ", what, why),
        "solve with rxSolve() directly, or put addlKeepsCov = / nonmem = TRUE in this call (for nlmixr2(est = \"rxSolve\"), through rxControl()), and say in the article which you chose")
    }
  }
  for (u in scan$unparsed) {
    iss[[length(iss) + 1L]] <- .lintIssue(
      "addlkeepscov-unverifiable", "warning",
      paste0("cannot verify the chunk containing `", u, "`: it mentions a solver ",
             "but does not parse as R, and ", what, why),
      "fix the chunk so it parses, then state addlKeepsCov or nonmem = TRUE in each solve")
  }
  iss
}

#' Statically pre-lint a validation vignette for common render-killers
#'
#' Scans the vignette's R chunks (and, when supplied, the model) for the
#' recurring failures that otherwise only surface during an expensive render:
#' an oversized simulation cohort, a missing `cmt =` for an algebraic
#' observable (the most common slot-renumbering failure), a named-vector scalar
#' passed to `amt =`, and a PKNCA input filter that drops the `time = 0` row.
#'
#' It also checks the lagged-dose bioavailability rule. When the model has an
#' `alag()` statement and an `f()` whose right-hand side depends on a covariate
#' (directly, through intermediate assignments, or through an `if()`
#' condition), every `rxSolve()` call in the article must state
#' `addlKeepsCov =` or `nonmem = TRUE`: by default rxode2 reads a lagged dose's
#' `f()` covariates at the dose's arrival time, whereas NONMEM uses the dose
#' record's covariates, and the two differ when the covariate changes between
#' occasions. `rxSolve()` calls are recognised however they are written
#' (`rxode2::rxSolve()`, piped, arguments over several lines), and so is rxode2's
#' `solve(model, ...)`: a `solve()` call counts as a simulation when an argument
#' is a model (a name assigned from `readModelDb()` or `rxode2()`, a
#' `model()` / `ini()` / `zeroRe()` pipe of one, or the model's library name),
#' so `base::solve()` on a matrix is left alone. A simulation run through
#' `nlmixr2(..., est = "rxSolve")`, `simulate()`, an `rxSolve` passed to another
#' function, or a `solve()` with event-table-like arguments but no traceable
#' model cannot be read statically and is reported as not verifiable unless the
#' call itself names the setting.
#'
#' @param rmd Path to the vignette `.Rmd`.
#' @param model Optional `rxUi` / model function / `nlmixr2lib` name; enables the
#'   algebraic-observable `cmt =` check and the lagged-dose check. The
#'   lagged-dose check falls back to the names the vignette passes to
#'   `readModelDb()` when no model is given.
#' @param max_per_arm Maximum simulated participants per arm (default 200).
#' @return An `nli_vignette_lint` list with `issues` (data frame) and `n`.
#' @export
lint_vignette <- function(rmd, model = NULL, max_per_arm = 200L) {
  if (!file.exists(rmd)) {
    return(structure(list(
      issues = .lintIssue("file", "error", paste0("vignette not found: ", rmd),
                          "check the path"), n = 1L),
      class = "nli_vignette_lint"))
  }
  code <- .lintRcode(rmd)
  blob <- paste(code, collapse = "\n")
  iss <- list()

  # 1. Event table referencing an algebraic observable as a compartment.
  # Per the nlmixr2 convention, event-table `cmt=` must name an ODE STATE, never
  # an algebraic observable: referencing the observable makes rxode2 inject a
  # `cmt()` slot for it AFTER the ODE states, renumbering every compartment --
  # the most common slot-renumbering render failure. rxode2 computes the
  # observable (e.g. `Cc <- central / vc`) at the state's observation rows.
  obs <- .lintObservables(model)
  if (length(obs)) {
    used <- obs[vapply(obs, function(o)
      grepl(sprintf("cmt\\s*=\\s*[\"']%s[\"']", o), blob), logical(1L))]
    if (length(used)) {
      states <- tryCatch(as.character(.stAsUi(model)$state),
                         error = function(e) character())
      rec <- if (length(states))
        paste0("use an ODE state name instead (e.g. cmt='", states[[length(states)]], "')")
      else "use the ODE state name instead"
      iss[[length(iss) + 1L]] <- .lintIssue(
        "cmt-observable", "warning",
        paste0("event table references algebraic observable(s) ",
               paste(used, collapse = ", "), " as a compartment via cmt="),
        paste0(rec, " -- referencing an observable injects a cmt slot and renumbers compartments"))
    }
  }

  # 2. Named-vector scalar passed to amt = (single-bracket subscript of a
  #    *character* key -> a named length-1 vector). A numeric index (`x[1]`)
  #    yields an unnamed scalar and is fine, so require a quoted key.
  if (grepl("amt\\s*=\\s*[A-Za-z._][\\w.$]*\\[\\s*[\"']", blob, perl = TRUE)) {
    iss[[length(iss) + 1L]] <- .lintIssue(
      "amt-named-scalar", "warning",
      "`amt =` is given a single-bracket character subscript of a named vector",
      "use `[[ ]]` (rxode2 rejects names on amt: 'Assertion on amt failed: May not have names')")
  }

  # 3. PKNCA input filter that can drop the time = 0 row -- only flagged when the
  #    vignette actually uses PKNCA (otherwise a `time > 0` filter is harmless).
  if (grepl("PKNCA", blob, fixed = TRUE) &&
      grepl("filter\\([^)]*\\b(time\\s*>\\s*0|Cc\\s*>\\s*0)\\b", blob, perl = TRUE)) {
    iss[[length(iss) + 1L]] <- .lintIssue(
      "pknca-zero-row", "warning",
      "a filter() may drop the time=0 record before PKNCA",
      "filter only with !is.na(); ensure a time=0 observation exists (else 'AUC range starting before first measurement')")
  }

  # 4. Simulated cohort larger than max_per_arm
  nums <- integer()
  for (pat in c("nSub\\s*=\\s*(\\d+)", "nsub\\s*=\\s*(\\d+)",
                "id\\s*=\\s*1:(\\d+)", "seq_len\\((\\d+)\\)",
                "id\\s*=\\s*seq_len\\((\\d+)\\)")) {
    m <- regmatches(blob, gregexpr(pat, blob, perl = TRUE))[[1L]]
    if (length(m)) nums <- c(nums,
      as.integer(regmatches(m, regexpr("\\d+", m))))
  }
  nums <- nums[is.finite(nums)]
  if (length(nums) && max(nums) > max_per_arm) {
    iss[[length(iss) + 1L]] <- .lintIssue(
      "cohort-too-large", "warning",
      paste0("simulated cohort of ", max(nums), " exceeds the ", max_per_arm,
             "-per-arm cap"),
      paste0("reduce per-arm simulation to <= ", max_per_arm,
             " (oversized cohorts are the top render-timeout / cost cause)"))
  }

  # 5. Assertions that cannot hold across machines, on a vignette that
  #    simulates a cohort.
  #
  # rxSetSeed() fixes rxode2's RNG stream PER SOLVER THREAD, not across thread
  # counts, so a 2-core CI runner and a 16-thread workstation draw different
  # cohorts from identical source and no seed makes them agree. An assertion
  # tight enough to sit inside that spread passes where it was written and
  # fails where it runs. A 2026-08-31 consolidation shipped twelve of these;
  # CI reported four, because each render shard aborts on its first failure.
  #
  # Only the high-precision shapes are flagged, and only when the vignette
  # actually simulates (set.seed + rxSolve). `>=` / `<=` forms are deliberately
  # NOT flagged: `all(conc >= 0)` is the recommended solver-noise guard.
  simulates <- grepl("set.seed", blob, fixed = TRUE) &&
    grepl("rxSolve", blob, fixed = TRUE)
  if (simulates) {
    # Match against CODE only. A correct fix documents the old assertion in a
    # comment ("an earlier revision used `all(diff(pv) < 0)` ..."), and matching
    # raw text would re-flag the very vignette that was just repaired -- a
    # linter that fires on the fixed version teaches authors to ignore it.
    codeonly <- sub("(^|[^\\\\])#.*$", "\\1", code, perl = TRUE)
    cblob <- paste(codeonly, collapse = "\n")
    frag <- list(
      c("assert-strict-monotone",
        # Compared against literal 0 only. `all(diff(x) < 0.25)` is a step
        # TOLERANCE -- the recommended fix -- and must not be flagged.
        "all\\s*\\(\\s*diff\\s*\\([^)]*\\)\\s*[<>]\\s*0+(\\.0+)?\\s*\\)",
        "asserts STRICT step-by-step monotonicity of a simulated series",
        "if the series is COHORT-derived, assert the trend (last vs first) or allow a step tolerance -- adjacent points invert on a redraw; if it is a typical-value / zeroRe() prediction it is deterministic and strict is correct, so this is a prompt to check which"),
      c("assert-exact-zero",
        "all\\s*\\([^)]*==\\s*0\\s*\\)",
        "asserts an EXACT zero over a simulated vector",
        "bound a small permitted fraction instead -- 'no subject exceeds X' is a property of one draw, not of the model")
      # NOTE: an `all(x > 0)` element-wise sign rule was tried and removed.
      # It is a legitimate and common shape when the effect is large and
      # unambiguous (positive controls such as
      # `all(prohibited_athletes$pct_over_mrl > 0)`), so it fired on correct
      # vignettes far too often to be worth its recall. Sign assertions on
      # NEAR-ZERO effects are still the most common failure -- they are covered
      # in prose in the failure-pattern catalogue, because distinguishing
      # "near zero" from "clearly positive" needs the values, not the source.
    )
    for (f in frag) {
      if (grepl(f[[2L]], cblob, perl = TRUE)) {
        iss[[length(iss) + 1L]] <- .lintIssue(f[[1L]], "warning", f[[3L]], f[[4L]])
      }
    }
  }

  # 6. Lagged-dose f() covariates: the solve must say which covariate
  #    convention it uses (see .lintLaggedDose()).
  iss <- c(iss, .lintLaggedDose(.lintRchunks(rmd), model))

  issues <- if (length(iss)) do.call(rbind, iss) else
    data.frame(check = character(), severity = character(),
               message = character(), fix = character(), stringsAsFactors = FALSE)
  rownames(issues) <- NULL
  structure(list(issues = issues, n = nrow(issues)), class = "nli_vignette_lint")
}

#' @export
print.nli_vignette_lint <- function(x, ...) {
  if (x$n == 0L) {
    cli::cli_alert_success("vignette pre-lint: no known render-killers found")
    return(invisible(x))
  }
  cli::cli_alert_warning("vignette pre-lint: {x$n} potential render issue{?s} (fix before rendering)")
  for (i in seq_len(x$n)) {
    cli::cli_text("! [{x$issues$check[i]}] {x$issues$message[i]}")
    cli::cli_text("    {.emph fix:} {x$issues$fix[i]}")
  }
  invisible(x)
}
