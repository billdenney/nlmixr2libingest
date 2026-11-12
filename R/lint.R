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
# the others). A chunk that does not run -- `eval = FALSE` in its header or
# `#| eval: false` in its body -- carries attr(, "eval") = FALSE.
.lintRchunks <- function(rmd) {
  lines <- readLines(rmd, warn = FALSE)
  inchunk <- FALSE
  chunks <- list()
  skip <- logical()
  for (ln in lines) {
    if (grepl("^```\\{[rR]", ln)) {
      inchunk <- TRUE
      chunks[[length(chunks) + 1L]] <- character()
      skip <- c(skip, grepl("eval\\s*=\\s*(FALSE|F)\\b", ln))
      next
    }
    if (inchunk && grepl("^```\\s*$", ln)) { inchunk <- FALSE; next }
    if (inchunk) {
      i <- length(chunks)
      chunks[[i]] <- c(chunks[[i]], ln)
      if (grepl("^#\\|\\s*eval:\\s*(false|FALSE)\\s*$", ln)) skip[[i]] <- TRUE
    }
  }
  for (i in which(skip)) attr(chunks[[i]], "eval") <- FALSE
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
# documentation list is incomplete is still covered. A failure to read either
# propagates: dropping the covariates would let the rule pass without looking.
.lintModelCovariates <- function(ui) {
  union(as.character(names(ui$meta$covariateData)), as.character(ui$allCovs))
}

# Covariates the model's f() statements depend on, when the model also has an
# alag() statement; character(0) otherwise. Dependence is traced through
# intermediate assignments and if() conditions (see .lintCollectAssignments).
.lintLaggedFCovariates <- function(ui) {
  acc <- new.env(parent = emptyenv())
  acc$deps <- list()
  acc$special <- list()
  statements <- ui$lstExpr
  if (!is.list(statements)) stop("the model has no readable statements", call. = FALSE)
  for (e in statements) .lintCollectAssignments(e, character(), acc)
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

# addlKeepsCov counts as stated unless it is NULL; nonmem counts unless it is
# NULL or a literal FALSE. A variable counts: it is an explicit statement even
# though its value cannot be read here.
.lintArgsState <- function(args) {
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  keeps <- args[nms == "addlKeepsCov"]
  nonmem <- args[nms == "nonmem"]
  any(!vapply(keeps, is.null, logical(1L))) ||
    any(vapply(nonmem, .lintNonmemOn, logical(1L)))
}
.lintNonmemOn <- function(v) {
  !(is.null(v) || identical(v, FALSE) || identical(v, quote(F)))
}

# Whether an argument is an rxControl() call, or a list() given as `control =`,
# whose own arguments state the setting.
.lintControlStates <- function(a, name) {
  if (!is.call(a)) return(FALSE)
  ctl <- (identical(name, "control") && .lintCallName(a) %in% c("rxControl", "list")) ||
    (identical(name, "") && identical(.lintCallName(a), "rxControl"))
  ctl && .lintArgsState(as.list(a)[-1L])
}

# TRUE when the call states the setting: addlKeepsCov or nonmem among its own
# arguments, or inside an rxControl() / `control = list()` argument. Arguments
# of other nested calls do not count: foo(addlKeepsCov = TRUE) inside an
# rxSolve() call is foo's argument, not rxSolve's.
.lintCallStates <- function(x) {
  args <- as.list(x)[-1L]
  if (.lintArgsState(args)) return(TRUE)
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  any(vapply(seq_along(args),
             function(i) .lintControlStates(args[[i]], nms[[i]]), logical(1L)))
}

# One-line, length-capped rendering of a call (or a line of text) for a message.
.lintTrunc <- function(s, width = 70L) {
  s <- gsub("\\s+", " ", s)
  if (nchar(s) > width) paste0(substr(s, 1L, width - 3L), "...") else s
}
.lintCallText <- function(x, width = 70L) {
  .lintTrunc(paste(deparse(x, width.cutoff = 500L), collapse = " "), width)
}

# --- finding the simulations in an article --------------------------------------
#
# The article's R chunks are parsed and walked in document order. Names are
# tracked through assignments so that rxode2's solve(model, ...) can be told
# from base::solve() on a matrix: a solve() call is a simulation when an
# argument traces to a model. The tracked state lives in an environment `acc`:
#   sites       every place the model is solved (see .lintWalkSolves());
#   models      the literal names given to readModelDb();
#   modelnames  names known to hold a model (and the model's library name);
#   containers  names known to hold a list of models;
#   eventnames  names known to hold an event table, or a function that builds one;
#   funs        article-local functions: name -> list(params);
#   pending     solve() calls inside a local function on one of its parameters,
#               resolved when the function is called with a model;
#   ctx         the local function being walked, if any;
#   unparsed    chunks that mention a solver but do not parse.

# Functions that return a model whatever their arguments, functions that return
# their first argument's model modified, and the event-table builders.
.lintModelMakers <- c("readModelDb", "rxode2", "rxode")
.lintModelWrappers <- c("model", "ini", "zeroRe", "rxFixPop", "rxRmIni",
                        "rxAppendModel", "suppressMessages", "suppressWarnings",
                        "invisible", "(")
.lintEventMakers <- c("et", "eventTable", "etRep", "etSeq", "etExpand",
                      "add.dosing", "add.sampling")
# Calls whose result is an event table when one of their arguments is, as in
# rbind(dosing_rows, obs_rows) or do.call(rbind, lapply(ids, obs_rows)).
.lintEventCombinators <- c("rbind", "bind_rows", "do.call", "lapply", "(")
.lintApplyFamily <- c("lapply", "sapply", "vapply", "Map", "mapply", "map", "walk")
# Arguments of rxSolve() that base::solve() (a, b, tol) does not have.
.lintSolveArgs <- c("events", "params", "inits", "nSub", "nsub", "nStud", "nstud",
                    "omega", "sigma", "thetaMat", "dfSub", "dfObs", "atol",
                    "rtol", "keep", "cores", "seed", "returnType",
                    "addlKeepsCov", "nonmem")
# Calls that mention rxSolve without using it.
.lintInert <- c("?", "help", "args", "formals", "body", "environment")

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

# TRUE when `x` is a list of models: a name known to hold one, a list()/c() with
# a model (or a list of models) in it, or lapply(names, readModelDb).
.lintIsContainer <- function(x, acc) {
  if (.lintIsPipe(x)) x <- .lintDePipe(x)
  if (is.symbol(x)) return(as.character(x) %in% acc$containers)
  if (!is.call(x)) return(FALSE)
  nm <- .lintCallName(x)
  args <- as.list(x)[-1L]
  if (nm %in% c("list", "c")) {
    return(any(vapply(args, function(a) .lintIsModel(a, acc) || .lintIsContainer(a, acc),
                      logical(1L))))
  }
  if (nm %in% .lintApplyFamily) {
    return(any(vapply(args, function(a) is.symbol(a) && as.character(a) %in% .lintModelMakers,
                      logical(1L))))
  }
  FALSE
}

# TRUE when `x` evaluates to a model: a name or string already known to hold
# one (including a model's literal library name), a readModelDb()/rxode2()/
# rxode() call, a model()/ini()/zeroRe()/... call on one, an element of a list
# of models (`$` or `[[`), or the `$ui` of a model.
.lintIsModel <- function(x, acc) {
  if (.lintIsPipe(x)) x <- .lintDePipe(x)
  if (is.symbol(x) || is.character(x)) {
    return(length(x) == 1L && as.character(x) %in% acc$modelnames)
  }
  if (!is.call(x)) return(FALSE)
  nm <- .lintCallName(x)
  if (nm %in% .lintModelMakers) return(TRUE)
  if (nm %in% c("$", "[[") && length(x) == 3L) {
    if (.lintIsContainer(x[[2L]], acc)) return(TRUE)
    return(nm == "$" && identical(x[[3L]], quote(ui)) && .lintIsModel(x[[2L]], acc))
  }
  # an event table built on a model, et(mod, ...), still carries the model
  if ((nm %in% .lintModelWrappers || nm %in% .lintEventMakers) && length(x) >= 2L) {
    return(.lintIsModel(x[[2L]], acc))
  }
  FALSE
}

# TRUE when `x` is an event table passed whole: a name known to hold one, an
# et()-family call, a call to a function known to build one, a record built
# inline with amt = / evid =, or rbind()/do.call()/lapply() of such. Anything
# taken from or computed on an event table (`$`, `[`, `[[`, arithmetic,
# crossprod()) is not.
.lintIsEvents <- function(x, acc) {
  if (.lintIsPipe(x)) x <- .lintDePipe(x)
  if (is.symbol(x)) return(as.character(x) %in% acc$eventnames)
  if (!is.call(x)) return(FALSE)
  nm <- .lintCallName(x)
  parts <- as.list(x)
  nms <- names(parts)
  if (is.null(nms)) nms <- rep("", length(parts))
  if (nm %in% .lintEventMakers || nm %in% acc$eventnames ||
      any(nms %in% c("amt", "evid"))) {
    return(TRUE)
  }
  if (identical(nm, "function")) return(.lintIsEvents(parts[[3L]], acc))
  if (identical(nm, "{")) return(.lintIsEvents(parts[[length(parts)]], acc))
  if (nm %in% .lintEventCombinators) {
    return(any(vapply(parts[-1L], .lintIsEvents, logical(1L), acc = acc)))
  }
  FALSE
}

# Classify a solve() call: "rxSolve" when an argument is a model (so it is a
# simulation whose arguments can be read like rxSolve()'s); "solve-unresolved"
# when no argument is a model but an argument is an event table or an
# rxSolve-only argument (a simulation we cannot trace); NA when it looks like
# base::solve() on a matrix.
.lintSolveKind <- function(x, acc) {
  args <- as.list(x)[-1L]
  if (any(vapply(args, .lintIsModel, logical(1L), acc = acc))) return("rxSolve")
  if (any(vapply(args, .lintIsEvents, logical(1L), acc = acc)) ||
      any(names(args) %in% .lintSolveArgs)) {
    return("solve-unresolved")
  }
  NA_character_
}

# TRUE when `a` is the name `s` or the string "s".
.lintIsRef <- function(a, s) identical(a, s) || identical(a, as.name(s))

# TRUE when `x` is a literal string, or c() of literal strings.
.lintIsLiteralChr <- function(x) {
  is.character(x) ||
    (is.call(x) && identical(.lintCallName(x), "c") &&
       all(vapply(as.list(x)[-1L], is.character, logical(1L))))
}
.lintHasString <- function(x, s) {
  if (is.character(x)) return(s %in% x)
  is.call(x) && identical(.lintCallName(x), "c") &&
    any(vapply(as.list(x)[-1L], .lintHasString, logical(1L), s = s))
}

# nlmixr2(..., est = "rxSolve") simulates through rxSolve(); an `est` that is
# not a literal cannot be read, so it is reported too.
.lintEstKind <- function(args) {
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  if (any(vapply(args[nms %in% c("est", "")], .lintHasString, logical(1L), s = "rxSolve"))) {
    return("est-rxSolve")
  }
  if (any(!vapply(args[nms == "est"], .lintIsLiteralChr, logical(1L)))) return("est-rxSolve")
  NA_character_
}

# rxSolve or solve handed to another function as a value. rxSolve by string
# (do.call("rxSolve", ...)) is always reported; solve is also an ordinary
# matrix function, so it is reported only when match.fun()/get() fetch it by
# name, or when it travels with a model, a list of models, an event table or an
# rxSolve-only argument (lapply(list(mod), solve, ev), do.call(solve, list(mod, ev))).
.lintIndirectKind <- function(nm, args, acc) {
  fetch <- nm %in% c("do.call", "match.fun", "get", "getExportedValue")
  if (fetch && any(vapply(args, identical, logical(1L), "rxSolve"))) return("indirect")
  if ("solve" %in% names(acc$funs)) return(NA_character_)
  if (nm %in% c("match.fun", "get", "getExportedValue") &&
      any(vapply(args, identical, logical(1L), "solve"))) {
    return("indirect")
  }
  is_solve <- vapply(args, .lintIsRef, logical(1L), s = "solve")
  if (!any(is_solve)) return(NA_character_)
  others <- args[!is_solve]
  travels <- any(vapply(others, function(a) {
    .lintIsModel(a, acc) || .lintIsContainer(a, acc) || .lintIsEvents(a, acc)
  }, logical(1L))) || any(names(others) %in% .lintSolveArgs)
  if (travels) "indirect" else NA_character_
}

# Record a place the model is solved; returns its position in acc$sites.
.lintAddSite <- function(acc, kind, stated, text) {
  id <- length(acc$sites) + 1L
  acc$sites[[id]] <- list(kind = kind, stated = stated, text = text)
  id
}

# Note what a name is assigned from, for later solve() calls.
.lintRecordAssignment <- function(name, rhs, acc) {
  if (.lintIsModel(rhs, acc)) {
    acc$modelnames <- union(acc$modelnames, name)
  } else if (.lintIsContainer(rhs, acc)) {
    acc$containers <- union(acc$containers, name)
  } else if (.lintIsEvents(rhs, acc)) {
    acc$eventnames <- union(acc$eventnames, name)
  }
  invisible(NULL)
}

# Match call arguments to a function's parameters: by name, then by position.
.lintMatchArgs <- function(params, args) {
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  out <- args[nms %in% params]
  free <- setdiff(params, names(out))
  pos <- args[nms == ""]
  k <- min(length(free), length(pos))
  if (k > 0L) out[free[seq_len(k)]] <- pos[seq_len(k)]
  out
}

# A solve() inside an article-local function, on one of its parameters, is a
# simulation only if the function is called with a model there. Remember it,
# and settle it when the call comes (see .lintResolvePending()).
.lintAddPending <- function(x, id, acc) {
  args <- as.list(x)[-1L]
  named <- vapply(args, function(a) if (is.symbol(a)) as.character(a) else "",
                  character(1L))
  for (p in intersect(named[nzchar(named)], acc$ctx$params)) {
    acc$pending[[length(acc$pending) + 1L]] <- list(
      fun = acc$ctx$name, param = p, site = id, stated = .lintCallStates(x),
      text = .lintCallText(x), resolved = FALSE)
  }
  invisible(NULL)
}
.lintResolvePending <- function(name, call, acc) {
  matched <- .lintMatchArgs(acc$funs[[name]]$params, as.list(call)[-1L])
  for (i in seq_along(acc$pending)) {
    p <- acc$pending[[i]]
    if (p$fun != name || p$resolved || !p$param %in% names(matched)) next
    if (!.lintIsModel(matched[[p$param]], acc)) next
    acc$pending[[i]]$resolved <- TRUE
    if (is.na(p$site)) {
      acc$pending[[i]]$site <- .lintAddSite(acc, "rxSolve", p$stated, p$text)
    } else {
      acc$sites[[p$site]]$kind <- "rxSolve"
    }
  }
  invisible(NULL)
}

# Names that hold a model only inside this call: with(list(m = mod), ...), and
# the parameter of a function mapped over a list of models.
.lintScopedModels <- function(nm, args, acc) {
  if (identical(nm, "with") && length(args) >= 2L && is.call(args[[1L]]) &&
      identical(.lintCallName(args[[1L]]), "list")) {
    bound <- as.list(args[[1L]])[-1L]
    bn <- names(bound)
    if (is.null(bn)) bn <- rep("", length(bound))
    return(bn[nzchar(bn) & vapply(bound, .lintIsModel, logical(1L), acc = acc)])
  }
  if (nm %in% .lintApplyFamily && any(vapply(args, .lintIsContainer, logical(1L), acc = acc))) {
    out <- character()
    for (f in args[vapply(args, function(a) is.call(a) &&
                            .lintCallName(a) %in% c("function", "~"), logical(1L))]) {
      out <- c(out, if (identical(.lintCallName(f), "~")) c(".x", ".") else names(f[[2L]])[1L])
    }
    return(out[!is.na(out)])
  }
  character()
}

# Record the places `x` solves the model, and what its assignments teach us.
.lintVisitCall <- function(x, nm, acc) {
  args <- as.list(x)[-1L]
  local_solve <- "solve" %in% names(acc$funs)
  kind <- if (nm %in% c("rxSolve", "rxSolve.default")) "rxSolve"
          else if (identical(nm, "solve") && !local_solve) .lintSolveKind(x, acc)
          else if (nm %in% c("nlmixr2", "nlmixr")) .lintEstKind(args)
          else if (identical(nm, "simulate")) "simulate"
          else .lintIndirectKind(nm, args, acc)
  id <- NA_integer_
  if (!is.na(kind)) {
    id <- .lintAddSite(acc, kind, kind != "indirect" && .lintCallStates(x),
                       .lintCallText(x))
  }
  if (identical(nm, "solve") && !local_solve && !is.null(acc$ctx) &&
      !identical(kind, "rxSolve")) {
    .lintAddPending(x, id, acc)
  }
  if (identical(nm, "readModelDb") && length(args) >= 1L && is.character(args[[1L]])) {
    acc$models <- c(acc$models, args[[1L]])
    acc$modelnames <- union(acc$modelnames, args[[1L]])
  }
  if (identical(nm, "for") && length(args) == 3L && is.symbol(args[[1L]]) &&
      .lintIsContainer(args[[2L]], acc)) {
    acc$modelnames <- union(acc$modelnames, as.character(args[[1L]]))
  }
  if (nm %in% names(acc$funs)) .lintResolvePending(nm, x, acc)
  invisible(NULL)
}

# Walk a local function's body with its parameters in scope.
.lintWalkFunction <- function(fn, name, acc) {
  outer <- acc$ctx
  acc$ctx <- list(name = name, params = names(fn[[2L]]))
  lapply(as.list(fn)[-1L], .lintWalkSolves, acc = acc)
  acc$ctx <- outer
  invisible(NULL)
}

# Walk parsed article code and record into `acc`. A site is a place the model
# is solved: kind "rxSolve" (a call to rxSolve, or to solve() on a model,
# however spelled or piped -- the native pipe is already rewritten to a plain
# call by the parser), "est-rxSolve" (nlmixr2(..., est = "rxSolve"), or an est
# that is not a literal), "simulate" (simulate()), "solve-unresolved" (solve()
# that looks like a simulation but whose model cannot be traced), or "indirect"
# (rxSolve or solve handed to another function as a value); `stated` says
# whether the call itself names the setting. Only an rxSolve or model solve()
# call can be checked argument by argument; the others pass their arguments on
# in ways static code cannot follow.
.lintWalkSolves <- function(x, acc) {
  if (is.pairlist(x)) {
    lapply(as.list(x), .lintWalkSolves, acc = acc)
    return(invisible(NULL))
  }
  if (is.symbol(x)) {
    if (as.character(x) %in% c("rxSolve", "rxSolve.default")) {
      .lintAddSite(acc, "indirect", FALSE, "rxSolve passed as a function")
    }
    return(invisible(NULL))
  }
  if (!is.call(x)) return(invisible(NULL))
  if (.lintIsPipe(x)) x <- .lintDePipe(x)
  nm <- .lintCallName(x)
  if (nm %in% .lintInert) return(invisible(NULL))
  parts <- as.list(x)
  if (nm %in% c("<-", "=") && length(parts) == 3L) {
    lhs <- parts[[2L]]
    rhs <- parts[[3L]]
    if (is.symbol(lhs)) {
      .lintRecordAssignment(as.character(lhs), rhs, acc)
      if (is.call(rhs) && identical(.lintCallName(rhs), "function")) {
        acc$funs[[as.character(lhs)]] <- list(params = names(rhs[[2L]]))
        .lintWalkFunction(rhs, as.character(lhs), acc)
        return(invisible(NULL))
      }
    } else if (is.call(lhs) && .lintCallName(lhs) %in% c("$", "[[") &&
               length(lhs) == 3L && is.symbol(lhs[[2L]]) && .lintIsModel(rhs, acc)) {
      acc$containers <- union(acc$containers, as.character(lhs[[2L]]))
    }
  }
  .lintVisitCall(x, nm, acc)
  scoped <- setdiff(.lintScopedModels(nm, parts[-1L], acc), acc$modelnames)
  acc$modelnames <- c(acc$modelnames, scoped)
  # The function position is not walked when it is a plain name or pkg::name:
  # the name itself is not a reference to rxSolve as a value.
  head_is_name <- is.symbol(parts[[1L]]) || .lintIsNsCall(parts[[1L]])
  lapply(if (head_is_name) parts[-1L] else parts, .lintWalkSolves, acc = acc)
  acc$modelnames <- setdiff(acc$modelnames, scoped)
  invisible(NULL)
}

# Scan the chunks for the places the model is solved. A chunk that does not
# parse but mentions a solver is recorded in acc$unparsed rather than skipped,
# because a solve that cannot be read cannot be shown to state the setting; a
# bare solve( counts as a solver only when the article also works with a model
# (readModelDb() or the model's name), since solve() is mostly matrix algebra.
# A chunk with eval = FALSE does not run and is skipped.
.lintScanSolves <- function(chunks, known = character()) {
  acc <- new.env(parent = emptyenv())
  acc$sites <- list()
  acc$models <- character()
  acc$modelnames <- known
  acc$containers <- character()
  acc$eventnames <- character()
  acc$funs <- list()
  acc$pending <- list()
  acc$ctx <- NULL
  acc$unparsed <- character()
  all_text <- unlist(chunks, use.names = FALSE)
  uses_model <- any(grepl("readModelDb", all_text, fixed = TRUE)) ||
    any(vapply(known, function(k) any(grepl(k, all_text, fixed = TRUE)), logical(1L)))
  solver <- if (uses_model) "rxSolve|simulate\\s*\\(|nlmixr2?\\s*\\(|\\bsolve\\s*\\("
            else "rxSolve|simulate\\s*\\(|nlmixr2?\\s*\\("
  for (ch in chunks) {
    if (isFALSE(attr(ch, "eval"))) next
    ex <- tryCatch(parse(text = paste(ch, collapse = "\n"), keep.source = FALSE),
                   error = function(e) NULL)
    if (is.null(ex)) {
      hit <- grep(solver, ch, value = TRUE)
      if (length(hit)) acc$unparsed <- c(acc$unparsed, .lintTrunc(hit[[1L]], 60L))
    } else {
      lapply(as.list(ex), .lintWalkSolves, acc = acc)
    }
  }
  list(sites = acc$sites, models = unique(acc$models), unparsed = acc$unparsed)
}

# The rule: when the model has alag() and an f() that depends on a covariate,
# every place the article solves the model must state addlKeepsCov or
# nonmem = TRUE. Returns a list of .lintIssue() rows.
#
# The model is the `model` argument when given (a name, path-loaded function or
# rxUi, exactly as the other model checks take it), else every literal name the
# article passes to readModelDb(). With neither, no model is known and the rule
# cannot apply. A model that is named but cannot be read -- parsed, or its
# covariates and statements extracted -- is reported, never skipped: a silent
# skip is how an unchecked article looks like a checked one.
.lintLaggedDose <- function(chunks, model) {
  known <- if (is.character(model) && length(model) == 1L) model else character()
  scan <- .lintScanSolves(chunks, known)
  if (!length(scan$sites) && !length(scan$unparsed)) return(list())
  srcs <- if (is.null(model)) as.list(scan$models) else list(model)
  if (!length(srcs)) return(list())

  iss <- list()
  covs <- character()
  for (src in srcs) {
    got <- tryCatch(.lintLaggedFCovariates(.stAsUi(src)), error = function(e) e)
    if (inherits(got, "error")) {
      label <- if (is.character(src)) paste0("`", src, "`") else "the supplied model"
      iss[[length(iss) + 1L]] <- .lintIssue(
        "addlkeepscov-model", "warning",
        paste0("cannot read ", label, " (", .lintTrunc(conditionMessage(got), 80L),
               "), so whether the article's simulations need addlKeepsCov (a model ",
               "with alag() and a covariate-dependent f()) was not checked"),
        "install rxode2 and nlmixr2lib, or pass the model's .R path as the second argument, so the model can be read")
    } else {
      covs <- union(covs, got)
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
#' passed to `amt =`, a PKNCA input filter that drops the `time = 0` row,
#' assertions that cannot hold across machines, and a simulation that does not
#' state its lagged-dose covariate convention (see Details).
#'
#' @details
#' **Lagged-dose bioavailability.** When the model has an `alag()` statement
#' and an `f()` whose right-hand side depends on a covariate (directly, through
#' intermediate assignments, or through an `if()` condition), every simulation
#' in the article -- each `rxSolve()` call and each rxode2 `solve(model, ...)`
#' call -- must state `addlKeepsCov =` or `nonmem = TRUE`. By default rxode2
#' reads a lagged dose's `f()` covariates at the dose's arrival time, whereas
#' NONMEM uses the dose record's covariates, and the two differ when the
#' covariate changes between occasions. The contract is to resolve or report
#' every form the rule can read, and to list here what it cannot.
#'
#' *What counts as a simulation.* `rxSolve()` in any spelling (`rxode2::`,
#' `rxSolve.default`, native or magrittr pipes, arguments over several lines).
#' `solve()` counts when an argument traces to a model through the article's
#' assignments: a name assigned from `readModelDb()`, `rxode2()` or `rxode()`;
#' a `model()` / `ini()` / `zeroRe()` pipe of one; the `$ui` of one; an element
#' (`$`, `[[`) of a list of models; the loop variable of a `for` loop over such
#' a list, or the parameter of a function mapped over it by `lapply()` and the
#' like; a `with(list(m = mod), ...)` binding; the model's library name. A
#' `solve()` inside an article-local function on one of its parameters counts
#' when the function is called with a model there, and is reported at the
#' definition. `base::solve()` on a matrix is left alone, and so is a `solve()`
#' the article defines itself (its own `rxSolve()` call is judged instead).
#' Chunks with `eval = FALSE` are skipped.
#'
#' *What counts as stating the setting.* `addlKeepsCov =` with any value but
#' `NULL`, or `nonmem =` with anything but `NULL` or a literal `FALSE`, among
#' the call's own arguments or inside an `rxControl()` (or `control = list()`)
#' argument. A variable counts, since it states the setting. Arguments of other
#' nested calls do not: `foo(addlKeepsCov = TRUE)` inside an `rxSolve()` call is
#' `foo`'s argument. `control = ctl`, with `ctl` defined elsewhere, is not
#' followed.
#'
#' *Reported as not verifiable* (unless the call itself states the setting):
#' `nlmixr2(..., est = "rxSolve")`, or an `est` that is not a literal;
#' `simulate()`; `rxSolve` or `solve` handed to another function as a value
#' (`do.call()`, `lapply()`, `Map()`, `match.fun()`, ...) -- `solve` only when
#' it travels with a model, a list of models, an event table or an
#' rxSolve-only argument, since `lapply(matrices, solve)` is matrix algebra;
#' a `solve()` with event-table-like arguments (an `et()` call, a data frame of
#' dosing records, a function that builds one, `atol =`, `params =`, ...) but no
#' traceable model; and a chunk that mentions a solver but does not parse (a
#' bare `solve(` counts only when the article also uses `readModelDb()` or the
#' model's name). A model that is named but cannot be read is reported too.
#'
#' *Not resolved and not reported.* A model returned by a user function, built
#' by `eval(parse())` or `getFromNamespace()`, or reached by a `for` loop over
#' `readModelDb()` names; a model passed to a local function through
#' `lapply()` or `do.call()` rather than called directly; `do.call(solve, args)`
#' with `args` a bare name; a `solve()` whose model cannot be traced and whose
#' arguments are not event-table-like (it is read as matrix algebra). Names are
#' tracked across the whole article rather than per function.
#'
#' *Over-approximations* (reported although the rule may not apply, and cleared
#' by stating the setting). `alag()` and `f()` are not tied to the same
#' compartment. Assignments are not ordered: every right-hand side a variable is
#' ever given counts, including one read before it is assigned. An article that
#' uses several models takes the union of their covariates, so a simulation of a
#' model without the hazard is reported when another model in the article has
#' it. Covariates that never change between occasions (weight, sex) are
#' reported like any other; stating `addlKeepsCov = FALSE` is the intended way
#' to clear them.
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
