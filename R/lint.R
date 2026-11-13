# Static, pre-render vignette linter.
#
# Rendering is the single most expensive / most-iterated operation in an
# extraction (each failed render adds a turn and a large render log to the
# re-read-every-turn context). This linter catches the most common, regex- and
# rxUi-detectable render-killers WITHOUT a render round-trip, so they are fixed
# before the first (expensive) render rather than discovered by it. It is a
# best-effort PRE-check -- it does not replace the render gate, and a clean lint
# does not guarantee a clean render.

# The chunk fences knitr recognises in markdown (knitr::all_patterns$md): an
# optional indent of spaces, tabs and `>`, three or more backticks, and for the
# opening fence an engine name and options in braces. Copied rather than read
# from knitr, which is only a Suggests.
.lintChunkBegin <- "^[\t >]*```+\\s*\\{([a-zA-Z0-9_]+( *[ ,].*)?)\\}\\s*$"
.lintChunkEnd <- "^[\t >]*```+\\s*$"

# Whether a chunk header turns evaluation off: a literal eval = FALSE (or F)
# among the options, outside any quoted string. Not honoured: eval = c(FALSE),
# eval = !TRUE, eval = <variable>, and a document-wide
# knitr::opts_chunk$set(eval = FALSE).
.lintHeaderNoEval <- function(options) {
  unquoted <- gsub("\"[^\"]*\"|'[^']*'", "", options)
  grepl("(^|[ ,])eval\\s*=\\s*(FALSE|F)\\s*(,|$)", unquoted)
}

# Whether a chunk's leading `#| ` option lines turn evaluation off. `meta` is
# the text after each line's `#| ` prefix. As in xfun::divide_chunk(), the block
# is YAML when its first line looks like `key: value`, and R-style options
# (`eval = FALSE, echo = TRUE`) otherwise. YAML booleans are the ones R's yaml
# reads as FALSE: false, no, n and off in lower, Capitalised or upper case --
# `F` is a string there, and the chunk runs. A trailing ` # comment` is not part
# of the value.
.lintOptionLinesNoEval <- function(meta) {
  if (!length(meta)) return(FALSE)
  if (grepl("^[^ :]+:($|\\s)", meta[[1L]])) {
    vals <- sub("^eval:\\s*", "", meta[grepl("^eval:($|\\s)", meta)])
    vals <- trimws(sub("\\s+#.*$", "", vals))
    return(any(vals %in% c("false", "False", "FALSE", "no", "No", "NO", "n", "N",
                           "off", "Off", "OFF")))
  }
  grepl("(^|,)\\s*eval\\s*=\\s*(FALSE|F)\\s*(,|$)", paste(meta, collapse = ","))
}

# The R code of each R chunk of an Rmd, one character vector per chunk (chunks
# are parsed separately so one that does not parse cannot hide the others).
# Fences are matched as knitr matches them, so an indented or four-backtick
# chunk is read; chunks of other engines are not. A chunk that does not run --
# `eval = FALSE` in its header, or a false `eval` among its leading `#| ` option
# lines (see .lintOptionLinesNoEval()) -- carries attr(, "eval") = FALSE.
.lintRchunks <- function(rmd) {
  lines <- readLines(rmd, warn = FALSE)
  inchunk <- FALSE
  is_r <- FALSE
  indent <- ""
  leading <- FALSE
  chunks <- list()
  skip <- logical()
  meta <- list()
  for (ln in lines) {
    if (!inchunk) {
      if (grepl(.lintChunkBegin, ln)) {
        inchunk <- TRUE
        options <- sub(.lintChunkBegin, "\\1", ln)
        is_r <- grepl("^[rR]($|[ ,])", options)
        indent <- sub("```.*$", "", ln)
        leading <- TRUE
        if (is_r) {
          chunks[[length(chunks) + 1L]] <- character()
          meta[[length(meta) + 1L]] <- character()
          skip <- c(skip, .lintHeaderNoEval(options))
        }
      }
      next
    }
    if (grepl(.lintChunkEnd, ln)) { inchunk <- FALSE; next }
    if (!is_r) next
    # a bare `>` ends a blockquoted line that is empty
    if (nzchar(indent)) {
      if (startsWith(ln, indent)) ln <- substring(ln, nchar(indent) + 1L)
      else if (identical(ln, sub("\\s+$", "", indent))) ln <- ""
    }
    i <- length(chunks)
    chunks[[i]] <- c(chunks[[i]], ln)
    if (leading && startsWith(ln, "#| ")) {
      meta[[i]] <- c(meta[[i]], substring(ln, 4L))
    } else {
      leading <- FALSE
    }
  }
  for (i in seq_along(chunks)) {
    if (skip[[i]] || .lintOptionLinesNoEval(meta[[i]])) attr(chunks[[i]], "eval") <- FALSE
  }
  chunks
}

# Everything from the first `#` not preceded by a backslash: the crude comment
# cut, used on code that cannot be parsed.
.lintCutComment <- function(x) sub("(^|[^\\\\])#.*$", "\\1", x, perl = TRUE)

# One chunk's code with its comments blanked, so that a rule matching text sees
# only what runs. Comments are found from the parse data, which knows a `#` in
# a string from a comment, and are cut from the end of their line by their own
# text (the parser's columns count a tab to the next multiple of 8, so they
# cannot be used to cut). The session's keep.parse.data option is set for the
# parse, so that only code that genuinely does not parse takes the fallback of
# dropping everything from the first `#` not preceded by a backslash.
.lintStripComments <- function(code) {
  if (!length(code)) return(code)
  old <- options(keep.source = TRUE, keep.parse.data = TRUE)
  on.exit(options(old), add = TRUE)
  pd <- tryCatch(utils::getParseData(parse(text = code, keep.source = TRUE)),
                 error = function(e) NULL)
  if (is.null(pd)) return(.lintCutComment(code))
  comments <- pd[pd$token == "COMMENT", , drop = FALSE]
  for (i in seq_len(nrow(comments))) {
    ln <- comments$line1[[i]]
    txt <- comments$text[[i]]
    code[[ln]] <- if (endsWith(code[[ln]], txt)) {
      substr(code[[ln]], 1L, nchar(code[[ln]]) - nchar(txt))
    } else {
      .lintCutComment(code[[ln]])
    }
  }
  code
}

# The code of all R chunks, comments removed.
.lintCode <- function(chunks) {
  as.character(unlist(lapply(chunks, .lintStripComments), use.names = FALSE))
}

# Algebraic observables: prediction variables that are not ODE compartment
# states (e.g. `Cc <- central / vc; Cc ~ prop(.)`). Best-effort via rxUi.
.lintObservables <- function(ui) {
  tryCatch({
    pd <- ui$predDf
    nm <- if (is.data.frame(pd) && "var" %in% names(pd)) pd$var
          else if (is.data.frame(pd) && "cond" %in% names(pd)) pd$cond
          else character()
    st <- tryCatch(as.character(ui$state), error = function(e) character())
    unique(setdiff(as.character(nm), st))
  }, error = function(e) character())
}

# Read each model source (a library name, a function, an rxUi): one list per
# source with `ui` (NULL when it could not be read) and `error`. rxode2 reports
# its syntax errors as messages, which would bury the lint output.
.lintReadModels <- function(srcs) {
  lapply(srcs, function(src) {
    ui <- NULL
    err <- tryCatch({
      suppressMessages(ui <- .stAsUi(src))
      NULL
    }, error = function(e) conditionMessage(e))
    list(label = if (is.character(src)) paste0("`", src, "`") else "the supplied model",
         ui = ui, error = err)
  })
}

.lintIssue <- function(check, severity, message, fix) {
  data.frame(check = check, severity = severity, message = message, fix = fix,
             stringsAsFactors = FALSE)
}

# --- lagged-dose f() covariates ------------------------------------------------
#
# A dose delayed by alag() is applied when it ARRIVES, so which record's
# covariates its f() reads depends on the setting and on the dose type
# (measured on rxode2 5.1.8.9000, e4cb67f6 and c865aa8, with a different value on
# each candidate record: the one before the dose, the dose's own, and one other
# record at the dose's time or inside the lag window):
#   bolus dose
#     addlKeepsCov = FALSE (the default) reads the record in force at arrival,
#       so a record at the dose's time (written before or after the dose) or one
#       inside the lag window wins over the dose's own record.
#     addlKeepsCov = TRUE and nonmem = TRUE read the dose record's own
#       covariates, which is NONMEM's pairing.
#   infusion (rate = -2 with dur(), rate = -1 with rate(), or a fixed rate)
#     TRUE and nonmem = TRUE never read the dose's own record.
#       rate = -1 and -2 read the last record before the dose, or a record at
#       the dose's time other than a dose into the same compartment (an
#       observation, or a dose into another compartment), which then supplies
#       f() whatever the row order.
#       A fixed rate (rate > 0) reads a record that cannot be pinned to the
#       dose or to its neighbours: with records at 0, 10, 20 and a dose at 24,
#       f() came from the record at 0 (not the one at 20); with the first
#       record at 5, 3 or 1 it came from the dose's own; with a record at the
#       arrival time (24.5) or just after it (24.7, lag 0.5) it came from that
#       record, and a record at 26 or 40 did not supply it. How far after the
#       arrival a record reaches depends on the dose's amount (a record at 26
#       supplied f() at amt 101 and 200, not at 100).
#     FALSE reads the dose's own record for every infusion type, unless a record
#       other than a dose into the same compartment shares the dose's time: that
#       record then supplies f(), whatever the row order. A bolus ignores a dose
#       into another compartment at its time; an infusion reads it. A record
#       inside the lag window does not override it.
#   a fixed duration (a dur column and no rate) reads like a bolus.
#   modelled rate() and dur() (rate = -1 and -2): a record at the dose's time or
#     inside the lag window (not at the arrival time itself) supplies them under
#     every setting, except under nonmem = TRUE when that record's row is
#     written before the dose. An observation does; so can a dose into another
#     compartment; a dose into the same compartment does not.
# Without a lag every setting reads the dose's own record. What is right for
# every dose type is an event table in which each record carries the covariates
# of the dose that opened its interval, with addlKeepsCov = FALSE. The rule asks
# only that the setting be stated; it does not read the event table.

# Record every assignment of a model() block into `acc`:
#   acc$deps    named list, variable -> the names its right-hand sides read,
#               over ALL of its assignments, plus the names of every enclosing
#               if() condition (which decide whether the assignment happens);
#   acc$special one entry per f() / alag() / dur() / rate() statement: the
#               function ("f", "alag", "dur" or "rate"), the names its
#               right-hand side reads, likewise, the right-hand side itself and
#               the compartment it is for (NA when not a plain name).
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
                   dur = "dur", rate = "rate", NA_character_)
      if (!is.na(fn)) {
        acc$special[[length(acc$special) + 1L]] <- list(
          fn = fn, reads = reads, rhs = x[[3L]],
          cmt = if (is.symbol(lhs[[2L]])) as.character(lhs[[2L]]) else NA_character_)
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

# addlKeepsCov counts as stated unless it is NULL: the author addressed it
# directly, so a variable counts too. nonmem counts only as a literal TRUE (or
# T): it sets addlKeepsCov only when it is TRUE, and a variable may hold FALSE.
.lintArgsState <- function(args) {
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  keeps <- args[nms == "addlKeepsCov"]
  nonmem <- args[nms == "nonmem"]
  any(!vapply(keeps, is.null, logical(1L))) ||
    any(vapply(nonmem, .lintNonmemOn, logical(1L)))
}
.lintNonmemOn <- function(v) isTRUE(v) || identical(v, quote(T))

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
#   models      the literal names given to readModelDb() or modellib();
#   modelnames  names known to hold a model (and the model's library name);
#   containers  names known to hold a list of models;
#   eventnames  names known to hold an event table, or a function that builds one;
#   funs        article-local functions: name -> list(params);
#   topfuns     names the article defines as top-level functions, which shadow
#               the base and rxode2 functions of the same name;
#   toplevel    the top-level expression being walked;
#   pending     solve() calls inside a local function on one of its parameters,
#               resolved when the function is called with a model;
#   ctx         the local function being walked, if any;
#   unparsed    chunks that mention a solver but do not parse.
# A name that is assigned something else stops being tracked (see
# .lintRecordAssignment()).

# Functions that return a model whatever their arguments, functions that return
# their first argument's model modified, and the event-table builders.
.lintModelMakers <- c("readModelDb", "modellib", "rxode2", "rxode")
.lintModelWrappers <- c("model", "ini", "zeroRe", "rxFixPop", "rxRmIni",
                        "rxAppendModel", "suppressMessages", "suppressWarnings",
                        "invisible", "(")
.lintEventMakers <- c("et", "eventTable", "etRep", "etSeq", "etExpand",
                      "add.dosing", "add.sampling")
# Calls whose result is an event table when one of their arguments is, as in
# rbind(dosing_rows, obs_rows) or do.call(rbind, lapply(ids, obs_rows)).
.lintEventCombinators <- c("rbind", "bind_rows", "do.call", "lapply", "(")
# Calls that keep a list of models a list of models.
.lintContainerWrappers <- c("setNames", "unname", "rev", "head", "tail")
# Calls that run a function over a list, binding its parameter to each element.
.lintApplyFamily <- c("lapply", "sapply", "vapply", "Map", "mapply", "map",
                      "map2", "pmap", "imap", "walk", "walk2", "pwalk", "iwalk")
.lintIsApply <- function(nm) nm %in% .lintApplyFamily || grepl("^map_", nm)
# The calls that run a function over arguments, for binding that function's
# parameters to their first elements (a wider list than .lintIsApply(), which
# decides where a list of models goes).
.lintIsApplyLike <- function(nm) {
  .lintIsApply(nm) || grepl("^(pmap|pwalk|map2|walk2|imap|map|walk)(_|$)", nm) ||
    nm %in% c("Map", "mapply", "rapply", "tapply", "outer")
}
# The solve() methods rxode2 registers, which rxode2:::solve.rxUi(mod, ev) and the
# like call by name.
.lintSolveMethods <- paste0("solve.", c("rxUi", "rxode2", "function", "rxEt",
                                        "rxParams", "rxSolve"))
.lintIsSolveName <- function(nm) nm == "solve" || nm %in% .lintSolveMethods
# Arguments of rxSolve() that base::solve() (a, b, tol) does not have.
.lintSolveArgs <- c("events", "params", "inits", "nSub", "nsub", "nStud", "nstud",
                    "omega", "sigma", "thetaMat", "dfSub", "dfObs", "atol",
                    "rtol", "keep", "cores", "seed", "returnType",
                    "addlKeepsCov", "nonmem")
# Calls that mention rxSolve without using it.
.lintInert <- c("?", "help", "args", "formals", "body", "environment")
.lintPipes <- c("%>%", "%T>%", "%<>%", "%!>%")

# `lhs %>% f(args)` is `f(lhs, args)`, or `f(args)` with lhs where a bare `.` is
# an argument; magrittr pipes are rewritten so they resolve like the native pipe
# the parser has already rewritten.
.lintIsPipe <- function(x) {
  is.call(x) && length(x) == 3L && is.symbol(x[[1L]]) &&
    as.character(x[[1L]]) %in% .lintPipes
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

# TRUE when `a` is a function (or purrr ~ lambda) whose value is a model, as in
# function(n) readModelDb(n).
.lintReturnsModel <- function(a, acc) {
  if (!is.call(a)) return(FALSE)
  nm <- .lintCallName(a)
  (identical(nm, "function") && .lintIsModel(a[[3L]], acc)) ||
    (identical(nm, "~") && length(a) == 2L && .lintIsModel(a[[2L]], acc))
}

# Whether every branch of an if/else, switch(), tryCatch()/try() or local() call
# `x` (named `nm`) satisfies `holds`; NULL when `x` is not one of those. An if
# without an else, a switch() with no arms, and a tryCatch() handler that does
# not return such a value fail.
.lintBranchHolds <- function(x, nm, holds) {
  args <- as.list(x)[-1L]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  if (identical(nm, "if")) {
    return(length(args) >= 3L && holds(args[[2L]]) && holds(args[[3L]]))
  }
  if (identical(nm, "switch")) {
    arms <- args[-1L]
    arms <- arms[!vapply(arms, identical, logical(1L), quote(expr = ))]
    return(length(arms) > 0L && all(vapply(arms, holds, logical(1L))))
  }
  if (identical(nm, "local")) return(length(args) >= 1L && holds(args[[1L]]))
  if (nm %in% c("tryCatch", "try", "withCallingHandlers")) {
    value <- args[nms %in% c("", "expr")]
    handlers <- args[!nms %in% c("", "expr", "finally")]
    return(length(value) >= 1L && holds(value[[1L]]) &&
             all(vapply(handlers, function(h) {
               is.call(h) && identical(.lintCallName(h), "function") && holds(h[[3L]])
             }, logical(1L))))
  }
  NULL
}

# TRUE when `x` is a list of models: a name known to hold one, a list()/c() with
# a model (or a list of models) in it, an apply-family call that builds models
# (lapply(names, readModelDb), lapply(names, function(n) readModelDb(n))), or
# setNames()/rev()/... of one.
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
  if (nm %in% .lintContainerWrappers && length(args) >= 1L) {
    return(.lintIsContainer(args[[1L]], acc))
  }
  if (.lintIsApply(nm)) {
    return(any(vapply(args, function(a) {
      (is.symbol(a) && as.character(a) %in% .lintModelMakers) || .lintReturnsModel(a, acc)
    }, logical(1L))))
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
  if (identical(nm, "{") && length(x) >= 2L) return(.lintIsModel(x[[length(x)]], acc))
  branches <- .lintBranchHolds(x, nm, function(e) .lintIsModel(e, acc))
  if (!is.null(branches)) return(branches)
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
  branches <- .lintBranchHolds(x, nm, function(e) .lintIsEvents(e, acc))
  if (!is.null(branches)) return(branches)
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

# TRUE when `x` is obj$name, obj@name or obj[[name]].
.lintIsMemberCall <- function(x) {
  is.call(x) && length(x) == 3L && is.symbol(x[[1L]]) &&
    as.character(x[[1L]]) %in% c("$", "@", "[[")
}

# TRUE when `a` refers to rxSolve as a value: the name, pkg::name, a member of
# an object (asNamespace("rxode2")$rxSolve, ns[["rxSolve"]]), or -- when
# `strings` -- the string (do.call("rxSolve", ...), match.fun("rxSolve")).
.lintIsRxSolveRef <- function(a, strings) {
  for (s in c("rxSolve", "rxSolve.default")) {
    if (is.symbol(a) && identical(a, as.name(s))) return(TRUE)
    if (strings && identical(a, s)) return(TRUE)
    if (.lintIsNsCall(a) && identical(as.character(a[[3L]]), s)) return(TRUE)
    if (.lintIsMemberCall(a) && (identical(a[[3L]], as.name(s)) || identical(a[[3L]], s))) {
      return(TRUE)
    }
  }
  FALSE
}

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

# rxSolve or solve handed to another function as a value. rxSolve (name,
# pkg::name, or the string given to do.call()/match.fun()/get()) is always
# reported; solve is also an ordinary matrix function, so it is reported only
# when match.fun()/get() fetch it by name, or when it travels with a model, a
# list of models, an event table or an rxSolve-only argument
# (lapply(list(mod), solve, ev), do.call(solve, list(mod, ev))). A `solve` the
# article defines at top level is its own function and is not looked for.
.lintIndirectKind <- function(nm, args, acc) {
  # obj$solve, obj[["solve"]] and pkg::solve name a member; they do not hand a
  # function over. A call to a member that is rxSolve is reported with its
  # call (see .lintVisitCall()), and rxSolve as a value at its parent call.
  if (nm %in% c("$", "@", "[[", "::", ":::")) return(NA_character_)
  fetch <- nm %in% c("do.call", "match.fun", "get", "getExportedValue")
  if (any(vapply(args, .lintIsRxSolveRef, logical(1L), strings = fetch))) return("indirect")
  if ("solve" %in% acc$topfuns) return(NA_character_)
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

# Whether an indirect call states the setting: as for a direct call, and also
# in a list() or c() among its arguments (do.call(rxSolve, list(mod, ev,
# addlKeepsCov = TRUE))).
.lintIndirectStates <- function(x) {
  args <- as.list(x)[-1L]
  .lintCallStates(x) || any(vapply(args, function(a) {
    is.call(a) && .lintCallName(a) %in% c("list", "c") &&
      .lintArgsState(as.list(a)[-1L])
  }, logical(1L)))
}

# The text of a `control =` argument that is not an rxControl() or list() call
# (a variable, say), so that the setting inside it cannot be read; NA otherwise.
.lintOpaqueControl <- function(x) {
  args <- as.list(x)[-1L]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  for (a in args[nms == "control"]) {
    if (!(is.call(a) && .lintCallName(a) %in% c("rxControl", "list"))) {
      return(.lintCallText(a, 40L))
    }
  }
  NA_character_
}

# Record a place the model is solved; returns its position in acc$sites. A site
# also keeps what the pairing rule reads (lint-pairing.R): the call, the position
# of the top-level expression holding it (an index into the scan's `exprs`),
# whether it sits inside a function (and that function's name, parameters and
# local variables), the argument that is the model and where that model came
# from.
.lintAddSite <- function(acc, kind, stated, text, opaque = NA_character_,
                         call = NULL, model_arg = NULL) {
  id <- length(acc$sites) + 1L
  origin <- if (is.symbol(model_arg)) acc$origins[[as.character(model_arg)]] else NULL
  acc$sites[[id]] <- list(
    kind = kind, stated = stated, text = text, opaque = opaque, call = call,
    top = if (is.null(acc$top_i)) NA_integer_ else acc$top_i,
    in_fun = !is.null(acc$ctx), fun = acc$ctx$name, params = acc$ctx$params,
    locals = acc$ctx$locals, path = acc$path, model_arg = model_arg,
    origin = if (is.null(origin)) NA_character_ else origin$name,
    modified = !is.null(origin) && isTRUE(origin$modified))
  id
}

# The library name a model expression was read from, and whether the article
# changed its equations: readModelDb("X"), modellib("X"), a name assigned from
# one, or such a call wrapped in ini() / zeroRe() / model() / ... Returns
# list(name = , modified = ), name NA when it is not a literal.
.lintModelOrigin <- function(x, acc) {
  if (.lintIsPipe(x)) x <- .lintDePipe(x)
  unknown <- list(name = NA_character_, modified = FALSE)
  if (is.symbol(x)) {
    o <- acc$origins[[as.character(x)]]
    return(if (is.null(o)) unknown else o)
  }
  if (is.character(x) && length(x) == 1L) return(list(name = x, modified = FALSE))
  if (!is.call(x)) return(unknown)
  nm <- .lintCallName(x)
  args <- as.list(x)[-1L]
  if (nm %in% c("readModelDb", "modellib") && length(args) >= 1L && is.character(args[[1L]])) {
    return(list(name = args[[1L]], modified = FALSE))
  }
  if (identical(nm, "{") && length(x) >= 2L) return(.lintModelOrigin(x[[length(x)]], acc))
  if ((nm %in% .lintModelWrappers || nm %in% .lintEventMakers) && length(args) >= 1L) {
    o <- .lintModelOrigin(args[[1L]], acc)
    if (nm %in% c("model", "rxAppendModel")) o$modified <- TRUE
    return(o)
  }
  unknown
}

# Note what a name is assigned from, for later solve() calls. A name assigned
# something that is none of a model, a list of models or an event table stops
# being tracked -- unless the new value is built from the name itself, as in
# ev <- ev[order(ev$id), ].
.lintRecordAssignment <- function(name, rhs, acc) {
  kind <- if (.lintIsModel(rhs, acc)) "model"
          else if (.lintIsContainer(rhs, acc)) "container"
          else if (.lintIsEvents(rhs, acc)) "events"
          else if (name %in% all.vars(rhs)) "same"
          else "none"
  if (identical(kind, "same")) return(invisible(NULL))
  acc$origins[[name]] <- if (identical(kind, "model")) .lintModelOrigin(rhs, acc) else NULL
  acc$modelnames <- setdiff(acc$modelnames, name)
  acc$containers <- setdiff(acc$containers, name)
  acc$eventnames <- setdiff(acc$eventnames, name)
  if (identical(kind, "model")) acc$modelnames <- c(acc$modelnames, name)
  if (identical(kind, "container")) acc$containers <- c(acc$containers, name)
  if (identical(kind, "events")) acc$eventnames <- c(acc$eventnames, name)
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

# Names that hold a model only inside this call: with(list(m = mod), ...), the
# parameter of a function mapped over a list of models (lapply, purrr, ...), and
# the variable of foreach(m = models) %do% ....
.lintScopedModels <- function(nm, args, acc) {
  if (identical(nm, "with") && length(args) >= 2L && is.call(args[[1L]]) &&
      identical(.lintCallName(args[[1L]]), "list")) {
    bound <- as.list(args[[1L]])[-1L]
    bn <- names(bound)
    if (is.null(bn)) bn <- rep("", length(bound))
    return(bn[nzchar(bn) & vapply(bound, .lintIsModel, logical(1L), acc = acc)])
  }
  if (nm %in% c("%do%", "%dopar%") && length(args) >= 1L && is.call(args[[1L]]) &&
      identical(.lintCallName(args[[1L]]), "foreach")) {
    iter <- as.list(args[[1L]])[-1L]
    bn <- names(iter)
    if (is.null(bn)) bn <- rep("", length(iter))
    return(bn[nzchar(bn) & vapply(iter, .lintIsContainer, logical(1L), acc = acc)])
  }
  if (.lintIsApply(nm) && any(vapply(args, .lintIsContainer, logical(1L), acc = acc))) {
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
  local_solve <- "solve" %in% acc$topfuns
  # asNamespace("rxode2")$rxSolve(mod, ev): the function is fetched from an
  # object, so the call cannot be followed, but its own arguments can state the
  # setting
  member_call <- .lintIsMemberCall(x[[1L]]) && .lintIsRxSolveRef(x[[1L]], strings = TRUE)
  kind <- if (member_call) "indirect"
          else if (nm %in% c("rxSolve", "rxSolve.default")) "rxSolve"
          else if (.lintIsSolveName(nm) && !(identical(nm, "solve") && local_solve)) .lintSolveKind(x, acc)
          else if (nm %in% c("nlmixr2", "nlmixr")) .lintEstKind(args)
          else if (identical(nm, "simulate")) "simulate"
          else .lintIndirectKind(nm, args, acc)
  id <- NA_integer_
  if (!is.na(kind)) {
    stated <- if (identical(kind, "indirect") && !member_call) .lintIndirectStates(x) else .lintCallStates(x)
    model_arg <- NULL
    if (identical(kind, "rxSolve")) {
      for (i in seq_along(args)) {
        if (!identical(args[[i]], quote(expr = )) && .lintIsModel(args[[i]], acc)) {
          model_arg <- args[[i]]
          break
        }
      }
    }
    id <- .lintAddSite(acc, kind, stated, .lintCallText(x),
                       if (kind %in% c("rxSolve", "solve-unresolved")) .lintOpaqueControl(x) else NA_character_,
                       call = x, model_arg = model_arg)
  }
  if (.lintIsSolveName(nm) && !(identical(nm, "solve") && local_solve) &&
      !is.null(acc$ctx) && !identical(kind, "rxSolve")) {
    .lintAddPending(x, id, acc)
  }
  if (nm %in% c("readModelDb", "modellib") && length(args) >= 1L && is.character(args[[1L]])) {
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

# The constructs a site is inside, outermost first, for the pairing rule to
# rebuild the code that runs before it (see .lintSiteTargets()): "block" (the
# statements before it in a `{` block), "for" (the loop variable and what it
# loops over), "function" (a named function's name and formals) and "lambda" (an
# anonymous function's parameters and, when it is handed to lapply() or the
# like, that call).
.lintPushFrame <- function(acc, frame) {
  acc$path[[length(acc$path) + 1L]] <- frame
  invisible(NULL)
}
.lintPopFrame <- function(acc) {
  acc$path[[length(acc$path)]] <- NULL
  invisible(NULL)
}

# Walk a local function's body with its parameters in scope.
.lintWalkFunction <- function(fn, name, acc) {
  outer <- acc$ctx
  acc$ctx <- list(name = name, params = names(fn[[2L]]),
                  locals = setdiff(.lintLocalNames(list(fn[[3L]])), names(fn[[2L]])))
  .lintPushFrame(acc, list(kind = "function", name = name, formals = fn[[2L]]))
  lapply(as.list(fn)[-1L], .lintWalkSolves, acc = acc)
  .lintPopFrame(acc)
  acc$ctx <- outer
  invisible(NULL)
}

# Walk the children of a call, recording the constructs that hold them.
.lintWalkKids <- function(nm, parts, kids, acc) {
  if (identical(nm, "{")) {
    stmts <- parts[-1L]
    for (k in seq_along(stmts)) {
      .lintPushFrame(acc, list(kind = "block", before = stmts[seq_len(k - 1L)]))
      .lintWalkSolves(stmts[[k]], acc)
      .lintPopFrame(acc)
    }
  } else if (identical(nm, "for") && length(parts) == 4L && is.symbol(parts[[2L]])) {
    .lintWalkSolves(parts[[3L]], acc)
    .lintPushFrame(acc, list(kind = "for", var = as.character(parts[[2L]]), over = parts[[3L]]))
    .lintWalkSolves(parts[[4L]], acc)
    .lintPopFrame(acc)
  } else if (identical(nm, "function") && length(parts) >= 3L) {
    ap <- acc$apply_ctx
    acc$apply_ctx <- NULL
    .lintPushFrame(acc, list(kind = "lambda", params = names(parts[[2L]]), apply = ap))
    lapply(kids, .lintWalkSolves, acc = acc)
    .lintPopFrame(acc)
  } else if (.lintIsApplyLike(nm)) {
    # an anonymous function handed to lapply(), map(), Map(), ... : its
    # parameters are bound to the first elements of the other arguments
    is_fn <- vapply(kids, function(k) is.call(k) && identical(.lintCallName(k), "function"), NA)
    for (i in seq_along(kids)) {
      if (is_fn[[i]]) acc$apply_ctx <- list(fn = nm, args = kids[!is_fn])
      .lintWalkSolves(kids[[i]], acc)
      acc$apply_ctx <- NULL
    }
  } else {
    lapply(kids, .lintWalkSolves, acc = acc)
  }
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
  is_top <- identical(x, acc$toplevel)
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
        if (is_top) acc$topfuns <- union(acc$topfuns, as.character(lhs))
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
  # the name itself is not a reference to rxSolve as a value. An argument that
  # is rxSolve itself was reported with its call (see .lintIndirectKind()).
  head_is_name <- is.symbol(parts[[1L]]) || .lintIsNsCall(parts[[1L]])
  kids <- if (head_is_name) parts[-1L] else parts
  kids <- kids[!vapply(kids, .lintIsRxSolveRef, logical(1L), strings = FALSE)]
  .lintWalkKids(nm, parts, kids, acc)
  acc$modelnames <- setdiff(acc$modelnames, scoped)
  invisible(NULL)
}

# Scan the chunks for the places the model is solved. A chunk that does not
# parse but mentions a solver is recorded in acc$unparsed rather than skipped,
# because a solve that cannot be read cannot be shown to state the setting; a
# bare solve( counts as a solver only when the article also works with a model
# (readModelDb(), modellib() or the model's name), since solve() is mostly matrix algebra.
# A chunk with eval = FALSE does not run and is skipped.
.lintScanSolves <- function(chunks, known = character()) {
  acc <- new.env(parent = emptyenv())
  acc$sites <- list()
  acc$models <- character()
  acc$modelnames <- known
  acc$containers <- character()
  acc$eventnames <- character()
  acc$origins <- list()
  acc$path <- list()
  acc$exprs <- list()
  acc$top_i <- NULL
  acc$funs <- list()
  acc$topfuns <- character()
  acc$toplevel <- NULL
  acc$pending <- list()
  acc$ctx <- NULL
  acc$unparsed <- character()
  all_text <- unlist(chunks, use.names = FALSE)
  uses_model <- any(grepl("readModelDb|modellib", all_text)) ||
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
      for (e in as.list(ex)) {
        acc$exprs[[length(acc$exprs) + 1L]] <- e
        acc$top_i <- length(acc$exprs)
        acc$toplevel <- e
        .lintWalkSolves(e, acc)
      }
    }
  }
  list(sites = acc$sites, models = unique(acc$models), unparsed = acc$unparsed,
       exprs = acc$exprs)
}

# The end of every lagged-dose fix: the setting to state and what it needs.
.lintStateTail <- paste0(
  "give every record the covariates of the dose that opened its interval and say ",
  "in the article that you did (addlKeepsCov = TRUE or nonmem = TRUE also pass ",
  "this check, but match NONMEM only for bolus doses)")

# The rule: when the model has alag() and an f() that depends on a covariate,
# every place the article solves the model must state addlKeepsCov or
# nonmem = TRUE. `scan` is .lintScanSolves() of the article; `models` is
# .lintReadModels() of the sources (the `model` argument, else the literal
# names the article passes to readModelDb()). With neither, no model is known
# and the rule cannot apply. A model that is named but cannot be read --
# parsed, or its covariates and statements extracted -- is reported, never
# skipped: a silent skip is how an unchecked article looks like a checked one.
# Returns a list of .lintIssue() rows.
.lintLaggedDose <- function(scan, models) {
  if (!length(scan$sites) && !length(scan$unparsed)) return(list())
  if (!length(models)) return(list())

  iss <- list()
  covs <- character()
  for (m in models) {
    got <- if (is.null(m$ui)) simpleError(m$error)
           else tryCatch(.lintLaggedFCovariates(m$ui), error = function(e) e)
    if (inherits(got, "error")) {
      iss[[length(iss) + 1L]] <- .lintIssue(
        "addlkeepscov-model", "warning",
        paste0("cannot read ", m$label, " (", .lintTrunc(conditionMessage(got), 80L),
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
  why <- paste0("With alag(), which record supplies f()'s covariates depends on the ",
                "setting and the dose type. By default (addlKeepsCov = FALSE) a bolus ",
                "dose reads the record in force when it arrives, so a record at the ",
                "dose's time or inside the lag window that carries another occasion's ",
                "covariates changes the bioavailability, and an infusion (rate = -1, ",
                "rate = -2 or a fixed rate) reads its own record unless another record ",
                "(not a dose into the same compartment) shares its dose time. ",
                "addlKeepsCov = TRUE and nonmem = TRUE read the ",
                "dose's own record for a bolus, which is NONMEM's pairing, but for an ",
                "infusion they do not: a modelled rate reads the record before the ",
                "dose or a record at its time (not a dose into the same compartment), ",
                "and a fixed rate reads a record ",
                "that depends on the layout of the table (the record at time 0, or one ",
                "at or just after its arrival, as far as its amount reaches) and cannot ",
                "be pinned to one. Give every record the ",
                "covariates of the dose that opened its interval, state ",
                "addlKeepsCov = FALSE, and say in the article that you did. TRUE or ",
                "nonmem = TRUE also passes this check, but matches NONMEM only for ",
                "bolus doses.")
  for (s in scan$sites) {
    if (s$stated) next
    if (s$kind == "rxSolve" && is.na(s$opaque)) {
      iss[[length(iss) + 1L]] <- .lintIssue(
        "addlkeepscov-unstated", "warning",
        paste0("`", s$text, "` states neither addlKeepsCov nor nonmem = TRUE, but ",
               what, why),
        paste0("add addlKeepsCov = FALSE to this call (or inside rxControl() given as control =); ",
               .lintStateTail))
    } else if (s$kind == "rxSolve") {
      # the call does not state the setting where it can be read, but its
      # control argument may: that is not known to be unstated
      iss[[length(iss) + 1L]] <- .lintIssue(
        "addlkeepscov-unverifiable", "warning",
        paste0("cannot verify `", s$text, "`: it does not state addlKeepsCov or ",
               "nonmem = TRUE where it can be read, and its control argument, `",
               s$opaque, "`, is a variable or call whose setting could not be read ",
               "here; ", what, why),
        paste0("state addlKeepsCov = FALSE in the call itself, or give control = rxControl(addlKeepsCov = FALSE) inline so it can be read; ",
               .lintStateTail))
    } else if (s$kind == "solve-unresolved") {
      iss[[length(iss) + 1L]] <- .lintIssue(
        "addlkeepscov-unverifiable", "warning",
        paste0("cannot verify `", s$text, "`: it looks like a simulation (solve() ",
               "with event-table-like arguments) but no argument traces to a model ",
               "(a name assigned from readModelDb(), modellib() or rxode2(), or a model()/ini() ",
               "pipe of one), and ", what, why),
        paste0("assign the model from readModelDb(), modellib() or rxode2() and solve it by that name, or put addlKeepsCov = FALSE in this call; ",
               .lintStateTail))
    } else {
      iss[[length(iss) + 1L]] <- .lintIssue(
        "addlkeepscov-unverifiable", "warning",
        paste0("cannot verify `", s$text, "`: it solves the model without a ",
               "rxSolve() call whose arguments can be read, and ", what, why),
        paste0("solve with rxSolve() directly, or put addlKeepsCov = FALSE in this call (for nlmixr2(est = \"rxSolve\"), through rxControl()); ",
               .lintStateTail))
    }
  }
  for (u in scan$unparsed) {
    iss[[length(iss) + 1L]] <- .lintIssue(
      "addlkeepscov-unverifiable", "warning",
      paste0("cannot verify the chunk containing `", u, "`: it mentions a solver ",
             "but does not parse as R, and ", what, why),
      paste0("fix the chunk so it parses, then state addlKeepsCov = FALSE in each solve; ",
             .lintStateTail))
  }
  iss
}

#' Pre-lint a validation vignette for common render-killers
#'
#' Scans the vignette's R chunks (and, when supplied, the model) for the
#' recurring failures that otherwise only surface during an expensive render:
#' an oversized simulation cohort, a missing `cmt =` for an algebraic
#' observable (the most common slot-renumbering failure), a named-vector scalar
#' passed to `amt =`, a PKNCA input filter that drops the `time = 0` row,
#' assertions that cannot hold across machines, a simulation that does not
#' state its lagged-dose covariate convention, and an event table that pairs a
#' lagged dose with another record's covariates (see Details). All rules read
#' the code without running it except the last, which runs the code that builds
#' each event table in a sandbox (see *Lagged-dose pairing*).
#'
#' @details
#' **What is read.** The R chunks of the Rmd, found as knitr finds them: an
#' indented, blockquoted or four-backtick fence is a chunk, and a chunk of
#' another engine is not. Comments are removed before any text rule matches, so
#' a commented-out call or an assertion quoted in a comment is not a finding.
#' A chunk that knitr does not run is not scanned by the lagged-dose rule (the
#' text rules still read it): a literal `FALSE` or `F` as `eval` among the header
#' options outside quoted strings, or a false `eval` among the chunk's leading
#' `#| ` option lines, read as `xfun::divide_chunk()` reads them. The lines need
#' the space after `#|` and must start the chunk. A block whose first line looks
#' like `key: value` is YAML, where `false`, `no`, `n` and `off` (in lower,
#' Capitalised or upper case) are FALSE, `F` and a quoted `"false"` are strings
#' (the chunk runs) and a trailing ` # comment` is not part of the value;
#' otherwise the lines are R options (`eval = FALSE, echo = TRUE`). Not honoured:
#' `eval = c(FALSE)`, `eval = !TRUE`, `eval = <variable>`, and a document-wide
#' `knitr::opts_chunk$set(eval = FALSE)`.
#'
#' **The model.** The `model` argument when given, else the literal names the
#' article passes to `readModelDb()` or `modellib()`. The `cmt =` rule and the lagged-dose rule
#' both read it; a model that is named but cannot be read is reported
#' (`cmt-observable-model`, `addlkeepscov-model`) rather than skipped, but only
#' when the article has something the rule would check (a `cmt =` string, a
#' simulation). With neither a model argument nor a `readModelDb()` or `modellib()` literal, no
#' model is known and those rules do not apply. A model given only as the
#' `model` argument is not a tracked name in the article: `solve(Some_Model, ev)`
#' is not recognised unless the article also passes `"Some_Model"` to
#' `readModelDb()` or `modellib()`.
#'
#' **Lagged-dose bioavailability.** When the model has an `alag()` statement
#' and an `f()` whose right-hand side depends on a covariate (directly, through
#' intermediate assignments, or through an `if()` condition), every simulation
#' in the article -- each `rxSolve()` call and each rxode2 `solve(model, ...)`
#' call -- must state `addlKeepsCov =` or `nonmem = TRUE`. A lagged dose is
#' applied when it arrives, and which record's covariates its `f()` reads then
#' depends on the setting and on the dose type (see *Which setting*), so the
#' result changes when a covariate changes between occasions. The contract is
#' to resolve or report every form the rule can read, and to list here what it
#' cannot.
#'
#' *Which setting.* Measured on rxode2 5.1.8.9000 (commits e4cb67f6 and
#' c865aa8), with a different value on each candidate record: the one before
#' the dose, the dose's own, and one other record at the dose's time or inside
#' the lag window. For a bolus dose, `addlKeepsCov = FALSE` (the default) reads
#' the record in force when the dose arrives, so a record at the dose's time
#' (written before or after the dose) or inside the lag window wins over the
#' dose's own record, while `addlKeepsCov = TRUE` and `nonmem = TRUE` read the
#' dose record's own covariates, which is NONMEM's pairing. For an infusion
#' (`rate = -2` with `dur()`, `rate = -1` with `rate()`, or a fixed rate),
#' `TRUE` and `nonmem = TRUE` never read the dose's own record. `rate = -1` and
#' `-2` read the last record before the dose, or a record at the dose's time
#' other than a dose into the same compartment (an observation, or a dose into
#' another compartment), which then supplies `f()`. A fixed rate (`rate` above
#' 0) reads a record that cannot be pinned to the dose or its neighbours: with
#' records at 0, 10 and 20 and a dose at 24, `f()` came from the record at 0, not
#' the one at 20; with the first record at 5, 3 or 1 it came from the dose's own
#' record; a record at the arrival time (24.5) or just after it (24.7, lag 0.5)
#' supplied it, and one at 26 or 40 did not; how far after the arrival a record
#' reaches depends on the dose's amount. `FALSE` reads the dose's own record for
#' every infusion type unless a record other than a dose into the same
#' compartment shares the dose's time, which then supplies `f()` whatever the row
#' order (a bolus ignores a dose into another compartment at its time, an
#' infusion reads it); a record inside the lag window does not override it. A
#' fixed duration (a `dur` column, no `rate`) reads like a bolus. A modelled
#' `rate()` or `dur()` comes from a record at the dose's time or inside the lag
#' window (not at the arrival time itself) under every setting, except under
#' `nonmem = TRUE` when that record's row is written before the dose: an
#' observation supplies it, so can a dose into another compartment, and a dose
#' into the same compartment does not. Without a lag every setting reads the
#' dose's own record. What is right for every dose type is an event table in
#' which each record carries the covariates of the dose that opened its
#' interval, with `addlKeepsCov = FALSE`; the rule accepts `TRUE` and
#' `nonmem = TRUE` too, but they match NONMEM only for bolus doses. The rule
#' checks that the setting is stated, not the event table.
#'
#' *What counts as a simulation.* `rxSolve()` in any spelling (`rxode2::`,
#' `rxSolve.default`, native or magrittr pipes, arguments over several lines).
#' `solve()` and the methods rxode2 registers (`solve.rxUi()` and the others)
#' count when an argument traces to a model through the article's assignments: a
#' name assigned from `readModelDb()`, `modellib()`, `rxode2()` or `rxode()`; a `model()` /
#' `ini()` / `zeroRe()` pipe of one; the `$ui` of one; an element (`$`, `[[`) of
#' a list of models, including one built by `lapply()`, `purrr::map()` or
#' `setNames()` over their names; the loop variable of a `for` loop over such a
#' list, the parameter of a function mapped over it (`lapply()`, the `purrr`
#' `map` family, `walk()`, ...), or the variable of a `foreach()` loop; a
#' `with(list(m = mod), ...)` binding; the model's library name. A `solve()`
#' inside an article-local function on one of its parameters counts when the
#' function is called with a model there, and is reported at the definition.
#' `base::solve()` on a matrix is left alone, and so is a `solve()` the article
#' defines itself at top level (its own `rxSolve()` call is judged instead; a
#' definition inside a function or `local()` shadows nothing). A name assigned
#' something that is not a model stops being one; an `if`/`else`, `switch()`,
#' `tryCatch()` or `local()` that yields a model in every branch is a model.
#' `mod$simulate()` and `mod$solve()` are not rxode2 methods and are ignored.
#'
#' *What counts as stating the setting.* `addlKeepsCov =` with any value but
#' `NULL` (a variable counts: the author addressed `addlKeepsCov` itself), or a
#' literal `nonmem = TRUE` (or `T`), among the call's own arguments or inside an
#' `rxControl()` (or `control = list()`) argument. `nonmem` counts only as a
#' literal `TRUE`, because it sets `addlKeepsCov` only when it is `TRUE` and a
#' variable may hold `FALSE`; `nonmem = FALSE`, `NULL` and `nonmem = flag` do not
#' count. Arguments of other nested calls do not count either:
#' `foo(addlKeepsCov = TRUE)` inside an `rxSolve()` call is `foo`'s argument.
#' `control = ctl`, with `ctl` defined elsewhere, is not followed, and the call
#' is reported as not verifiable rather than as unstated. For an indirect call
#' (`do.call(rxSolve, list(mod, ev, addlKeepsCov = TRUE))`,
#' `lapply(mods, solve, ev, nonmem = TRUE)`) the setting is looked for among its
#' arguments and in a `list()` or `c()` among them.
#'
#' *Reported as not verifiable* (unless the call itself states the setting): an
#' `rxSolve()` or model `solve()` whose `control =` is a variable or any call
#' other than `rxControl()` / `list()`, since the setting inside it cannot be
#' read; a call to `rxSolve` fetched from an object
#' (`asNamespace("rxode2")$rxSolve(...)`, `ns[["rxSolve"]](...)`);
#' `nlmixr2(..., est = "rxSolve")`, or an `est` that is not a literal;
#' `simulate()`; `rxSolve` or `solve` handed to another function as a value
#' (`do.call()`, `lapply()`, `Map()`, `match.fun()`, ...) -- `solve` only when
#' it travels with a model, a list of models, an event table or an
#' rxSolve-only argument, since `lapply(matrices, solve)` is matrix algebra;
#' a `solve()` with event-table-like arguments (an `et()` call, a data frame of
#' dosing records, a function that builds one, `atol =`, `params =`, ...) but no
#' traceable model; and a chunk that mentions a solver but does not parse (a
#' bare `solve(` counts only when the article also uses `readModelDb()`, `modellib()` or the
#' model's name).
#'
#' *Not resolved and not reported.* Only these forms are silent whatever the
#' arguments: a call built from text (`eval(parse(text = ...))`) and one fetched
#' through a function the rule does not follow
#' (`getFromNamespace("rxSolve", "rxode2")(...)`,
#' `getS3method("solve", "rxUi")(...)`). Others are reported only when their
#' arguments are event-table-like, and are
#' otherwise read as matrix algebra: a model returned by a user function; a
#' model handed to a local function through `lapply()` or `do.call()` rather than
#' called directly; `do.call(solve, args)` with `args` a bare name. A `solve()`
#' whose model cannot be traced and whose arguments are not event-table-like is
#' read as matrix algebra. Names are tracked across the whole article rather than
#' per function. A record built inline with `amt =` or `evid =` counts as an
#' event table even in `solve(rbind(amt = ..., time = ...))`.
#'
#' *Over-approximations* (reported although the rule may not apply, and cleared
#' by stating the setting). `alag()` and `f()` are not tied to the same
#' compartment. Assignments are not ordered: every right-hand side a variable is
#' ever given counts, including one read before it is assigned. An article that
#' uses several models takes the union of their covariates, so a simulation of a
#' model without the hazard is reported when another model in the article has
#' it. Covariates that never change between occasions (weight, sex) are
#' reported like any other; stating `addlKeepsCov = FALSE` is the intended way
#' to clear them. A wrapper that forwards `...` into `rxSolve()` is reported at
#' its definition even when every caller states the setting, since the setting
#' cannot be seen flowing through.
#'
#' **Lagged-dose pairing.** For a model with an `alag()` compartment whose
#' `f()`, `rate()` or `dur()` reads a covariate, the rule reads the event table
#' each `rxSolve()` is given and reports (`lagged-dose-pairing`) every lagged
#' dose for which a record the setting may read the covariates from carries
#' different values than the dose's own record: the records are those in the
#' table in *Which setting* (a record at the dose's time, one inside the lag
#' window, the record before the dose, ...), and a difference in any of them is
#' reported, because which one wins depends on the dose type and the row order.
#' The finding says how many lagged doses are affected and names one: its time
#' and subject, the covariate, both values and the record. The setting is the
#' one the call states as a literal (`addlKeepsCov`, `nonmem`, in an inline
#' `rxControl()` or `control = list()`), `FALSE` when it states none, and all
#' three when it cannot be read (a variable, `tieOrder`). The lag time of each
#' dose is the model's `alag()` evaluated at the initial estimates, with every
#' eta 0 and the dose record's covariates; when that cannot be computed only the
#' records at the dose's own time are checked and the finding says so. A fixed
#' rate under `TRUE` or `nonmem = TRUE` may read any record of the subject, so
#' any difference is reported. The table that makes every `FALSE` setting clean
#' is the one in *Which setting*: each record carries the covariates of the dose
#' that opened its interval.
#'
#' *How the event table is obtained.* The code that builds it is run: the
#' statements before the call that define what the events argument reads (found
#' by name, so a statement that defines nothing needed is not run), the code
#' before the call in the blocks, loops and function it is inside, the
#' parameters of a helper function bound from its top-level calls (its defaults,
#' `...`, and a helper handed to `lapply()` by name included), the first value
#' of a loop variable and the first element of what `lapply()`, `purrr::map()`,
#' `Map()` or `purrr::pmap()` is applied to. It runs in a sandbox: the only
#' functions it can find are the article's own and a fixed list from base, stats,
#' utils, tibble, tidyr, purrr, dplyr and rxode2's event-table builders
#' (`et()`, `add.dosing()`, ...), so a call to anything else (file access,
#' `system()`, `eval()`, `readModelDb()`, `rxSolve()`, a model) is refused before
#' anything runs, whatever spelling reaches it, and is reported as not
#' verifiable. A run is limited to 20 seconds and 3 million rows, an article to
#' 20 simulations, and the session's random-number stream is put back
#' afterwards. This guards an article's own code against its accidents; it is
#' not a boundary against hostile code.
#'
#' *Reported as not verifiable* (`lagged-dose-pairing-unverifiable`, one finding
#' per reason naming the first simulation and counting the rest): an event table
#' built with a call the sandbox does not run, which includes any dependence on
#' the model object (`readModelDb()`, `rxode2()`, `zeroRe()`, `ui$omega`); a
#' table with `addl` or steady-state (`ss`) doses; a table that is not a data
#' frame or event table, has no numeric `time` column or is empty; a build that
#' fails or runs over the limits; a simulation that is not an `rxSolve()` or
#' `solve(model, ...)` call whose arguments can be read (`do.call()`,
#' `nlmixr2()`, `simulate()`); a helper the article never calls at the top level,
#' or an anonymous function whose parameter is not one the rule binds; the model
#' changed by `model()` in the article; and, with several models that have a
#' lagged dose, a simulation whose model cannot be told. A simulation of another
#' library model (read from a different literal name) is not about the lagged
#' dose and is skipped.
#'
#' *What it does not check.* Doses expanded by `addl`, because the pairing of an
#' expanded dose was not measured. Next to another dose the measurements are
#' partial (a bolus's `f()` and a dose into another compartment): a dose at the
#' dose's time or inside the lag window is a candidate for every quantity and a
#' dose at its time makes every later record a candidate for `rate()` and
#' `dur()`. A loop or an apply-family function is run for its first element
#' only. Etas are 0 in the lag time, so a lag longer than the dosing interval
#' that only some subjects reach is not seen. The compartment of a numeric `cmt`
#' is its position among the model's states. A covariate the table does not
#' carry as a column (an `iCov`, a constant) cannot differ between records and
#' is not compared.
#'
#' *Over-approximations.* Every record a dose may read counts, so a table whose
#' last-written record in the window carries the dose's own covariates is still
#' reported when an earlier one in the window does not. A typical-value solve
#' (`zeroRe()`) of a model whose covariate acts through an eta is reported
#' although its effect is nil. Covariates that never change between occasions
#' are never a difference, so they are not reported.
#'
#' @param rmd Path to the vignette `.Rmd`.
#' @param model Optional `rxUi` / model function / `nlmixr2lib` name; enables the
#'   algebraic-observable `cmt =` check and the lagged-dose check. Both fall back
#'   to the names the vignette passes to `readModelDb()` or `modellib()` when no
#'   model is given.
#' @param max_per_arm Maximum simulated participants per arm (default 200).
#' @param pairing Whether to run the lagged-dose pairing rule, which runs the
#'   code that builds each simulation's event table in a sandbox (default `TRUE`,
#'   or the option `nlmixr2libingest.lint_pairing`). `FALSE` leaves the other
#'   rules as they are.
#' @return An `nli_vignette_lint` list with `issues` (data frame) and `n`.
#' @export
lint_vignette <- function(rmd, model = NULL, max_per_arm = 200L,
                          pairing = getOption("nlmixr2libingest.lint_pairing", TRUE)) {
  if (!file.exists(rmd)) {
    return(structure(list(
      issues = .lintIssue("file", "error", paste0("vignette not found: ", rmd),
                          "check the path"), n = 1L),
      class = "nli_vignette_lint"))
  }
  # The code of the R chunks with comments removed, so that a commented-out call
  # or a comment that quotes an old assertion does not match a rule.
  chunks <- .lintRchunks(rmd)
  code <- .lintCode(chunks)
  blob <- paste(code, collapse = "\n")
  iss <- list()

  # The models the model-dependent rules read: the `model` argument, else the
  # literal names the article passes to readModelDb(). They are read once, and
  # only when a rule needs them.
  known <- if (is.character(model) && length(model) == 1L) model else character()
  scan <- .lintScanSolves(chunks, known)
  srcs <- if (is.null(model)) as.list(scan$models) else list(model)
  has_cmt <- grepl("cmt\\s*=\\s*[\"']", blob)
  has_solve <- length(scan$sites) > 0L || length(scan$unparsed) > 0L
  models <- if (length(srcs) && (has_cmt || has_solve)) .lintReadModels(srcs) else list()

  # 1. Event table referencing an algebraic observable as a compartment.
  # Per the nlmixr2 convention, event-table `cmt=` must name an ODE STATE, never
  # an algebraic observable: referencing the observable makes rxode2 inject a
  # `cmt()` slot for it AFTER the ODE states, renumbering every compartment --
  # the most common slot-renumbering render failure. rxode2 computes the
  # observable (e.g. `Cc <- central / vc`) at the state's observation rows. A
  # model that cannot be read is reported, not skipped: without it the rule
  # cannot tell an observable from a state.
  if (has_cmt) {
    for (m in models) {
      if (is.null(m$ui)) {
        iss[[length(iss) + 1L]] <- .lintIssue(
          "cmt-observable-model", "warning",
          paste0("cannot read ", m$label, " (", .lintTrunc(m$error, 80L),
                 "), so whether an event table's cmt= names an algebraic ",
                 "observable was not checked"),
          "install rxode2 and nlmixr2lib, or pass the model's .R path as the second argument, so the model can be read")
        next
      }
      obs <- .lintObservables(m$ui)
      used <- obs[vapply(obs, function(o)
        grepl(sprintf("cmt\\s*=\\s*[\"']%s[\"']", o), blob), logical(1L))]
      if (length(used)) {
        states <- tryCatch(as.character(m$ui$state), error = function(e) character())
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
    # Match against CODE only (`blob` has its comments removed). A correct fix
    # documents the old assertion in a comment ("an earlier revision used
    # `all(diff(pv) < 0)` ..."), and matching raw text would re-flag the very
    # vignette that was just repaired -- a linter that fires on the fixed
    # version teaches authors to ignore it.
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
      if (grepl(f[[2L]], blob, perl = TRUE)) {
        iss[[length(iss) + 1L]] <- .lintIssue(f[[1L]], "warning", f[[3L]], f[[4L]])
      }
    }
  }

  # 6. Lagged-dose f() covariates: the solve must say which covariate
  #    convention it uses (see .lintLaggedDose()).
  iss <- c(iss, .lintLaggedDose(scan, models))

  # 7. Lagged-dose pairing: the event table each simulation is given, checked
  #    against the covariates the lagged doses' f() / rate() / dur() read (see
  #    .lintPairing()). It runs the code that builds the event table, in a
  #    sandbox, so it can be turned off.
  if (isTRUE(pairing)) iss <- c(iss, .lintPairing(scan, models))

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
