# Restricted evaluation of the code that builds an article's event table.
#
# The lagged-dose pairing rule (see lint-pairing.R) needs the event table a
# simulation is given, and the articles build it with dplyr / tidyr pipelines and
# helper functions, so it has to be computed. Only the backward slice of the
# events argument is run (.lintSlice()), and it is run so that it can neither
# touch the machine nor reach the model or the solver:
#
#   * every function the code can call is looked up in an environment whose
#     parent is emptyenv() and which holds only the names listed below, so a
#     function that is not listed is not found, whatever spelling reaches it
#     (a bare name, pkg::name, do.call("name", ...), lapply(x, "name"));
#   * `::` is replaced by a function that returns a listed function and stops on
#     anything else, and `:::`, get(), eval(), parse(), environment() and the
#     other ways of reaching a name by value are not listed;
#   * before anything runs, the slice's calls are checked against the same list
#     so that the finding can name the call that is not allowed;
#   * the run has a time limit and a row cap, and the session's RNG state is put
#     back afterwards (set.seed() and the r*() draws are allowed).
#
# A slice that cannot be run is reported to the caller as not verifiable with the
# reason; it is never skipped.

# Names the sandbox exposes, by the package they come from. Later entries mask
# earlier ones, as they do on a search path with dplyr attached last. Everything
# here is a pure function of its arguments: no file, connection, environment,
# option, process or network access, no code from a string, and nothing that runs
# a model.
.lintSandboxSyntax <- c(
  "<-", "=", "{", "(", "if", "for", "while", "repeat", "break", "next",
  "function", "return", "+", "-", "*", "/", "^", "%%", "%/%", "%in%", "%o%",
  "%*%", "==", "!=", "<", ">", "<=", ">=", "!", "&", "&&", "|", "||", ":", "~",
  "[", "[[", "$", "[<-", "[[<-", "$<-", "names<-", "dim<-", "dimnames<-",
  "colnames<-", "rownames<-", "levels<-", "attr<-", "class<-", "is.na<-",
  "diag<-", "substr<-"
)
.lintSandboxBase <- c(
  # construction and shape
  "c", "list", "vector", "numeric", "integer", "character", "logical", "double",
  "rep", "rep_len", "rep.int", "seq", "seq_len", "seq_along", "seq.int", "rev",
  "sort", "order", "rank", "unique", "duplicated", "append", "head", "tail",
  "matrix", "array", "t", "diag", "cbind", "rbind", "data.frame",
  "as.data.frame", "expand.grid", "merge", "split", "unsplit", "unlist",
  "lapply", "sapply", "vapply", "Map", "mapply", "Filter", "Reduce", "Position",
  "Find", "do.call", "outer", "tapply", "apply", "sweep", "scale", "aggregate",
  "within", "with", "transform", "subset", "stack", "setNames", "unname",
  "names", "colnames", "rownames", "dimnames", "dim", "nrow", "ncol", "NROW",
  "NCOL", "length", "nlevels", "levels", "droplevels", "factor", "as.factor",
  "table", "tabulate", "cut", "findInterval", "interaction", "rowSums",
  "colSums", "rowMeans", "colMeans", "max.col", "which", "which.min",
  "which.max", "match", "setdiff", "union", "intersect", "is.element", "rle",
  "inverse.rle", "modifyList", "mapply", "rapply", "nchar", "nlevels",
  # arithmetic and maths
  "sum", "prod", "mean", "median", "var", "sd", "min", "max", "range", "pmin",
  "pmax", "abs", "sqrt", "exp", "log", "log10", "log2", "log1p", "expm1",
  "floor", "ceiling", "round", "signif", "trunc", "sin", "cos", "tan", "asin",
  "acos", "atan", "atan2", "sinh", "cosh", "tanh", "choose", "factorial",
  "gamma", "lgamma", "beta", "cumsum", "cumprod", "cummax", "cummin", "diff",
  "crossprod", "tcrossprod", "chol", "det", "quantile", "cor", "rowsum",
  # testing and conversion
  "identical", "all.equal", "isTRUE", "isFALSE", "is.na", "is.null",
  "is.numeric", "is.character", "is.logical", "is.integer", "is.function",
  "is.data.frame", "is.list", "is.finite", "is.infinite", "is.nan", "is.factor",
  "is.vector", "is.matrix", "any", "all", "anyNA", "ifelse", "switch", "xor",
  "complete.cases", "as.numeric", "as.integer", "as.character", "as.logical",
  "as.vector", "as.list", "as.matrix", "as.double",
  # text
  "paste", "paste0", "sprintf", "format", "formatC", "toupper", "tolower",
  "substr", "substring", "strsplit", "gsub", "sub", "grepl", "grep", "regexpr",
  "gregexpr", "regmatches", "startsWith", "endsWith", "trimws", "toString",
  "sprintf", "rev", "nchar", "chartr", "casefold", "make.names",
  "make.unique",
  # control and conditions
  "stop", "warning", "tryCatch", "try", "suppressWarnings", "suppressMessages",
  "invisible", "identity", "missing", "stopifnot", "Negate", "Vectorize",
  "Recall", "nargs", "%||%", "set.seed", "sample", "sample.int",
  # constants
  "T", "F", "pi", "letters", "LETTERS", "month.name", "month.abb"
)
.lintSandboxStats <- c(
  "rnorm", "runif", "rbinom", "rlnorm", "rpois", "rexp", "rgamma", "rbeta", "rt",
  "rchisq", "rweibull", "rcauchy", "rgeom", "rnbinom", "rmultinom", "rlogis",
  "dnorm", "pnorm", "qnorm", "dlnorm",
  "plnorm", "qlnorm", "dbinom", "pbinom", "qbinom", "dpois", "ppois", "qpois",
  "dexp", "pexp", "qexp", "dgamma", "pgamma", "qgamma", "dbeta", "pbeta",
  "qbeta", "dt", "pt", "qt", "dchisq", "pchisq", "qchisq", "dunif", "punif",
  "qunif", "dweibull", "pweibull", "qweibull", "approx", "approxfun", "setNames",
  "median", "var", "sd", "quantile", "cor", "ave", "aggregate", "weighted.mean",
  "complete.cases", "na.omit", "reshape", "cov", "mad", "IQR", "fivenum",
  "filter", "lag"
)
.lintSandboxUtils <- c("head", "tail", "modifyList", "stack", "combn")
.lintSandboxTibble <- c(
  "tibble", "tribble", "as_tibble", "add_row", "add_column", "enframe",
  "deframe", "lst", "rownames_to_column", "column_to_rownames", "new_tibble"
)
.lintSandboxTidyr <- c(
  "crossing", "expand_grid", "expand", "nesting", "complete", "fill",
  "pivot_longer", "pivot_wider", "unnest", "nest", "separate", "unite",
  "replace_na", "drop_na", "uncount", "full_seq", "unnest_longer",
  "unnest_wider", "separate_wider_delim", "everything", "all_of", "any_of",
  "starts_with", "ends_with", "contains", "matches", "where", "last_col"
)
.lintSandboxPurrr <- c(
  "map", "map_dbl", "map_int", "map_chr", "map_lgl", "map_dfr", "map_dfc", "map2",
  "map2_dbl", "map2_int", "map2_chr", "map2_dfr", "pmap", "pmap_dbl", "pmap_dfr",
  "imap", "imap_dfr", "reduce", "accumulate", "keep", "discard", "compact",
  "flatten", "pluck", "list_rbind", "list_cbind", "set_names", "partial",
  "negate", "modify", "modify_at", "map_if", "map_at", "detect", "every", "some",
  "list_flatten", "list_c", "list_simplify", "pmap_chr", "pmap_int", "map_vec",
  "map2_vec", "reduce2", "transpose", "array_branch", "cross", "cross2",
  "cross_df", "as_mapper"
)
.lintSandboxDplyr <- c(
  "mutate", "transmute", "filter", "select", "arrange", "desc", "group_by",
  "ungroup", "summarise", "summarize", "reframe", "n", "row_number", "left_join",
  "right_join", "inner_join", "full_join", "cross_join", "anti_join",
  "semi_join", "bind_rows", "bind_cols", "distinct", "slice", "slice_head",
  "slice_tail", "slice_min", "slice_max", "slice_sample", "rowwise", "rename",
  "rename_with", "relocate", "pull", "count", "add_count", "tally", "coalesce",
  "if_else", "case_when", "case_match", "between", "lag", "lead", "first", "last",
  "nth", "across", "everything", "all_of", "any_of", "starts_with", "ends_with",
  "contains", "matches", "where", "pick", "group_split", "group_keys",
  "group_vars", "do", "na_if", "n_distinct", "min_rank", "dense_rank",
  "cur_group_id", "cur_group", "cur_data", "consecutive_id", "nest_by",
  "pull", "if_any", "if_all", "row_number", "ntile", "percent_rank", "cume_dist",
  "lst", "tibble", "as_tibble", "data_frame", "near", "n_groups", "last_col",
  "num_range", "one_of", "%>%", "mutate_at", "mutate_all", "mutate_if",
  "select_at", "rename_at", "summarise_at", "summarise_all", "arrange_at",
  "filter_at", "group_by_at", "transmute_at"
)
# rxode2's event-table builders; they construct a table and run nothing.
.lintSandboxRxode2 <- c("et", "etRep", "etSeq", "etRbind", "etExpand", "add.dosing", "add.sampling")

.lintSandboxPackages <- list(
  base = c(.lintSandboxBase, .lintSandboxSyntax),
  stats = .lintSandboxStats,
  utils = .lintSandboxUtils,
  tibble = .lintSandboxTibble,
  tidyr = .lintSandboxTidyr,
  purrr = .lintSandboxPurrr,
  dplyr = .lintSandboxDplyr,
  rxode2 = .lintSandboxRxode2
)

# The packages `pkg::name` may name, and what is left out on purpose.
.lintSandboxOrder <- c("base", "stats", "utils", "tibble", "tidyr", "purrr",
                       "dplyr", "rxode2")

# Functions the models' lag expressions use that rxode2 defines.
.lintSandboxModelExtras <- list(
  `%||%` = function(x, y) if (is.null(x)) y else x,
  expit = function(x) 1 / (1 + exp(-x)),
  logit = function(x) log(x / (1 - x)),
  probitInv = function(x) stats::pnorm(x),
  probit = function(p) stats::qnorm(p)
)

# Whether `pkg::name` may be called: the package's own list, and the package
# installed (a name from a package that is not installed is looked up as absent).
.lintSandboxAllows <- function(pkg, name) {
  pkg %in% names(.lintSandboxPackages) && name %in% .lintSandboxPackages[[pkg]]
}

# The namespace (or base environment) a package's names are fetched from, or
# NULL when the package is not installed.
.lintSandboxNamespace <- function(pkg) {
  if (identical(pkg, "base")) return(baseenv())
  if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
  asNamespace(pkg)
}

# The function a sandbox name stands for, or NULL when the package that would
# supply it is not installed or does not export it. A package's exports include
# what it re-exports (dplyr::all_of() is tidyselect's), which its namespace
# environment does not hold.
.lintSandboxFetch <- function(pkg, name, ns = .lintSandboxNamespace(pkg)) {
  if (is.null(ns)) return(NULL)
  if (identical(pkg, "base")) {
    return(if (exists(name, envir = ns, inherits = FALSE)) get(name, envir = ns) else NULL)
  }
  tryCatch(getExportedValue(pkg, name), error = function(e) NULL)
}

# `::` for the sandbox: returns a listed function, stops on any other.
.lintSandboxColon <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  if (!.lintSandboxAllows(pkg, name)) {
    stop(sprintf("`%s::%s` is not allowed in the sandbox", pkg, name), call. = FALSE)
  }
  fn <- .lintSandboxFetch(pkg, name)
  if (is.null(fn)) {
    stop(sprintf("`%s::%s` is not available (package %s is not installed)", pkg, name, pkg),
         call. = FALSE)
  }
  fn
}

# The environment the code runs in: the listed names bound in an environment
# whose parent is emptyenv(). The R object `.lintSandboxColon` is defined in this
# package, but only the function it returns reaches the code.
.lintSandboxEnv <- function() {
  wl <- new.env(parent = emptyenv())
  spaces <- lapply(stats::setNames(.lintSandboxOrder, .lintSandboxOrder), .lintSandboxNamespace)
  for (pkg in .lintSandboxOrder) {
    for (nm in .lintSandboxPackages[[pkg]]) {
      fn <- .lintSandboxFetch(pkg, nm, spaces[[pkg]])
      if (!is.null(fn)) assign(nm, fn, envir = wl)
    }
  }
  # Syntax is bound from base even where a later package lists the same name.
  for (nm in .lintSandboxSyntax) {
    fn <- .lintSandboxFetch("base", nm, baseenv())
    if (!is.null(fn)) assign(nm, fn, envir = wl)
  }
  for (nm in names(.lintSandboxModelExtras)) {
    assign(nm, .lintSandboxModelExtras[[nm]], envir = wl)
  }
  assign("::", .lintSandboxColon, envir = wl)
  wl
}

# --- slicing -------------------------------------------------------------------

# The arguments of a call (or the elements of a formals list) without the empty
# ones, which `x[1, ]` and `function(x, y = )` have and which cannot be held in a
# variable.
.lintKids <- function(e) {
  kids <- as.list(e)
  empty <- vapply(seq_along(kids), function(i) identical(kids[[i]], quote(expr = )), NA)
  kids[!empty]
}

# The name a replacement target is stored in: names(x) <- v, x$a$b <- v and
# x[[i]] <- v all assign to x.
.lintAssignTarget <- function(lhs) {
  while (is.call(lhs) && length(lhs) >= 2L) lhs <- lhs[[2L]]
  if (is.symbol(lhs)) as.character(lhs) else NA_character_
}

# Names a statement binds (or rebinds) at the top level. A compound statement
# (an if, for, while or a braced block) binds every name assigned anywhere in it.
# Assignments inside a function body are local to it and bind nothing; a loop
# variable is bound by its loop. `acc$out` collects the names.
.lintBindsWalk <- function(e, acc) {
  if (!is.call(e)) return(invisible(NULL))
  op <- if (is.symbol(e[[1L]])) as.character(e[[1L]]) else ""
  if (op %in% c("<-", "=", "<<-") && length(e) == 3L) {
    nm <- .lintAssignTarget(e[[2L]])
    if (!is.na(nm)) acc$out <- c(acc$out, nm)
    .lintBindsWalk(e[[3L]], acc)
  } else if (op == "->" && length(e) == 3L) {
    nm <- .lintAssignTarget(e[[3L]])
    if (!is.na(nm)) acc$out <- c(acc$out, nm)
  } else if (op == "for" && length(e) == 4L) {
    acc$out <- c(acc$out, as.character(e[[2L]]))
    .lintBindsWalk(e[[3L]], acc)
    .lintBindsWalk(e[[4L]], acc)
  } else if (!op %in% c("function", "local")) {
    for (el in .lintKids(e)[-1L]) .lintBindsWalk(el, acc)
  }
  invisible(NULL)
}
.lintBinds <- function(x) {
  acc <- new.env(parent = emptyenv())
  acc$out <- character()
  .lintBindsWalk(x, acc)
  unique(acc$out)
}

# Every name in an expression, function names and the default values of a
# function's parameters included (all.names() skips the defaults, so a constant
# used as `function(n = n_arm)` would not be found).
.lintNames <- function(e) {
  if (is.symbol(e)) return(as.character(e))
  if (is.call(e) || is.pairlist(e)) {
    return(unique(unlist(lapply(.lintKids(e), .lintNames), use.names = FALSE)))
  }
  character()
}

# Names a statement reads, function names included, so that a helper the code
# calls is pulled into the slice with the variables it reads.
.lintReads <- function(x) {
  # `x <- value` reads what `value` reads, not `x`; a replacement
  # (`x$a <- value`, `names(x) <- value`) reads its target too.
  if (is.call(x) && is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("<-", "=") &&
      length(x) == 3L && is.symbol(x[[2L]])) {
    return(.lintNames(x[[3L]]))
  }
  .lintNames(x)
}

# Whether a statement rebinds `nm` without reading it: `nm <- <expression
# without nm>`. Such a statement hides every earlier binding of `nm`.
.lintKills <- function(x, nm) {
  is.call(x) && is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("<-", "=") &&
    length(x) == 3L && is.symbol(x[[2L]]) && identical(as.character(x[[2L]]), nm) &&
    !nm %in% .lintNames(x[[3L]])
}

# The statements before position `top` that `wanted` (an expression) depends on,
# in their original order. Straight-line backward slicing: walking back from the
# statement before `top`, a statement is kept when it binds a name still wanted;
# it then wants what it reads, and a plain rebinding stops wanting the name it
# rebinds. Calls to set.seed() are kept, so that random draws repeat the
# article's own stream when the draws are in the slice.
.lintSlice <- function(exprs, top, wanted) {
  need <- .lintReads(wanted)
  keep <- logical(top - 1L)
  for (k in rev(seq_len(top - 1L))) {
    s <- exprs[[k]]
    bound <- .lintBinds(s)
    seed <- is.call(s) && identical(.lintCallName(s), "set.seed")
    if (seed || length(intersect(bound, need))) {
      keep[[k]] <- TRUE
      hit <- intersect(bound, need)
      for (nm in hit) if (.lintKills(s, nm)) need <- setdiff(need, nm)
      need <- union(need, .lintReads(s))
    }
  }
  exprs[which(keep)]
}

# --- the check before the run ----------------------------------------------------

# Names a slice defines for itself: assigned names, function parameters, loop
# variables. A call to one of them is the code's own. `acc$out` collects them.
.lintLocalWalk <- function(e, acc) {
  if (is.pairlist(e)) {
    acc$out <- c(acc$out, names(e))
    for (el in .lintKids(e)) .lintLocalWalk(el, acc)
    return(invisible(NULL))
  }
  if (!is.call(e)) return(invisible(NULL))
  op <- if (is.symbol(e[[1L]])) as.character(e[[1L]]) else ""
  if (op %in% c("<-", "=", "<<-") && length(e) == 3L) {
    nm <- .lintAssignTarget(e[[2L]])
    if (!is.na(nm)) acc$out <- c(acc$out, nm)
  } else if (op == "for" && length(e) == 4L) {
    acc$out <- c(acc$out, as.character(e[[2L]]))
  } else if (op == "function") {
    acc$out <- c(acc$out, names(e[[2L]]))
  }
  for (el in .lintKids(e)) .lintLocalWalk(el, acc)
  invisible(NULL)
}
.lintLocalNames <- function(stmts) {
  acc <- new.env(parent = emptyenv())
  acc$out <- character()
  for (s in stmts) .lintLocalWalk(s, acc)
  unique(acc$out)
}

# Collect into `acc$bad` the functions an expression calls that the sandbox does
# not allow: the called name of every call (`f(...)`, `pkg::f(...)`), minus
# `local`, the slice's own names.
.lintCallsWalk <- function(e, local, acc) {
  if (is.pairlist(e)) {
    for (el in .lintKids(e)) .lintCallsWalk(el, local, acc)
    return(invisible(NULL))
  }
  if (!is.call(e)) return(invisible(NULL))
  # pkg::name used as a value (lapply(x, dplyr::mutate)) is checked like a call
  if (.lintIsNsCall(e)) {
    pkg <- as.character(e[[2L]])
    nm <- as.character(e[[3L]])
    op <- as.character(e[[1L]])
    if (!.lintSandboxAllows(pkg, nm) || !identical(op, "::")) acc$bad <- c(acc$bad, paste0(pkg, op, nm))
    return(invisible(NULL))
  }
  head <- e[[1L]]
  if (is.symbol(head)) {
    nm <- as.character(head)
    ok <- nm %in% local || any(vapply(.lintSandboxPackages, function(p) nm %in% p, NA))
    if (!ok) acc$bad <- c(acc$bad, nm)
  } else if (.lintIsNsCall(head)) {
    pkg <- as.character(head[[2L]])
    nm <- as.character(head[[3L]])
    op <- as.character(head[[1L]])
    if (!.lintSandboxAllows(pkg, nm) || !identical(op, "::")) {
      acc$bad <- c(acc$bad, paste0(pkg, op, nm))
    }
  } else {
    .lintCallsWalk(head, local, acc)
  }
  for (el in .lintKids(e)[-1L]) .lintCallsWalk(el, local, acc)
  invisible(NULL)
}

# The functions a slice calls that the sandbox does not allow, as "name" or
# "pkg::name"; empty when every call is listed.
.lintDisallowedCalls <- function(stmts) {
  local <- .lintLocalNames(stmts)
  acc <- new.env(parent = emptyenv())
  acc$bad <- character()
  for (s in stmts) .lintCallsWalk(s, local, acc)
  unique(acc$bad)
}

# --- the run -------------------------------------------------------------------

# Evaluate `target` (an expression) after `stmts` in the sandbox. Returns
# list(ok = TRUE, value = <the value>) or list(ok = FALSE, reason = <why the code
# was not run or failed>). `seconds` bounds the run; `max_rows` bounds the number
# of rows of a data frame result.
.lintSandboxRun <- function(stmts, target, seconds = 20, max_rows = 3000000L,
                            wl = .lintSandboxEnv()) {
  bad <- .lintDisallowedCalls(c(stmts, list(target)))
  if (length(bad)) {
    return(list(ok = FALSE, reason = paste0(
      "its event table is built with ", paste0("`", utils::head(bad, 3L), "`", collapse = ", "),
      if (length(bad) > 3L) " and others", ", which the pre-lint does not run")))
  }
  env <- new.env(parent = wl)
  seed_exists <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (seed_exists) seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    setTimeLimit(elapsed = Inf, transient = FALSE)
    if (seed_exists) assign(".Random.seed", seed, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  res <- tryCatch({
    setTimeLimit(elapsed = seconds, transient = TRUE)
    suppressWarnings(suppressMessages({
      for (s in stmts) eval(s, envir = env)
      value <- eval(target, envir = env)
    }))
    setTimeLimit(elapsed = Inf, transient = FALSE)
    list(ok = TRUE, value = value)
  }, error = function(e) {
    setTimeLimit(elapsed = Inf, transient = FALSE)
    msg <- conditionMessage(e)
    list(ok = FALSE, reason = if (grepl("time limit", msg, fixed = TRUE)) {
      sprintf("its event table took longer than %g seconds to build", seconds)
    } else {
      paste0("building its event table failed (", .lintTrunc(msg, 90L), ")")
    })
  })
  if (isTRUE(res$ok) && is.data.frame(res$value) && nrow(res$value) > max_rows) {
    return(list(ok = FALSE, reason = sprintf(
      "its event table has %d rows (the pre-lint stops at %d)", nrow(res$value),
      as.integer(max_rows))))
  }
  res
}
