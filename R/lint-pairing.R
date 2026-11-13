# The lagged-dose pairing rule: for each dose that alag() delays, which record of
# the event table supplies the covariates its f() (and rate() / dur()) read, and
# does that record carry the dose's own covariates?
#
# lint.R's rule asks that a simulation state its addlKeepsCov setting; this one
# reads the event table the simulation is given and reports the doses whose
# covariates the setting would take from another record. The pairing below was
# measured on rxode2 5.1.8.9000, with a distinct covariate value on every record
# so that a result names the record it came from. A record is a row of the
# subject's event table (observation or dose); "own" is the dose's row. With
# alag() = 0 every setting reads the dose's own row.
#
#   f()                     addlKeepsCov = FALSE         TRUE, nonmem = TRUE
#   bolus, fixed dur        a record at the dose's time  the dose's own row
#                           or up to and including the
#                           arrival time (t + lag)
#   fixed rate (rate > 0)   a record at the dose's time  none of a fixed layout:
#                                                        the record at time 0, or
#                                                        one at or just after the
#                                                        arrival, or its own row
#   rate = -1, rate = -2    a record at the dose's time  a record at the dose's
#                                                        time, the last record
#                                                        before it, or, for a
#                                                        subject's first record,
#                                                        the next one inside the
#                                                        lag window
#   rate() / dur()          an observation at the dose's time or before the
#   (rate = -1, -2)         arrival (the arrival itself does not count; a dose
#                           inside the window does not), under every setting,
#                           except that nonmem = TRUE ignores a record written
#                           before the dose in the data
#
# Where more than one record could win (several at the dose's time, several in
# the window) which one does depends on the dose type and the row order, so the
# rule treats every record the dose may read as a candidate and reports a
# difference from the dose's own covariates in any of them. For a fixed-rate
# dose under TRUE or nonmem = TRUE every other record of the subject is a
# candidate, because no layout of the table pins which record it reads.
# NA covariates are filled forward from the previous record before they are
# compared, as the solver does; a record that is still NA never differs.

# The rows (other than `i`) of one subject's records that dose `i` may read.
# `time` is the subject's record times in data order; `lag` is the lag time (NA
# when it could not be computed, in which case the window is every record up to
# `horizon`, the next dose); `qty` is "f" or "rate"; `type` is "bolus", "dur", "fixed" or
# "modelled"; `setting` is "FALSE", "TRUE" or "nonmem"; `dose` says which records
# are doses (default: none, so every other record is an observation) and `same`
# which of them go into the dose's own compartment (default: every dose).
#
# The cells follow the measurements in the header of this file. The behaviour
# next to another dose was only measured for a bolus's f() and for a dose into
# another compartment, so the rule is wider there: a dose at the dose's time or
# inside the lag window is a candidate for every quantity, a dose at the dose's
# time also makes the record before the dose a candidate for an infusion's f() if
# it goes into the dose's own compartment, and makes the next record after it a
# candidate for rate() / dur().
.lintPairCandidates <- function(time, i, lag, qty, type, setting,
                                dose = rep(FALSE, length(time)), horizon = Inf,
                                same = dose) {
  if (!is.na(lag) && lag <= 0) return(integer())
  others <- setdiff(seq_along(time), i)
  t <- time[[i]]
  at <- others[time[others] == t]
  known <- !is.na(lag)
  # the window ends at the arrival time; when the lag is not known it ends at the
  # next dose (`horizon`), so every record a lag shorter than the dosing interval
  # could reach is a candidate
  end <- if (known) t + lag else horizon
  # a record at the arrival time counts for a bolus's f(); the next dose's own
  # record, where the window ends, does not
  closed <- if (known) others[time[others] > t & time[others] <= end]
            else others[time[others] > t & time[others] < end]
  open <- others[time[others] > t & time[others] < end]
  # a dose recorded earlier that is still in its lag when this dose is recorded
  # arrives after it, and can supply it
  early <- if (known) others[dose[others] & time[others] < t & time[others] + lag > t] else integer()
  before <- others[time[others] < t]
  prev <- if (length(before)) before[time[before] == max(time[before])] else integer()
  if (identical(qty, "f")) {
    return(unique(switch(
      type,
      bolus = , dur = if (identical(setting, "FALSE")) c(at, closed, early) else integer(),
      fixed = if (identical(setting, "FALSE")) c(at, if (any(same[at])) prev, early) else others,
      # a modelled rate reads the record at its time, else the record before it,
      # and the next record inside the window when it is the subject's first; a
      # dose at its time into the same compartment leaves the record before it
      # (one into another compartment supplies like an observation)
      modelled = if (identical(setting, "FALSE")) c(at, if (any(same[at])) prev, early)
                 else c(at, prev, if (!length(prev)) open, early),
      stop("unknown dose type: ", type, call. = FALSE))))
  }
  cand <- c(at, open, early)
  # a dose at the dose's time (into another compartment): rxode2 reads rate() /
  # dur() from the next record after it, which is not in the window
  if (any(dose[at])) {
    after <- others[time[others] > t]
    if (length(after)) cand <- c(cand, after[time[after] == min(time[after])])
  }
  # nonmem = TRUE reads records in data order: one written before the dose is
  # not read
  if (identical(setting, "nonmem")) cand <- cand[!(cand %in% at & cand < i & !dose[cand])]
  unique(cand)
}

# Fill NA forward along one subject's records in time order (ties in data order),
# leaving a leading NA alone.
.lintFillForward <- function(x, time) {
  ord <- order(time, seq_along(time))
  v <- x[ord]
  ok <- !is.na(v)
  if (!any(ok)) return(x)
  idx <- cummax(ifelse(ok, seq_along(v), 0L))
  v[idx > 0L] <- v[idx[idx > 0L]]
  out <- x
  out[ord] <- v
  out
}

# Whether two covariate values differ. Numbers are compared to a relative 1e-8,
# anything else by its text; an NA never differs.
.lintDiffers <- function(a, b) {
  if (is.na(a) || is.na(b)) return(FALSE)
  if (is.numeric(a) && is.numeric(b)) return(abs(a - b) > 1e-8 * max(1, abs(a), abs(b)))
  !identical(as.character(a), as.character(b))
}

# --- the event table -------------------------------------------------------------

# The column of `ev` named `name`, ignoring case (rxode2 reads its standard
# columns that way), or NULL.
.lintEvCol <- function(ev, name) {
  i <- match(tolower(name), tolower(names(ev)))
  if (is.na(i)) NULL else ev[[i]]
}

# The lagged-compartment entry (an index into `cmts`) each cmt value of the table
# refers to, NA where it is another compartment. `cmt` may be names or numbers;
# a number is a position in `state`.
.lintCmtEntry <- function(cmt, cmts, state) {
  names_ <- vapply(cmts, function(ce) ce$cmt, "")
  if (is.factor(cmt)) cmt <- as.character(cmt)
  if (is.character(cmt)) return(match(cmt, names_))
  at <- match(names_, state)
  match(as.integer(cmt), at, incomparables = NA)
}

# One event table as per-record data for the checks: `.id`, `.time`, `.dose`,
# `.type`, `.lc` (the lagged-compartment entry the dose goes to, NA for a record
# that is not a lagged dose) and the covariates the spec reads, filled forward.
# Returns list(ok = TRUE, rec = <data frame>, cov = <covariate columns present>)
# or list(ok = FALSE, reason = ...) for a table it cannot read.
.lintEventRecords <- function(ev, spec) {
  fail <- function(why) list(ok = FALSE, reason = why)
  if (inherits(ev, "rxEt")) ev <- as.data.frame(ev)
  if (!is.data.frame(ev)) return(fail("the events argument is not a data frame or event table"))
  if (!nrow(ev)) return(fail("the event table is empty"))
  time <- .lintEvCol(ev, "time")
  if (is.null(time) || !is.numeric(time) || anyNA(time)) {
    return(fail("the event table has no numeric `time` column without missing values"))
  }
  amt <- .lintEvCol(ev, "amt")
  evid <- .lintEvCol(ev, "evid")
  if (is.null(evid)) evid <- if (is.null(amt)) rep(0L, nrow(ev)) else ifelse(!is.na(amt) & amt != 0, 1L, 0L)
  amt_ok <- if (is.null(amt)) rep(TRUE, nrow(ev)) else !is.na(amt) & amt != 0
  dose <- !is.na(evid) & evid %in% c(1L, 4L) & amt_ok
  addl <- .lintEvCol(ev, "addl")
  if (!is.null(addl) && any(dose & !is.na(addl) & addl > 0)) {
    return(fail("the event table uses addl, which the pre-lint does not expand"))
  }
  ss <- .lintEvCol(ev, "ss")
  if (!is.null(ss) && any(dose & !is.na(ss) & ss > 0)) {
    return(fail("the event table has steady-state doses (ss), which the pre-lint does not model"))
  }
  rate <- .lintEvCol(ev, "rate")
  dur <- .lintEvCol(ev, "dur")
  type <- rep("bolus", nrow(ev))
  if (!is.null(dur)) type[!is.na(dur) & dur > 0] <- "dur"
  if (!is.null(rate)) {
    type[!is.na(rate) & rate > 0] <- "fixed"
    type[!is.na(rate) & rate == -1] <- "rate1"
    type[!is.na(rate) & rate == -2] <- "rate2"
  }
  cmt <- .lintEvCol(ev, "cmt")
  if ((is.null(cmt) || is.numeric(cmt)) && !length(spec$state)) {
    return(fail("the model's compartments are not known, so the doses' compartments cannot be matched to alag()"))
  }
  lc <- if (is.null(cmt)) rep(.lintCmtEntry(1L, spec$cmts, spec$state), nrow(ev))
        else .lintCmtEntry(cmt, spec$cmts, spec$state)
  id <- .lintEvCol(ev, "id")
  if (is.null(id)) id <- rep(1L, nrow(ev))
  covs <- intersect(spec$covs_used, names(ev))
  cmt_chr <- if (is.null(cmt)) rep("1", nrow(ev)) else as.character(cmt)
  rec <- data.frame(.id = id, .time = time, .dose = dose, .type = type, .lc = lc,
                    .cmt = cmt_chr, stringsAsFactors = FALSE)
  rec$.dose[!dose] <- FALSE
  rec$.lc[!dose] <- NA_integer_
  for (cv in covs) {
    x <- ev[[cv]]
    if (is.factor(x)) x <- as.character(x)
    rec[[cv]] <- x
  }
  # fill each subject's covariates forward
  if (length(covs)) {
    groups <- split(seq_len(nrow(rec)), rec$.id)
    for (cv in covs) {
      x <- rec[[cv]]
      for (rows in groups) x[rows] <- .lintFillForward(x[rows], rec$.time[rows])
      rec[[cv]] <- x
    }
  }
  list(ok = TRUE, rec = rec, cov = covs)
}

# --- the check ----------------------------------------------------------------------

# Doses of an event table whose covariates the setting would read from another
# record. The lag time of a dose is computed from the model (see .lintLagTime();
# NA when it cannot be), cached in `cache`; `lag_known` FALSE says the call
# supplies parameters that may change it, so it is NA for every dose. Returns a
# data frame with one row per (dose, record, covariate) difference.
.lintPairCheck <- function(rec, spec, setting, cache, lag_known = TRUE) {
  out <- list()
  covs <- setdiff(names(rec), c(".id", ".time", ".dose", ".type", ".lc", ".cmt"))
  for (rows in split(seq_len(nrow(rec)), rec$.id)) {
    time <- rec$.time[rows]
    dose <- rec$.dose[rows]
    lc <- rec$.lc[rows]
    type <- rec$.type[rows]
    cmtv <- rec$.cmt[rows]
    cvv <- stats::setNames(lapply(covs, function(cv) rec[[cv]][rows]), covs)
    idv <- rec$.id[[rows[[1L]]]]
    for (i in which(dose & !is.na(lc))) {
      ce <- spec$cmts[[lc[[i]]]]
      own_type <- type[[i]]
      vals <- lapply(cvv, function(v) v[[i]])
      lag <- if (lag_known) .lintLagCached(spec, ce, vals, cache) else NA_real_
      later <- time[dose & time > time[[i]]]
      horizon <- if (length(later)) min(later) else Inf
      same <- dose & cmtv == cmtv[[i]]
      f_type <- switch(own_type, bolus = "bolus", dur = "dur", fixed = "fixed", "modelled")
      qtys <- list(list(q = "f", type = f_type, cov = ce$f))
      if (own_type == "rate1") qtys[[2L]] <- list(q = "rate", type = "modelled", cov = ce$rate)
      if (own_type == "rate2") qtys[[2L]] <- list(q = "rate", type = "modelled", cov = ce$dur)
      for (qq in qtys) {
        use <- intersect(qq$cov, covs)
        if (!length(use)) next
        for (j in .lintPairCandidates(time, i, lag, qq$q, qq$type, setting, dose, horizon, same)) for (cv in use) {
          if (.lintDiffers(cvv[[cv]][[i]], cvv[[cv]][[j]])) {
            out[[length(out) + 1L]] <- list(
              id = idv, time = time[[i]], cmt = ce$cmt, type = own_type, qty = qq$q,
              setting = setting, cand_time = time[[j]], cand_dose = dose[[j]],
              covariate = cv, own = as.character(cvv[[cv]][[i]]),
              other = as.character(cvv[[cv]][[j]]), lag = lag)
          }
        }
      }
    }
  }
  if (!length(out)) return(NULL)
  cols <- lapply(names(out[[1L]]), function(f) unlist(lapply(out, `[[`, f), use.names = FALSE))
  names(cols) <- names(out[[1L]])
  as.data.frame(cols, stringsAsFactors = FALSE)
}

# How many lagged doses the table has (the denominator of "n of m doses").
.lintLaggedDoseCount <- function(rec) sum(rec$.dose & !is.na(rec$.lc))

# --- the model ------------------------------------------------------------------------

# What the pairing rule needs from a model: for each compartment with an alag()
# statement, the covariates its f(), rate() and dur() read (traced through
# intermediate assignments and if() conditions, as .lintLaggedFCovariates()
# does), the covariates its lag reads, and its lag statement; plus the model's
# states (for numeric `cmt` values) and everything needed to evaluate the lag
# time (statements, initial estimates, eta names). Compartments whose f(), rate()
# and dur() read no covariate are left out. A failure to read the model
# propagates.
.lintLagSpec <- function(ui) {
  acc <- new.env(parent = emptyenv())
  acc$deps <- list()
  acc$special <- list()
  statements <- ui$lstExpr
  if (!is.list(statements)) stop("the model has no readable statements", call. = FALSE)
  for (e in statements) .lintCollectAssignments(e, character(), acc)
  covs <- .lintModelCovariates(ui)
  sp <- acc$special
  fn <- vapply(sp, function(s) s$fn, "")
  cm <- vapply(sp, function(s) s$cmt, "")
  reach <- function(s) intersect(.lintReachable(s$reads, acc$deps), covs)
  covs_of <- function(f, cmt) {
    hit <- sp[fn == f & (is.na(cm) | cm == cmt)]
    sort(unique(as.character(unlist(lapply(hit, reach)))))
  }
  ini <- ui$iniDf
  etas <- as.character(ini$name[is.na(ini$ntheta)])
  cmts <- lapply(unique(cm[fn == "alag" & !is.na(cm)]), function(x) {
    lag_stmts <- sp[fn == "alag" & cm == x]
    # every name the lag depends on, through intermediate assignments
    names_in <- unique(as.character(unlist(lapply(lag_stmts, function(st) .lintReachable(st$reads, acc$deps)))))
    list(cmt = x, f = covs_of("f", x), rate = covs_of("rate", x), dur = covs_of("dur", x),
         lagcov = sort(unique(as.character(unlist(lapply(lag_stmts, reach))))),
         lagnames = names_in, lag_eta = length(intersect(names_in, etas)) > 0L)
  })
  keep <- vapply(cmts, function(ce) length(ce$f) + length(ce$rate) + length(ce$dur) > 0L, NA)
  cmts <- cmts[keep]
  is_theta <- !is.na(ini$ntheta)
  est <- stats::setNames(as.list(ini$est[is_theta]), ini$name[is_theta])
  list(cmts = cmts,
       state = tryCatch(as.character(ui$state), error = function(e) character()),
       covs = covs, stmts = statements, est = est, etas = as.character(ini$name[!is_theta]),
       covs_used = unique(unlist(lapply(cmts, function(ce) c(ce$f, ce$rate, ce$dur, ce$lagcov)))),
       env = NULL)
}

# Run the model's statements as an R script in `env`: assignments are evaluated
# in order, `if` / `{` followed, alag() statements stored as `.lag.<cmt>`. ODE
# lines, residual-error lines and anything else are skipped, and so is a
# statement that fails to evaluate (a variable it defines is then absent, which
# a later statement that needs it will report by failing too).
.lintRunModel <- function(stmts, env) {
  for (s in stmts) {
    if (!is.call(s)) next
    op <- if (is.symbol(s[[1L]])) as.character(s[[1L]]) else ""
    if (identical(op, "{")) {
      .lintRunModel(as.list(s)[-1L], env)
    } else if (identical(op, "if")) {
      cond <- tryCatch(eval(s[[2L]], env), error = function(e) NA)
      if (length(cond) == 1L && !is.na(cond)) {
        branch <- if (isTRUE(as.logical(cond))) s[[3L]] else if (length(s) >= 4L) s[[4L]] else NULL
        if (!is.null(branch)) .lintRunModel(list(branch), env)
      }
    } else if (op %in% c("<-", "=") && length(s) == 3L) {
      lhs <- s[[2L]]
      val <- tryCatch(eval(s[[3L]], env), error = function(e) NULL)
      if (is.null(val)) next
      if (is.symbol(lhs)) {
        assign(as.character(lhs), val, envir = env)
      } else if (is.call(lhs) && is.symbol(lhs[[1L]]) && length(lhs) == 2L &&
                 as.character(lhs[[1L]]) %in% c("alag", "lag") && is.symbol(lhs[[2L]])) {
        assign(paste0(".lag.", as.character(lhs[[2L]])), val, envir = env)
      }
    }
  }
  invisible(NULL)
}

# The lag time of a dose into compartment `ce$cmt`: the model's statements run at
# the initial estimates and the dose record's covariates. NA when it cannot be
# computed (a statement the sandbox cannot run, a covariate the table does not
# carry), is not a finite number of at least 0, or depends on a random effect.
.lintLagTime <- function(spec, ce, covvals) {
  # a lag that reads a random effect differs between subjects; the typical value
  # is not the lag
  if (isTRUE(ce$lag_eta)) return(NA_real_)
  env <- new.env(parent = spec$env)
  for (nm in names(spec$est)) assign(nm, spec$est[[nm]], envir = env)
  for (nm in spec$etas) assign(nm, 0, envir = env)
  for (nm in names(covvals)) assign(nm, covvals[[nm]], envir = env)
  .lintRunModel(spec$stmts, env)
  v <- get0(paste0(".lag.", ce$cmt), envir = env, inherits = FALSE)
  if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v < 0) return(NA_real_)
  as.numeric(v)
}

# The lag time of a dose, cached by the values of the covariates the lag reads
# (`cache` is an environment kept by the caller for one event table).
.lintLagCached <- function(spec, ce, vals, cache) {
  use <- vals[intersect(ce$lagcov, names(vals))]
  key <- paste(c(ce$cmt, unlist(use)), collapse = "\r")
  hit <- cache[[key]]
  if (is.null(hit)) {
    hit <- list(v = .lintLagTime(spec, ce, vals))
    cache[[key]] <- hit
  }
  hit$v
}

# --- the simulation's setting ---------------------------------------------------------

# Read the setting-related arguments `args` of a call into `st` (an environment
# with `keeps`, `nonmem` and `unreadable`), following an inline rxControl() /
# list() `control` argument.
.lintTakeSetting <- function(args, st) {
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  for (i in seq_along(args)) {
    a <- args[[i]]
    nm <- nms[[i]]
    if (identical(nm, "addlKeepsCov")) {
      if (is.null(a)) next
      if (is.logical(a) && length(a) == 1L && !is.na(a)) st$keeps <- a
      else if (identical(a, quote(T))) st$keeps <- TRUE
      else if (identical(a, quote(F))) st$keeps <- FALSE
      else st$unreadable <- TRUE
    } else if (identical(nm, "nonmem")) {
      if (is.null(a)) next
      if (is.logical(a) && length(a) == 1L && !is.na(a)) st$nonmem <- isTRUE(a)
      else if (identical(a, quote(T))) st$nonmem <- TRUE
      else if (identical(a, quote(F))) st$nonmem <- FALSE
      else st$unreadable <- TRUE
    } else if (identical(nm, "tieOrder")) {
      st$unreadable <- TRUE
    } else if (identical(nm, "control") ||
               (identical(nm, "") && is.call(a) && identical(.lintCallName(a), "rxControl"))) {
      if (is.call(a) && .lintCallName(a) %in% c("rxControl", "list")) {
        .lintTakeSetting(as.list(a)[-1L], st)
      } else if (identical(nm, "control")) {
        st$unreadable <- TRUE
      }
    }
  }
  invisible(NULL)
}

# Which of the three settings a simulation call runs under, as the values of
# .lintPairCandidates(): "FALSE" (addlKeepsCov = FALSE, the default), "TRUE"
# (addlKeepsCov = TRUE) or "nonmem" (nonmem = TRUE). A literal setting gives one;
# a setting the call does not state is FALSE; anything that cannot be read (a
# variable, tieOrder, a control argument that is not an inline rxControl() /
# list(), nonmem = TRUE together with addlKeepsCov = FALSE, or `unread`, which
# says the setting may come from elsewhere) gives all three.
.lintSiteSettings <- function(call, unread = FALSE) {
  st <- new.env(parent = emptyenv())
  st$keeps <- NULL
  st$nonmem <- FALSE
  st$unreadable <- unread
  .lintTakeSetting(as.list(call)[-1L], st)
  if (st$unreadable || (st$nonmem && identical(st$keeps, FALSE))) return(c("FALSE", "TRUE", "nonmem"))
  if (st$nonmem) return("nonmem")
  if (isTRUE(st$keeps)) "TRUE" else "FALSE"
}

# --- what else a call or an article can change -----------------------------------------------

# Whether a `params` / `theta` argument may touch the names a lag depends on: a
# literal c() / list() whose names are all given and none of them one of
# `lag_names` does not; anything else (a variable, a data frame, an unnamed
# element) may.
.lintParamsTouch <- function(a, lag_names) {
  if (!is.call(a) || !.lintCallName(a) %in% c("c", "list")) return(TRUE)
  nms <- names(as.list(a)[-1L])
  if (is.null(nms) || any(!nzchar(nms))) return(TRUE)
  length(intersect(nms, lag_names)) > 0L
}

# How many of a simulation call's unnamed arguments are not the model, a model
# expression or the event table: a params or control object given by position.
.lintExtraPositional <- function(call, model_arg) {
  args <- as.list(call)[-1L]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  free <- nms == "" & !vapply(args, identical, NA, model_arg) & !vapply(args, .lintIsModelish, NA)
  max(0L, sum(free) - if ("events" %in% nms) 0L else 1L)
}

# Whether the lag time computed from the model is the lag the call uses: not when
# it passes parameters that may change what the lag reads (params, theta,
# thetaMat, iCov, or an unnamed extra argument).
.lintSiteLagKnown <- function(call, model_arg, spec) {
  if (.lintExtraPositional(call, model_arg) > 0L) return(FALSE)
  lag_names <- unique(unlist(lapply(spec$cmts, function(ce) ce$lagnames)))
  lag_covs <- unique(unlist(lapply(spec$cmts, function(ce) ce$lagcov)))
  args <- as.list(call)[-1L]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  for (i in seq_along(args)) {
    if (nms[[i]] %in% c("params", "theta") && .lintParamsTouch(args[[i]], lag_names)) return(FALSE)
    if (identical(nms[[i]], "thetaMat") && length(intersect(lag_names, names(spec$est)))) return(FALSE)
    if (identical(nms[[i]], "iCov") && length(lag_covs)) return(FALSE)
  }
  TRUE
}

# Whether the article sets an rxode2 option (`options(rxode2.addlKeepsCov = ...)`
# and the like) anywhere, which would change the default of a simulation that
# states nothing.
.lintSetsRxodeOptions <- function(exprs) {
  for (e in exprs) {
    acc <- new.env(parent = emptyenv())
    acc$found <- FALSE
    .lintFindOptions(e, acc)
    if (acc$found) return(TRUE)
  }
  FALSE
}
.lintFindOptions <- function(e, acc) {
  if (!is.call(e)) return(invisible(NULL))
  if (identical(.lintCallName(e), "options")) {
    nms <- names(as.list(e)[-1L])
    if (any(grepl("^rxode2", nms))) acc$found <- TRUE
  }
  for (el in .lintKids(e)) .lintFindOptions(el, acc)
  invisible(NULL)
}

# --- the rule ---------------------------------------------------------------------------

# The simulations of an article the rule looks at: at most this many are run.
.lintPairingMaxSites <- 20L

.lintSettingLabel <- c(`FALSE` = "addlKeepsCov = FALSE", `TRUE` = "addlKeepsCov = TRUE",
                       nonmem = "nonmem = TRUE")

# Whether an argument is a model expression (readModelDb(), rxode2(), zeroRe(),
# model(), ...) rather than an event table.
.lintIsModelish <- function(a) {
  is.call(a) && .lintCallName(a) %in% c(.lintModelMakers, .lintModelWrappers)
}

# The arguments of a simulation call that may be its event table: `events =`, or
# else the unnamed arguments other than the model.
.lintEventCandidates <- function(call, model_arg) {
  args <- as.list(call)[-1L]
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  if ("events" %in% nms) return(args["events" == nms])
  keep <- nms == "" & !vapply(args, identical, NA, model_arg) &
    !vapply(args, .lintIsModelish, NA)
  args[keep]
}

# Collect into `acc$found` the calls to function `fun` in an expression, with
# the index `top` of the top-level statement holding them. The bodies of
# functions are not searched: a call there has the enclosing function's
# arguments, not the article's.
.lintCallsTo <- function(e, fun, top, acc) {
  if (!is.call(e)) return(invisible(NULL))
  if (identical(.lintCallName(e), "function")) return(invisible(NULL))
  if (is.symbol(e[[1L]]) && identical(as.character(e[[1L]]), fun)) {
    acc$found[[length(acc$found) + 1L]] <- list(call = e, top = top)
  } else if (.lintIsApplyLike(.lintCallName(e))) {
    # lapply(X, fun, ...): a call of fun on the first element of X
    args <- as.list(e)[-1L]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    hit <- vapply(args, function(a) is.symbol(a) && identical(as.character(a), fun), NA)
    if (any(hit)) {
      pos <- args[nms == "" & !hit]
      extra <- args[nms != "" & !nms %in% c("X", ".x", ".l", "FUN", ".f")]
      if (!length(pos)) pos <- args[nms %in% c("X", ".x")]
      if (length(pos)) {
        acc$found[[length(acc$found) + 1L]] <- list(
          call = as.call(c(list(as.name(fun), .lintFirstOf(pos[[1L]])), extra)), top = top)
      }
    }
  }
  for (el in .lintKids(e)) .lintCallsTo(el, fun, top, acc)
  invisible(NULL)
}

# `e` with every `...` replaced by the arguments `dots` (a list, possibly named).
.lintSubstituteDots <- function(e, dots) {
  if (!is.call(e)) return(e)
  kids <- as.list(e)
  nms <- names(kids)
  if (is.null(nms)) nms <- rep("", length(kids))
  new <- list()
  new_nms <- character()
  dnms <- names(dots)
  if (is.null(dnms)) dnms <- rep("", length(dots))
  for (i in seq_along(kids)) {
    if (identical(kids[[i]], quote(expr = ))) {
      new[length(new) + 1L] <- list(quote(expr = ))
      new_nms <- c(new_nms, nms[[i]])
    } else if (identical(kids[[i]], quote(...))) {
      new <- c(new, dots)
      new_nms <- c(new_nms, dnms)
    } else {
      new[length(new) + 1L] <- list(.lintSubstituteDots(kids[[i]], dots))
      new_nms <- c(new_nms, nms[[i]])
    }
  }
  names(new) <- new_nms
  as.call(new)
}

# What a call to a function gives its parameters: `p <- <argument>` statements
# (the argument matched by name, then by position; the default for a parameter
# the call does not give), and the arguments left over for `...`.
.lintBindCall <- function(formals, args) {
  params <- names(formals)
  fixed <- setdiff(params, "...")
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  used <- rep(FALSE, length(args))
  bound <- list()
  for (i in seq_along(args)) {
    if (nms[[i]] %in% fixed && is.null(bound[[nms[[i]]]])) {
      bound[[nms[[i]]]] <- args[[i]]
      used[[i]] <- TRUE
    }
  }
  before_dots <- if ("..." %in% params) params[seq_len(match("...", params) - 1L)] else fixed
  free <- setdiff(before_dots, names(bound))
  pos <- which(!used & nms == "")
  for (j in seq_len(min(length(pos), length(free)))) {
    bound[[free[[j]]]] <- args[[pos[[j]]]]
    used[[pos[[j]]]] <- TRUE
  }
  stmts <- list()
  for (p in fixed) {
    if (!is.null(bound[[p]])) {
      stmts[[length(stmts) + 1L]] <- call("<-", as.name(p), bound[[p]])
    } else if (!identical(formals[[p]], quote(expr = ))) {
      stmts[[length(stmts) + 1L]] <- call("<-", as.name(p), formals[[p]])
    }
  }
  list(stmts = stmts, dots = if ("..." %in% params) args[!used] else NULL)
}

# The statements that run before a site inside the constructs `frames` (see
# .lintPushFrame()), as `list(stmts, lambda)`: the statements before it in each
# block, the first value of each loop variable, and the parameters of the
# anonymous functions it is inside (which are not bound).
.lintFrameStatements <- function(frames) {
  stmts <- list()
  lambda <- character()
  for (fr in frames) {
    if (identical(fr$kind, "block")) {
      stmts <- c(stmts, fr$before)
    } else if (identical(fr$kind, "for")) {
      stmts[[length(stmts) + 1L]] <- call("<-", as.name(fr$var), .lintFirstOf(fr$over))
    } else if (identical(fr$kind, "lambda")) {
      bound <- .lintLambdaBindings(fr$params, fr$apply)
      stmts <- c(stmts, bound)
      lambda <- c(lambda, setdiff(fr$params, vapply(bound, function(b) as.character(b[[2L]]), "")))
    }
  }
  list(stmts = stmts, lambda = lambda)
}

# `(x)[[1L]]`, the first element of what an expression evaluates to.
.lintFirstOf <- function(x) call("[[", call("(", x), 1L)

# The parameters of an anonymous function handed to an apply-family call, bound
# to the first element of what it is applied to: `p <- (X)[[1]]` for lapply() and
# map(); the first element of each list for Map(), mapply() and map2(); the first
# row of the list or data frame, by name or by position, for pmap(). `ap` is
# list(fn = , args = ) of the call, its other arguments.
.lintLambdaBindings <- function(params, ap) {
  params <- setdiff(params, "...")
  if (is.null(ap) || !length(params)) return(list())
  nms <- names(ap$args)
  if (is.null(nms)) nms <- rep("", length(ap$args))
  pos <- ap$args[nms == ""]
  if (!length(pos)) pos <- ap$args[nms %in% c("X", ".x", ".l")]
  if (!length(pos)) return(list())
  if (grepl("^(pmap|pwalk)", ap$fn)) {
    lst <- pos[[1L]]
    return(lapply(seq_along(params), function(k) {
      p <- params[[k]]
      call("<-", as.name(p), bquote(
        if (is.null((.(lst))[[.(p)]])) (.(lst))[[.(k)]][[1L]] else (.(lst))[[.(p)]][[1L]]))
    }))
  }
  if (ap$fn %in% c("Map", "mapply") || grepl("^(map2|walk2|imap)", ap$fn)) {
    k <- min(length(params), length(pos))
    return(lapply(seq_len(k), function(i) call("<-", as.name(params[[i]]), .lintFirstOf(pos[[i]]))))
  }
  list(call("<-", as.name(params[[1L]]), .lintFirstOf(pos[[1L]])))
}

# What a simulation call is given as events, as a list of targets. Each has the
# candidate expressions (`events =`, or the unnamed arguments other than the
# model), `base` (the code of the first `base - 1` top-level statements runs
# first), `extra` (statements that run after those: the code before the site in
# the blocks, loops and function it is inside, and the parameters of that
# function bound from a call to it), `lambda` (parameters of anonymous
# functions it is inside, which are unbound) and `uncalled` (TRUE when its
# function is never called at the top level). A site in a named function gives
# one target per top-level call to it; any other site gives one.
# Returns list(ok = TRUE, targets = ) or list(ok = FALSE, reason = ).
.lintSiteTargets <- function(site, scan) {
  cands <- .lintEventCandidates(site$call, site$model_arg)
  if (!length(cands)) {
    return(list(ok = FALSE, reason = "the call has no events argument that can be identified"))
  }
  frames <- site$path
  fun_at <- which(vapply(frames, function(fr) identical(fr$kind, "function"), NA))
  if (!length(fun_at)) {
    fs <- .lintFrameStatements(frames)
    return(list(ok = TRUE, targets = list(list(
      cands = cands, base = site$top, extra = fs$stmts, lambda = fs$lambda, uncalled = FALSE))))
  }
  fun <- frames[[max(fun_at)]]
  fs <- .lintFrameStatements(frames[-seq_len(max(fun_at))])
  acc <- new.env(parent = emptyenv())
  acc$found <- list()
  for (k in seq_along(scan$exprs)) {
    if (k > site$top) .lintCallsTo(scan$exprs[[k]], fun$name, k, acc)
  }
  if (!length(acc$found)) {
    bound <- .lintBindCall(fun$formals, list())
    return(list(ok = TRUE, targets = list(list(
      cands = cands, base = site$top, extra = c(bound$stmts, fs$stmts), lambda = fs$lambda,
      uncalled = TRUE, fun = fun$name))))
  }
  targets <- lapply(acc$found, function(cl) {
    bound <- .lintBindCall(fun$formals, as.list(cl$call)[-1L])
    extra <- c(bound$stmts, fs$stmts)
    use <- cands
    if (length(bound$dots) || "..." %in% names(fun$formals)) {
      extra <- lapply(extra, .lintSubstituteDots, dots = bound$dots)
      # an argument that is `...` itself stands for the call's extra arguments
      # (`events =` or unnamed ones)
      dnm <- names(bound$dots)
      if (is.null(dnm)) dnm <- rep("", length(bound$dots))
      spliced <- bound$dots[dnm %in% c("", "events")]
      use <- unlist(lapply(cands, function(cd) {
        if (identical(cd, quote(...))) spliced else list(.lintSubstituteDots(cd, bound$dots))
      }), recursive = FALSE)
      if (is.null(use)) use <- list()
    }
    list(cands = use, base = cl$top, extra = extra, lambda = fs$lambda, uncalled = FALSE)
  })
  list(ok = TRUE, targets = targets)
}

# Say why a target's event table could not be built in terms of the article: a
# parameter of an anonymous function is not bound, a function is never called.
.lintTargetReason <- function(reason, target) {
  if (isTRUE(target$uncalled)) {
    return(paste0("it is made inside `", target$fun, "()`, which the article does not call at the top level"))
  }
  m <- regmatches(reason, regexec("object '([^']+)' not found", reason))[[1L]]
  if (length(m) == 2L && m[[2L]] %in% target$lambda) {
    return(paste0("it is made inside an anonymous function whose parameter `", m[[2L]],
                  "` the pre-lint does not bind"))
  }
  reason
}

# The event table for one target: the code that runs before the site (see
# .lintSiteTargets()) is sliced for what the candidate argument depends on and
# run in the sandbox, and the first value that is an event table is used.
# Returns list(ok = TRUE, ev = <table>) or list(ok = FALSE, reason = ...).
.lintTargetEvents <- function(target, scan, wl) {
  flat <- c(scan$exprs[seq_len(target$base - 1L)], target$extra)
  first_reason <- NULL
  for (cand in target$cands) {
    if (identical(cand, quote(...))) {
      if (is.null(first_reason)) first_reason <- "its events are passed through `...` and the function is not called with them"
      next
    }
    stmts <- .lintSlice(flat, length(flat) + 1L, cand)
    run <- .lintSandboxRun(stmts, cand, wl = wl)
    if (!run$ok) {
      if (is.null(first_reason)) first_reason <- run$reason
      next
    }
    if (is.data.frame(run$value) || inherits(run$value, "rxEt")) return(list(ok = TRUE, ev = run$value))
    if (is.null(first_reason)) first_reason <- "the events argument is not a data frame or event table"
  }
  list(ok = FALSE, reason = .lintTargetReason(first_reason, target))
}

# The library name a model label stands for, or NA when the model was supplied
# as a function or an rxUi (the label is then "the supplied model").
.lintLabelName <- function(label) {
  if (grepl("^`.*`$", label)) substr(label, 2L, nchar(label) - 1L) else NA_character_
}

# Which model a simulation uses, among the article's models that have a lagged
# dose whose covariates matter: the only one when there is one; otherwise the
# one the call's model argument was read from. NULL with a reason when unknown.
.lintSiteSpec <- function(site, specs) {
  if (length(specs) == 1L) return(list(spec = specs[[1L]]))
  labels <- vapply(specs, function(sp) sp$label, "")
  hit <- if (is.na(site$origin)) integer() else which(labels == paste0("`", site$origin, "`"))
  if (length(hit) == 1L) return(list(spec = specs[[hit]]))
  list(reason = "the article has more than one model with a lagged dose and the one this simulation uses cannot be read")
}

# One sentence about the first difference of a setting.
.lintDiffExample <- function(d, ndose_total) {
  d1 <- d[1L, ]
  what <- if (identical(d1$qty, "f")) "f()" else "rate()/dur()"
  kind <- if (isTRUE(d1$cand_dose)) "dose" else "observation or other"
  paste0("the dose at time ", format(d1$time), " (ID ", d1$id, ") has ", d1$covariate,
         " = ", d1$own, ", but its ", what, " may read ", d1$covariate, " = ", d1$other,
         " from the ", kind, " record at time ", format(d1$cand_time))
}

# The finding for one simulation: `diffs` is the rows of .lintPairCheck() over
# the settings it ran under (NULL when there were none), `n_doses` the number of
# lagged doses, `settings` the settings checked, `lag_known` whether the lag
# time could be computed for every dose.
.lintPairingIssue <- function(text, diffs, n_doses, settings, lag_known) {
  parts <- character()
  for (st in settings) {
    d <- diffs[diffs$setting == st, , drop = FALSE]
    if (!nrow(d)) next
    n_aff <- nrow(unique(d[c("id", "time", "cmt")]))
    parts <- c(parts, paste0("under ", .lintSettingLabel[[st]], ", ", n_aff, " of ", n_doses,
                             " lagged dose", if (n_doses != 1L) "s", " may read covariates from a record ",
                             "other than their own: ", .lintDiffExample(d, n_doses)))
  }
  unread <- length(settings) > 1L
  lagnote <- if (!lag_known) {
    paste0(" The lag time is not known from the model's initial estimates (it reads a random ",
           "effect, a parameter the call supplies or a covariate the table does not carry, or ",
           "could not be computed), so every record up to the dose's next dose was checked, ",
           "not only those inside its lag window.")
  } else ""
  list(check = "lagged-dose-pairing",
       message = paste0("`", text, "` is given an event table in which ",
                        paste(parts, collapse = "; "), ".",
                        if (unread) " The call's setting could not be read, so all three were checked.",
                        lagnote),
       fix = paste0("give every record the covariates of the dose that opened its interval (carry ",
                    "each dose's covariates forward to the records after it, up to the next dose) ",
                    "and state addlKeepsCov = FALSE; addlKeepsCov = TRUE or nonmem = TRUE can still ",
                    "read the record before an infusion, which carries the previous dose's covariates"))
}

# Remember that a simulation could not be checked, and why.
.lintNoteUnverifiable <- function(unver, reason, text) {
  unver[[reason]] <- c(unver[[reason]], text)
  invisible(NULL)
}

# The pairing rule over an article. `scan` is .lintScanSolves() of it and
# `models` the models read for the lagged-dose rule. Returns a list of
# .lintIssue() rows: one "lagged-dose-pairing" per simulation with a difference,
# and one "lagged-dose-pairing-unverifiable" per reason a simulation could not be
# checked (naming the first and counting the rest).
.lintPairing <- function(scan, models) {
  if (!length(scan$sites)) return(list())
  specs <- list()
  problems <- character()
  for (m in models) {
    if (is.null(m$ui)) {
      problems <- c(problems, paste0("the model ", m$label, " could not be read (",
                                     .lintTrunc(if (is.null(m$error)) "no reason given" else m$error, 80L), ")"))
      next
    }
    sp <- tryCatch(.lintLagSpec(m$ui), error = function(e) e)
    if (inherits(sp, "error")) {
      problems <- c(problems, paste0("the model ", m$label, " could not be analysed (",
                                     .lintTrunc(conditionMessage(sp), 80L), ")"))
    } else if (length(sp$cmts)) {
      sp$label <- m$label
      specs[[length(specs) + 1L]] <- sp
    }
  }
  if (!length(specs) && !length(problems)) return(list())
  wl <- .lintSandboxEnv()
  for (i in seq_along(specs)) specs[[i]]$env <- wl
  opts_unread <- .lintSetsRxodeOptions(scan$exprs)

  iss <- list()
  unver <- new.env(parent = emptyenv())
  run <- 0L
  spec_names <- vapply(specs, function(sp) .lintLabelName(sp$label), "")
  for (problem in problems) {
    for (site in scan$sites) .lintNoteUnverifiable(unver, problem, site$text)
  }
  if (!length(specs)) return(.lintPairingRows(iss, unver))
  for (site in scan$sites) {
    text <- site$text
    # a simulation of another library model is not about a lagged dose here: the
    # model it solves was read from a literal name that none of the lagged models has
    if (!is.na(site$origin) && !anyNA(spec_names) && !site$origin %in% spec_names) next
    if (!identical(site$kind, "rxSolve") || is.null(site$call)) {
      .lintNoteUnverifiable(unver, if (identical(site$kind, "rxSolve")) {
        "it is made inside a function, so its event table depends on the function's arguments"
      } else {
        "it is not an rxSolve() or solve(model, ...) call whose arguments can be read"
      }, text)
      next
    }
    if (isTRUE(site$modified)) {
      .lintNoteUnverifiable(unver, "the model it solves is changed by model() in the article, so its lag and f() may differ from the library's", text)
      next
    }
    pick <- .lintSiteSpec(site, specs)
    if (is.null(pick$spec)) {
      .lintNoteUnverifiable(unver, pick$reason, text)
      next
    }
    tg <- .lintSiteTargets(site, scan)
    if (!tg$ok) {
      .lintNoteUnverifiable(unver, tg$reason, text)
      next
    }
    spec <- pick$spec
    settings <- .lintSiteSettings(
      site$call, unread = opts_unread || .lintExtraPositional(site$call, site$model_arg) > 0L)
    lag_known <- .lintSiteLagKnown(site$call, site$model_arg, spec)
    diffs <- list()
    n_doses <- 0L
    for (target in tg$targets) {
      if (run >= .lintPairingMaxSites) {
        .lintNoteUnverifiable(unver, sprintf("the pre-lint runs at most %d simulations per article", .lintPairingMaxSites), text)
        break
      }
      run <- run + 1L
      got <- .lintTargetEvents(target, scan, wl)
      if (!got$ok) {
        .lintNoteUnverifiable(unver, got$reason, text)
        next
      }
      recs <- .lintEventRecords(got$ev, spec)
      if (!recs$ok) {
        .lintNoteUnverifiable(unver, recs$reason, text)
        next
      }
      cache <- new.env(parent = emptyenv())
      d <- do.call(rbind, lapply(settings, function(st) .lintPairCheck(recs$rec, spec, st, cache, lag_known)))
      n_doses <- n_doses + .lintLaggedDoseCount(recs$rec)
      if (!is.null(d)) diffs[[length(diffs) + 1L]] <- d
    }
    if (!length(diffs)) next
    diffs <- do.call(rbind, diffs)
    iss[[length(iss) + 1L]] <- .lintPairingIssue(
      text, diffs, n_doses, settings, !anyNA(diffs$lag))
  }
  .lintPairingRows(iss, unver)
}

# The findings as rows: the differences, then one "not verifiable" per reason.
.lintPairingRows <- function(iss, unver) {
  out <- lapply(iss, function(i) .lintIssue(i$check, "warning", i$message, i$fix))
  for (reason in sort(names(unver))) {
    texts <- unver[[reason]]
    out[[length(out) + 1L]] <- .lintIssue(
      "lagged-dose-pairing-unverifiable", "warning",
      paste0("cannot check how the event table of `", texts[[1L]], "`",
             if (length(texts) > 1L) paste0(" (and ", length(texts) - 1L, " more simulation",
                                            if (length(texts) > 2L) "s", ")"),
             " pairs covariates with the model's lagged doses: ", reason, "."),
      paste0("build the event table in top-level code from base, stats, dplyr, tidyr, tibble, purrr ",
             "and rxode2::et calls so the pre-lint can run it, or check by hand that every record ",
             "carries the covariates of the dose that opened its interval"))
  }
  out
}
