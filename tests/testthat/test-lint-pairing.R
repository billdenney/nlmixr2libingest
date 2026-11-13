# The lagged-dose pairing rule (R/lint-pairing.R): which records of an event table
# a lagged dose may read the covariates of its f() / rate() / dur() from, per
# dose type and setting, as measured on rxode2 5.1.8.9000; the table checks built
# on that; and the rule end to end through lint_vignette().

# --- the measured candidate table ---------------------------------------------------------

# One row of expectations per dose type, each as c(FALSE, TRUE, nonmem): the
# records (by index in `time`, data order) the dose at index `i` may read.
# Every layout has records at 0 and 20 before the dose (index 1, 2) unless noted.
pair_layouts <- list(
  # no other record near the dose
  none = list(time = c(0, 20, 24), i = 3L, lag = 0.5,
    bolus = list(integer(), integer(), integer()),
    fixed = list(integer(), 1:2, 1:2),
    modelled = list(integer(), 2L, 2L),
    rate = list(integer(), integer(), integer())),
  # a record at the dose's time written before the dose
  co_before = list(time = c(0, 20, 24, 24), i = 4L, lag = 0.5,
    bolus = list(3L, integer(), integer()),
    fixed = list(3L, 1:3, 1:3),
    modelled = list(3L, 2:3, 2:3),
    rate = list(3L, 3L, integer())),
  # a record at the dose's time written after the dose
  co_after = list(time = c(0, 20, 24, 24), i = 3L, lag = 0.5,
    bolus = list(4L, integer(), integer()),
    fixed = list(4L, c(1L, 2L, 4L), c(1L, 2L, 4L)),
    modelled = list(4L, c(2L, 4L), c(2L, 4L)),
    rate = list(4L, 4L, 4L)),
  # a record inside the lag window
  window = list(time = c(0, 20, 24, 24.25), i = 3L, lag = 0.5,
    bolus = list(4L, integer(), integer()),
    fixed = list(integer(), c(1L, 2L, 4L), c(1L, 2L, 4L)),
    modelled = list(integer(), 2L, 2L),
    rate = list(4L, 4L, 4L)),
  # a record exactly at the arrival time: it counts for a bolus's f() only
  arrival = list(time = c(0, 20, 24, 24.5), i = 3L, lag = 0.5,
    bolus = list(4L, integer(), integer()),
    fixed = list(integer(), c(1L, 2L, 4L), c(1L, 2L, 4L)),
    modelled = list(integer(), 2L, 2L),
    rate = list(integer(), integer(), integer())),
  # a record after the arrival time
  after = list(time = c(0, 20, 24, 24.7), i = 3L, lag = 0.5,
    bolus = list(integer(), integer(), integer()),
    fixed = list(integer(), c(1L, 2L, 4L), c(1L, 2L, 4L)),
    modelled = list(integer(), 2L, 2L),
    rate = list(integer(), integer(), integer())),
  # several records at the dose's time, in the window and after the arrival
  several = list(time = c(20, 24, 24, 24.2, 24.3, 24.8), i = 2L, lag = 0.5,
    bolus = list(3:5, integer(), integer()),
    fixed = list(3L, c(1L, 3L, 4L, 5L, 6L), c(1L, 3L, 4L, 5L, 6L)),
    modelled = list(3L, c(1L, 3L), c(1L, 3L)),
    rate = list(3:5, 3:5, 3:5)),
  # the next dose inside the lag window (lag longer than the interval), no record
  # before the dose, so a modelled rate reads the next record in the window
  overlap = list(time = c(0, 10, 40), i = 1L, lag = 20,
    bolus = list(2L, integer(), integer()),
    fixed = list(integer(), 2:3, 2:3),
    modelled = list(integer(), 2L, 2L),
    rate = list(2L, 2L, 2L)),
  # no lag: every setting reads the dose's own record
  no_lag = list(time = c(0, 20, 24, 24), i = 4L, lag = 0,
    bolus = list(integer(), integer(), integer()),
    fixed = list(integer(), integer(), integer()),
    modelled = list(integer(), integer(), integer()),
    rate = list(integer(), integer(), integer())),
  # lag not known: the window is every record up to the next dose (here none, so
  # to the end of the table)
  lag_unknown = list(time = c(0, 20, 24, 24, 24.25), i = 4L, lag = NA_real_,
    bolus = list(c(3L, 5L), integer(), integer()),
    fixed = list(3L, c(1L, 2L, 3L, 5L), c(1L, 2L, 3L, 5L)),
    modelled = list(3L, 2:3, 2:3),
    rate = list(c(3L, 5L), c(3L, 5L), 5L))
)

test_that("the candidate records of every dose type and setting match the measured table", {
  settings <- c("FALSE", "TRUE", "nonmem")
  for (nm in names(pair_layouts)) {
    lay <- pair_layouts[[nm]]
    expect_setequal(setdiff(names(lay), c("time", "i", "lag")), c("bolus", "fixed", "modelled", "rate"))
    for (type in c("bolus", "fixed", "modelled")) {
      for (k in seq_along(settings)) {
        got <- .lintPairCandidates(lay$time, lay$i, lay$lag, "f", type, settings[[k]])
        expect_identical(sort(got), lay[[type]][[k]], info = paste(nm, "f", type, settings[[k]]))
      }
    }
    for (k in seq_along(settings)) {
      got <- .lintPairCandidates(lay$time, lay$i, lay$lag, "rate", "modelled", settings[[k]])
      expect_identical(sort(got), lay$rate[[k]], info = paste(nm, "rate", settings[[k]]))
    }
    # a fixed duration reads like a bolus
    for (k in seq_along(settings)) {
      expect_identical(.lintPairCandidates(lay$time, lay$i, lay$lag, "f", "dur", settings[[k]]),
                       .lintPairCandidates(lay$time, lay$i, lay$lag, "f", "bolus", settings[[k]]),
                       info = paste(nm, "dur", settings[[k]]))
    }
  }
})

test_that("a dose next to the dose widens the candidates", {
  # records: 1 at 20, the dose at 24 (index 2), another dose at 24 (index 3), an observation at 26 (4)
  time <- c(20, 24, 24, 26)
  dose <- c(FALSE, TRUE, TRUE, FALSE)
  cand <- function(qty, type, setting) {
    sort(.lintPairCandidates(time, 2L, 0.5, qty, type, setting, dose))
  }
  expect_identical(cand("f", "bolus", "FALSE"), 3L)
  # a dose at the dose's time leaves the record before the dose a candidate for an infusion's f()
  expect_identical(cand("f", "modelled", "FALSE"), c(1L, 3L))
  expect_identical(cand("f", "fixed", "FALSE"), c(1L, 3L))
  expect_identical(cand("f", "modelled", "TRUE"), c(1L, 3L))
  # a co-timed dose into ANOTHER compartment supplies like an observation: not the record before
  same_cmt <- c(FALSE, TRUE, FALSE, FALSE)
  expect_identical(sort(.lintPairCandidates(time, 2L, 0.5, "f", "modelled", "FALSE", dose, same = same_cmt)), 3L)
  expect_identical(sort(.lintPairCandidates(time, 2L, 0.5, "f", "fixed", "FALSE", dose, same = same_cmt)), 3L)
  # with an observation at the dose's time the record before is not one under FALSE
  expect_identical(sort(.lintPairCandidates(c(20, 24, 24, 26), 2L, 0.5, "f", "modelled", "FALSE",
                                            c(FALSE, TRUE, FALSE, FALSE))), 3L)
  # rate() / dur(): the dose at the dose's time, and the next record after it
  expect_identical(cand("rate", "modelled", "FALSE"), 3:4)
  expect_identical(cand("rate", "modelled", "nonmem"), 3:4)
  # of several later records only the next one (or the records at its time)
  later <- c(24, 24, 25, 25, 30, 50)
  expect_identical(sort(.lintPairCandidates(later, 1L, 0.5, "rate", "modelled", "FALSE",
                                            c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))), 2:4)
  # without a dose at the dose's time only the observations near it are candidates
  expect_identical(sort(.lintPairCandidates(c(20, 24, 24.2, 26), 2L, 0.5, "rate", "modelled", "FALSE",
                                            c(FALSE, TRUE, FALSE, FALSE))), 3L)
  # a dose inside the lag window is a candidate for rate() / dur()
  expect_identical(sort(.lintPairCandidates(c(24, 24.2), 1L, 0.5, "rate", "modelled", "FALSE",
                                            c(TRUE, TRUE))), 2L)
  # a record written before the dose at its time is not read by nonmem = TRUE unless it is a dose
  expect_identical(.lintPairCandidates(c(24, 24), 2L, 0.5, "rate", "modelled", "nonmem", c(FALSE, TRUE)), integer())
  expect_identical(.lintPairCandidates(c(24, 24), 2L, 0.5, "rate", "modelled", "nonmem", c(TRUE, TRUE)), 1L)
})

test_that("an earlier dose still in its lag when the dose is recorded is a candidate", {
  # a dose at 23.8 with lag 0.5 arrives at 24.3, after the dose recorded at 24
  time <- c(23.8, 24)
  dose <- c(TRUE, TRUE)
  expect_identical(.lintPairCandidates(time, 2L, 0.5, "f", "bolus", "FALSE", dose), 1L)
  expect_identical(.lintPairCandidates(time, 2L, 0.5, "f", "modelled", "TRUE", dose), 1L)
  expect_identical(.lintPairCandidates(time, 2L, 0.5, "rate", "modelled", "FALSE", dose), 1L)
  # one that has arrived by then is not
  expect_identical(.lintPairCandidates(c(23, 24), 2L, 0.5, "rate", "modelled", "FALSE", c(TRUE, TRUE)), integer())
  # an observation recorded earlier is not a candidate for being "in its lag"
  expect_identical(.lintPairCandidates(c(23.8, 24), 2L, 0.5, "rate", "modelled", "FALSE", c(FALSE, TRUE)), integer())
  # with an unknown lag the horizon is the next dose
  expect_identical(.lintPairCandidates(c(24, 25, 30, 36), 1L, NA_real_, "f", "bolus", "FALSE",
                                       c(TRUE, FALSE, FALSE, TRUE), horizon = 36), 2:3)
  expect_identical(.lintPairCandidates(c(24, 25, 30, 36), 1L, NA_real_, "f", "bolus", "FALSE",
                                       c(TRUE, FALSE, FALSE, TRUE), horizon = 28), 2L)
})

test_that("an unknown dose type is an error, not an empty answer", {
  expect_error(.lintPairCandidates(c(0, 24), 2L, 0.5, "f", "infusion", "FALSE"), "unknown dose type")
})

test_that("a single record has no candidates", {
  for (type in c("bolus", "fixed", "modelled")) {
    for (st in c("FALSE", "TRUE", "nonmem")) {
      expect_identical(.lintPairCandidates(5, 1L, 0.5, "f", type, st), integer(), info = paste(type, st))
    }
  }
})

# --- small pieces ---------------------------------------------------------------------------

test_that("NA covariates are filled forward in time order, a leading NA stays", {
  expect_identical(.lintFillForward(c(1, NA, 3, NA), c(0, 1, 2, 3)), c(1, 1, 3, 3))
  expect_identical(.lintFillForward(c(NA, 2, NA), c(0, 1, 2)), c(NA, 2, 2))
  # filled in time order, not data order
  expect_identical(.lintFillForward(c(NA, 5, 1), c(2, 3, 1)), c(1, 5, 1))
  expect_identical(.lintFillForward(c(NA_real_, NA_real_), c(0, 1)), c(NA_real_, NA_real_))
  expect_identical(.lintFillForward(numeric(), numeric()), numeric())
  expect_identical(.lintFillForward(c("a", NA), c(0, 1)), c("a", "a"))
  # ties keep data order
  expect_identical(.lintFillForward(c(1, NA), c(5, 5)), c(1, 1))
})

test_that("covariate values differ by number with a tolerance, by text otherwise; NA never differs", {
  expect_false(.lintDiffers(2, 2))
  expect_true(.lintDiffers(1, 2))
  expect_false(.lintDiffers(1, 1 + 1e-12))
  expect_true(.lintDiffers(1, 1.0001))
  expect_false(.lintDiffers(NA, 2))
  expect_false(.lintDiffers(2, NA))
  expect_false(.lintDiffers(NA, NA))
  expect_true(.lintDiffers("a", "b"))
  expect_false(.lintDiffers("a", "a"))
  expect_true(.lintDiffers(TRUE, FALSE))
  expect_false(.lintDiffers(1e9, 1e9 + 1))
})

# --- reading an event table --------------------------------------------------------------------

test_that("an event table is read case-insensitively and every record is classified", {
  skip_if_not_installed("rxode2")
  spec <- pair_spec()
  ev <- data.frame(ID = c(1, 1, 1, 1, 1, 1), TIME = c(0, 24, 24, 24, 24, 5),
                   EVID = c(1, 1, 1, 1, 0, 2), AMT = c(100, 100, 100, 100, NA, NA),
                   CMT = c("depot", "depot", "depot", "depot", "central", "central"),
                   RATE = c(0, 50, -1, -2, NA, NA), OCC = 1:6)
  ev$DUR <- NA_real_
  res <- .lintEventRecords(ev, spec)
  expect_true(res$ok)
  expect_identical(res$rec$.type[res$rec$.dose], c("bolus", "fixed", "rate1", "rate2"))
  expect_identical(res$rec$.dose, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(res$rec$.lc, c(1L, 1L, 1L, 1L, NA, NA))
  expect_identical(res$cov, "OCC")
  # a fixed duration
  ev2 <- pair_events(list(c(0, "d", 1), c(5, "o", 1)), dur = 2)
  expect_identical(.lintEventRecords(ev2, spec)$rec$.type[[1L]], "dur")
})

test_that("the dose's compartment is matched by name or by position in the model's states", {
  skip_if_not_installed("rxode2")
  spec <- pair_spec()
  expect_identical(spec$state, c("depot", "central"))
  by_name <- pair_events(list(c(0, "d", 1), c(1, "d", 1)))
  by_name$cmt <- c("depot", "central")
  expect_identical(.lintEventRecords(by_name, spec)$rec$.lc, c(1L, NA))
  by_number <- pair_events(list(c(0, "d", 1), c(1, "d", 1)))
  by_number$cmt <- c(1L, 2L)
  expect_identical(.lintEventRecords(by_number, spec)$rec$.lc, c(1L, NA))
  by_factor <- pair_events(list(c(0, "d", 1)))
  by_factor$cmt <- factor("depot")
  expect_identical(.lintEventRecords(by_factor, spec)$rec$.lc, 1L)
  # no cmt column: the first compartment, which is the lagged one here
  none <- pair_events(list(c(0, "d", 1)))
  none$cmt <- NULL
  expect_identical(.lintEventRecords(none, spec)$rec$.lc, 1L)
  # a negative number turns a compartment off, it is not a dose
  neg <- pair_events(list(c(0, "d", 1)))
  neg$cmt <- -1L
  expect_identical(.lintEventRecords(neg, spec)$rec$.lc, NA_integer_)
  # NA never matches an NA position
  spec2 <- spec
  spec2$state <- "central"
  na_cmt <- pair_events(list(c(0, "d", 1)))
  na_cmt$cmt <- NA_integer_
  expect_identical(.lintEventRecords(na_cmt, spec2)$rec$.lc, NA_integer_)
})

test_that("a table the rule cannot read is reported with why, never read wrongly", {
  skip_if_not_installed("rxode2")
  spec <- pair_spec()
  expect_match(.lintEventRecords(1:3, spec)$reason, "not a data frame or event table")
  expect_match(.lintEventRecords(data.frame(), spec)$reason, "is empty")
  no_time <- pair_events(list(c(0, "d", 1)))
  no_time$time <- NULL
  expect_match(.lintEventRecords(no_time, spec)$reason, "no numeric `time` column")
  na_time <- pair_events(list(c(0, "d", 1), c(1, "o", 1)))
  na_time$time[[2L]] <- NA
  expect_match(.lintEventRecords(na_time, spec)$reason, "no numeric `time` column")
  chr_time <- pair_events(list(c(0, "d", 1)))
  chr_time$time <- "0"
  expect_match(.lintEventRecords(chr_time, spec)$reason, "no numeric `time` column")
  addl <- pair_events(list(c(0, "d", 1)))
  addl$addl <- 3L
  expect_match(.lintEventRecords(addl, spec)$reason, "uses addl")
  addl$addl <- 0L  # an addl of 0 adds nothing
  expect_true(.lintEventRecords(addl, spec)$ok)
  ss <- pair_events(list(c(0, "d", 1)))
  ss$ss <- 1L
  expect_match(.lintEventRecords(ss, spec)$reason, "steady-state")
  nostate <- spec
  nostate$state <- character()
  by_number <- pair_events(list(c(0, "d", 1)))
  by_number$cmt <- 1L
  expect_match(.lintEventRecords(by_number, nostate)$reason, "compartments are not known")
  by_default <- pair_events(list(c(0, "d", 1)))
  by_default$cmt <- NULL
  expect_match(.lintEventRecords(by_default, nostate)$reason, "compartments are not known")
  # names need no state
  expect_true(.lintEventRecords(pair_events(list(c(0, "d", 1))), nostate)$ok)
})

test_that("evid is taken from amt when the table has none, and doses need a non-zero amt", {
  skip_if_not_installed("rxode2")
  spec <- pair_spec()
  ev <- pair_events(list(c(0, "d", 1), c(1, "o", 1)))
  ev$evid <- NULL
  expect_identical(.lintEventRecords(ev, spec)$rec$.dose, c(TRUE, FALSE))
  ev2 <- pair_events(list(c(0, "d", 1), c(1, "d", 1)))
  ev2$amt[[2L]] <- 0
  expect_identical(.lintEventRecords(ev2, spec)$rec$.dose, c(TRUE, FALSE))
  ev3 <- pair_events(list(c(0, "d", 1)))
  ev3$amt <- NULL
  expect_identical(.lintEventRecords(ev3, spec)$rec$.dose, TRUE)
  ev3$evid <- NULL
  expect_identical(.lintEventRecords(ev3, spec)$rec$.dose, FALSE)
})

test_that("an event table with no covariate column has nothing to compare and no finding", {
  skip_if_not_installed("rxode2")
  spec <- pair_spec(lag = "0.5")
  ev <- pair_events(list(c(0, "d", 1), c(24, "d", 2), c(24, "o", 1)), rate = -2)
  ev$OCC <- NULL
  res <- .lintEventRecords(ev, spec)
  expect_true(res$ok)
  expect_identical(res$cov, character())
  expect_null(.lintPairCheck(res$rec, spec, "FALSE", new.env(parent = emptyenv())))
})

# --- the model ---------------------------------------------------------------------------------

test_that("the spec holds the covariates each lagged compartment's f, rate, dur and lag read", {
  skip_if_not_installed("rxode2")
  spec <- .lintLagSpec(rxode2::rxode2(make_pair_model()))
  expect_length(spec$cmts, 1L)
  ce <- spec$cmts[[1L]]
  expect_identical(ce$cmt, "depot")
  expect_identical(ce$f, "OCC")
  expect_identical(ce$rate, "OCC")
  expect_identical(ce$dur, "OCC")
  expect_identical(ce$lagcov, "OCC")
  expect_identical(spec$covs_used, "OCC")
  # a lag that reads no covariate, an f that does
  ce2 <- .lintLagSpec(rxode2::rxode2(make_pair_model(lag = "0.5")))$cmts[[1L]]
  expect_identical(ce2$lagcov, character())
  expect_identical(ce2$f, "OCC")
  # an f that reads none leaves the compartment's f out, but rate and dur still count
  ce3 <- .lintLagSpec(rxode2::rxode2(make_pair_model(f = "exp(lf)")))$cmts[[1L]]
  expect_identical(ce3$f, character())
  expect_identical(ce3$dur, "OCC")
})

test_that("a model without alag(), or with nothing that reads a covariate, has an empty spec", {
  skip_if_not_installed("rxode2")
  no_lag <- .lintLagSpec(rxode2::rxode2(make_lag_model(lag = "")))
  expect_length(no_lag$cmts, 0L)
  no_cov <- .lintLagSpec(rxode2::rxode2(make_lag_model(f = "f(depot) <- exp(lf)")))
  expect_length(no_cov$cmts, 0L)
})

test_that("the lag time is the model's alag() at the initial estimates, etas zero, on the dose's covariates", {
  skip_if_not_installed("rxode2")
  spec <- pair_spec()  # alag(depot) <- exp(ltlag) * (1 + OCC), ltlag = log(0.25)
  ce <- spec$cmts[[1L]]
  expect_equal(.lintLagTime(spec, ce, list(OCC = 0)), 0.25)
  expect_equal(.lintLagTime(spec, ce, list(OCC = 1)), 0.5)
  expect_equal(.lintLagTime(spec, ce, list(OCC = 3)), 1)
  # a covariate the table does not carry, or an NA one, gives no lag
  expect_identical(.lintLagTime(spec, ce, list()), NA_real_)
  expect_identical(.lintLagTime(spec, ce, list(OCC = NA_real_)), NA_real_)
  # a lag that is negative, or that reads a covariate the dose record does not have
  neg <- pair_spec(lag = "-1")
  expect_identical(.lintLagTime(neg, neg$cmts[[1L]], list(OCC = 1)), NA_real_)
  wt <- pair_spec(lag = "exp(ltlag) * WT")
  expect_identical(.lintLagTime(wt, wt$cmts[[1L]], list(OCC = 1)), NA_real_)
  expect_equal(.lintLagTime(wt, wt$cmts[[1L]], list(OCC = 1, WT = 4)), 1)
})

test_that("an if() in the model is followed when the lag is computed", {
  skip_if_not_installed("rxode2")
  spec <- pair_spec(lag = "tl", pre = "if (OCC == 1) { tl <- 0.25 } else { tl <- 2 }")
  ce <- spec$cmts[[1L]]
  expect_identical(ce$lagcov, "OCC")
  expect_equal(.lintLagTime(spec, ce, list(OCC = 1)), 0.25)
  expect_equal(.lintLagTime(spec, ce, list(OCC = 2)), 2)
})

test_that("the lag of each dose follows its own covariates and is cached by them", {
  skip_if_not_installed("rxode2")
  spec <- pair_spec()  # lag 0.25 * (1 + OCC)
  ce <- spec$cmts[[1L]]
  cache <- new.env(parent = emptyenv())
  expect_equal(.lintLagCached(spec, ce, list(OCC = 1), cache), 0.5)
  expect_equal(.lintLagCached(spec, ce, list(OCC = 2), cache), 0.75)
  expect_length(ls(cache), 2L)
  expect_equal(.lintLagCached(spec, ce, list(OCC = 1), cache), 0.5)
  expect_length(ls(cache), 2L)
})

# --- the setting a call runs under ---------------------------------------------------------------

test_that("the setting of a call is read when it is a literal, defaulted when absent, and all three otherwise", {
  s <- function(code) .lintSiteSettings(parse(text = code, keep.source = FALSE)[[1L]])
  expect_identical(s("rxSolve(mod, ev)"), "FALSE")
  expect_identical(s("rxSolve(mod, ev, addlKeepsCov = FALSE)"), "FALSE")
  expect_identical(s("rxSolve(mod, ev, addlKeepsCov = F)"), "FALSE")
  expect_identical(s("rxSolve(mod, ev, addlKeepsCov = TRUE)"), "TRUE")
  expect_identical(s("rxSolve(mod, ev, addlKeepsCov = T)"), "TRUE")
  expect_identical(s("rxSolve(mod, ev, nonmem = TRUE)"), "nonmem")
  expect_identical(s("rxSolve(mod, ev, nonmem = T)"), "nonmem")
  expect_identical(s("rxSolve(mod, ev, nonmem = FALSE)"), "FALSE")
  expect_identical(s("rxSolve(mod, ev, nonmem = FALSE, addlKeepsCov = TRUE)"), "TRUE")
  expect_identical(s("rxSolve(mod, ev, addlKeepsCov = NULL)"), "FALSE")
  # inside rxControl() or an inline control list
  expect_identical(s("rxSolve(mod, ev, control = rxControl(addlKeepsCov = TRUE))"), "TRUE")
  expect_identical(s("rxSolve(mod, ev, control = list(nonmem = TRUE))"), "nonmem")
  expect_identical(s("rxSolve(mod, ev, rxControl(addlKeepsCov = TRUE))"), "TRUE")
  # anything that cannot be read is checked under all three
  all3 <- c("FALSE", "TRUE", "nonmem")
  expect_identical(s("rxSolve(mod, ev, addlKeepsCov = flag)"), all3)
  expect_identical(s("rxSolve(mod, ev, nonmem = flag)"), all3)
  expect_identical(s("rxSolve(mod, ev, control = ctl)"), all3)
  expect_identical(s("rxSolve(mod, ev, control = make_control())"), all3)
  expect_identical(s("rxSolve(mod, ev, tieOrder = 'data')"), all3)
  expect_identical(s("rxSolve(mod, ev, nonmem = TRUE, addlKeepsCov = FALSE)"), all3)
  expect_identical(s("rxSolve(mod, ev, control = rxControl(addlKeepsCov = flag))"), all3)
})

# --- the check on a table ---------------------------------------------------------------------------

# Records with every record carrying the covariates of the dose that opened its
# interval: occasion 1 up to the second dose, occasion 2 from it.
ideal_records <- list(c(0, "d", 1), c(10, "o", 1), c(20, "o", 1),
                      c(24, "d", 2), c(25, "o", 2), c(30, "o", 2))
# The same doses with every observation on occasion 2, a record at the second
# dose's time and one inside its lag window.
naive_records <- list(c(0, "d", 2), c(12, "d", 1), c(12, "o", 2), c(12.25, "o", 2), c(13, "o", 2))

test_that("a table whose records carry the opening dose's covariates is clean under FALSE for every dose type", {
  skip_if_not_installed("rxode2")
  for (rate in c(0, 50, -1, -2)) {
    expect_null(pair_diffs(pair_events(ideal_records, rate = rate), "FALSE"), info = rate)
  }
  expect_null(pair_diffs(pair_events(ideal_records, dur = 2), "FALSE"))
})

test_that("under TRUE or nonmem = TRUE a bolus is clean there but an infusion reads another record", {
  skip_if_not_installed("rxode2")
  for (st in c("TRUE", "nonmem")) {
    expect_null(pair_diffs(pair_events(ideal_records, rate = 0), st), info = st)
    expect_null(pair_diffs(pair_events(ideal_records, dur = 2), st), info = st)
    # the second dose is occasion 2; a fixed-rate infusion may read any other record
    d50 <- pair_diffs(pair_events(ideal_records, rate = 50), st)
    expect_identical(sort(unique(d50$time)), c(0, 24))
    expect_identical(unique(d50$qty), "f")
    expect_identical(unique(d50$type), "fixed")
    expect_identical(sort(unique(d50$cand_time[d50$time == 24])), c(0, 10, 20))
    expect_identical(sort(unique(d50$cand_time[d50$time == 0])), c(24, 25, 30))
    # a modelled-rate infusion reads the record before it, which is occasion 1
    for (rate in c(-1, -2)) {
      d <- pair_diffs(pair_events(ideal_records, rate = rate), st)
      expect_identical(unique(d$time), 24)
      expect_identical(d$cand_time, 20)
      expect_identical(d$own, "2")
      expect_identical(d$other, "1")
    }
  }
})

test_that("a layout with other occasions' records at the dose's time is flagged under each setting that reads them", {
  skip_if_not_installed("rxode2")
  # the dose at 12 is occasion 1; records at 12 and 12.25 are occasion 2
  flagged <- function(rate, st, dur = NULL) {
    d <- pair_diffs(pair_events(naive_records, rate = rate, dur = dur), st)
    if (is.null(d)) character() else sort(unique(paste(d$qty, d$cand_time)))
  }
  expect_identical(flagged(0, "FALSE"), c("f 12", "f 12.25"))
  expect_identical(flagged(0, "TRUE"), character())
  expect_identical(flagged(0, "nonmem"), character())
  expect_identical(flagged(NULL, "FALSE", dur = 2), c("f 12", "f 12.25"))
  expect_identical(flagged(NULL, "TRUE", dur = 2), character())
  # a fixed rate: the record at the dose's time under FALSE, every other record otherwise
  expect_identical(flagged(50, "FALSE"), "f 12")
  expect_identical(flagged(50, "TRUE"), c("f 0", "f 12", "f 12.25", "f 13"))
  # rate = -1 / -2: f from the record at the dose's time, rate() / dur() from that and the window
  for (rate in c(-1, -2)) {
    expect_identical(flagged(rate, "FALSE"), c("f 12", "rate 12", "rate 12.25"), info = rate)
    # TRUE and nonmem = TRUE also read the record before the dose, the one at 0
    expect_identical(flagged(rate, "TRUE"), c("f 0", "f 12", "rate 12", "rate 12.25"), info = rate)
    expect_identical(flagged(rate, "nonmem"), c("f 0", "f 12", "rate 12", "rate 12.25"), info = rate)
  }
})

test_that("nonmem = TRUE ignores a rate() / dur() record written before the dose, the others read it", {
  skip_if_not_installed("rxode2")
  # the occasion-2 record at the dose's time comes first in the data
  before <- list(c(0, "d", 2), c(12, "o", 2), c(12, "d", 1), c(14, "o", 1))
  flagged <- function(st) {
    d <- pair_diffs(pair_events(before, rate = -2), st)
    if (is.null(d)) character() else sort(unique(paste(d$qty, d$cand_time)))
  }
  expect_identical(flagged("FALSE"), c("f 12", "rate 12"))
  expect_identical(flagged("TRUE"), c("f 0", "f 12", "rate 12"))
  expect_identical(flagged("nonmem"), c("f 0", "f 12"))
})

test_that("a difference names the dose, the record, the covariate and both values", {
  skip_if_not_installed("rxode2")
  d <- pair_diffs(pair_events(naive_records, rate = 0, id = 7L), "FALSE")
  expect_identical(d$id, c(7L, 7L))
  expect_identical(d$time, c(12, 12))
  expect_identical(d$cmt, c("depot", "depot"))
  expect_identical(d$covariate, c("OCC", "OCC"))
  expect_identical(d$own, c("1", "1"))
  expect_identical(d$other, c("2", "2"))
  expect_identical(d$cand_dose, c(FALSE, FALSE))
  expect_identical(d$cand_time, c(12, 12.25))
  expect_identical(d$setting, c("FALSE", "FALSE"))
})

test_that("each subject is checked on its own records and a dose to another compartment is ignored", {
  skip_if_not_installed("rxode2")
  a <- pair_events(naive_records, rate = 0, id = 1L)
  b <- pair_events(ideal_records, rate = 0, id = 2L)
  d <- pair_diffs(rbind(a, b), "FALSE")
  expect_identical(unique(d$id), 1L)
  # a record of another subject at the same time is not a candidate
  c1 <- pair_events(list(c(0, "d", 1), c(5, "o", 1)), id = 1L)
  c2 <- pair_events(list(c(0, "o", 9)), id = 2L)
  expect_null(pair_diffs(rbind(c1, c2), "FALSE"))
  # an intravenous dose to the central compartment is not a lagged dose
  iv <- pair_events(list(c(0, "d", 1), c(12, "d", 1), c(12, "o", 2)))
  iv$cmt[iv$time == 12 & iv$evid == 1L] <- "central"
  recs <- .lintEventRecords(iv, pair_spec(lag = "0.5"))
  expect_identical(.lintLaggedDoseCount(recs$rec), 1L)
  expect_null(pair_diffs(iv, "FALSE"))
})

test_that("covariates are compared after NA is filled forward, and an NA never differs", {
  skip_if_not_installed("rxode2")
  # the occasion-2 dose has no covariate of its own; filled from the record before (occasion 1)
  recs <- list(c(0, "d", 1), c(12, "d", NA), c(12, "o", 1), c(13, "o", 1))
  expect_null(pair_diffs(pair_events(recs, rate = 0), "FALSE"))
  # a co-timed record that is NA never supplies a different value
  recs2 <- list(c(0, "d", 1), c(12, "d", 2), c(12, "o", NA))
  expect_null(pair_diffs(pair_events(recs2, rate = 0), "FALSE"))
})

test_that("a covariate-dependent lag decides which records are inside the window", {
  skip_if_not_installed("rxode2")
  # lag = 0.25 * (1 + OCC): the dose at 12 (OCC 1) is delayed 0.5 h, so the record at 12.4 is in its window
  spec <- pair_spec()
  recs <- list(c(0, "d", 2), c(12, "d", 1), c(12.4, "o", 2), c(14, "o", 1))
  d <- pair_diffs(pair_events(recs, rate = 0), "FALSE", spec)
  expect_identical(d$cand_time, 12.4)
  expect_equal(d$lag, 0.5)
  # the same record is outside the window of an OCC-0 dose (lag 0.25)
  recs0 <- list(c(0, "d", 2), c(12, "d", 0), c(12.4, "o", 2), c(14, "o", 0))
  expect_null(pair_diffs(pair_events(recs0, rate = 0), "FALSE", spec))
})

test_that("when the lag cannot be computed every record up to the next dose is checked, and the row says so", {
  skip_if_not_installed("rxode2")
  # the lag reads WT, which the table does not carry
  spec <- pair_spec(lag = "exp(ltlag) * WT")
  recs <- list(c(0, "d", 2), c(12, "d", 1), c(12, "o", 2), c(12.25, "o", 2), c(40, "o", 2))
  d <- pair_diffs(pair_events(recs, rate = 0), "FALSE", spec)
  expect_identical(d$cand_time, c(12, 12.25, 40))
  expect_true(all(is.na(d$lag)))
  # the dose at 0 stops at the dose at 12: its records up to there are candidates, not the later ones
  recs0 <- list(c(0, "d", 2), c(5, "o", 1), c(12, "d", 2))
  d0 <- pair_diffs(pair_events(recs0, rate = 0), "FALSE", spec)
  expect_identical(d0$time, 0)
  expect_identical(d0$cand_time, 5)
})

test_that("no lag means no finding, whatever the table", {
  skip_if_not_installed("rxode2")
  for (st in c("FALSE", "TRUE", "nonmem")) {
    for (rate in c(0, 50, -1, -2)) {
      expect_null(pair_diffs(pair_events(naive_records, rate = rate), st, pair_spec(lag = "0")),
                  info = paste(st, rate))
    }
  }
})

test_that("a table with no dose, one dose, or one record is handled", {
  skip_if_not_installed("rxode2")
  expect_null(pair_diffs(pair_events(list(c(0, "o", 1), c(1, "o", 2))), "FALSE"))
  expect_null(pair_diffs(pair_events(list(c(0, "d", 1))), "FALSE"))
  expect_null(pair_diffs(pair_events(list(c(0, "o", 1))), "FALSE"))
  expect_identical(.lintLaggedDoseCount(.lintEventRecords(pair_events(list(c(0, "o", 1))), pair_spec())$rec), 0L)
})

# --- end to end -----------------------------------------------------------------------------

# An event table in the article's own code: the dose at 12 is occasion 1, the
# records at 12 and 12.25 occasion 2 (the layout of origin/main's Tsirizani
# Figure 2: a second dose on another occasion among observations of the first).
naive_table <- c(
  "ev <- data.frame(id = 1, time = c(0, 12, 12, 12.25, 13), evid = c(1, 1, 0, 0, 0),",
  "                 amt = c(100, 100, NA, NA, NA),",
  '                 cmt = c("depot", "depot", "central", "central", "central"),',
  "                 rate = c(-2, -2, NA, NA, NA), OCC = c(2, 1, 2, 2, 2))")
# The same doses with every record carrying the opening dose's occasion.
ideal_table <- c(
  "ev <- data.frame(id = 1, time = c(0, 6, 12, 14), evid = c(1, 0, 1, 0),",
  "                 amt = c(100, NA, 100, NA),",
  '                 cmt = c("depot", "central", "depot", "central"),',
  "                 rate = c(-2, NA, -2, NA), OCC = c(2, 2, 1, 1))")

test_that("an article whose event table pairs another occasion's record with a lagged dose is flagged", {
  skip_if_not_installed("rxode2")
  res <- pair_lint(c(from_db, naive_table, "sim <- rxode2::rxSolve(mod, ev, addDosing = FALSE)"))
  expect_identical(pair_checks(res), "lagged-dose-pairing")
  msg <- res$issues$message[res$issues$check == "lagged-dose-pairing"]
  expect_match(msg, "`rxode2::rxSolve(mod, ev, addDosing = FALSE)` is given an event table in which under addlKeepsCov = FALSE, 1 of 2 lagged doses may read covariates from a record other than their own: the dose at time 12 (ID 1) has OCC = 1, but its f() may read OCC = 2 from the observation or other record at time 12.", fixed = TRUE)
  expect_false(grepl("could not be read", msg))
  expect_false(grepl("lag time could not", msg))
  fix <- res$issues$fix[res$issues$check == "lagged-dose-pairing"]
  expect_match(fix, "give every record the covariates of the dose that opened its interval", fixed = TRUE)
  expect_match(fix, "state addlKeepsCov = FALSE", fixed = TRUE)
})

test_that("an article whose records carry the opening dose's covariates is clean under FALSE", {
  skip_if_not_installed("rxode2")
  res <- pair_lint(c(from_db, ideal_table, "sim <- rxode2::rxSolve(mod, ev, addlKeepsCov = FALSE)"))
  expect_identical(pair_checks(res), character())
})

test_that("the setting the call states decides what is checked", {
  skip_if_not_installed("rxode2")
  call_with <- function(args, table = ideal_table) {
    pair_checks(pair_lint(c(from_db, table, sprintf("sim <- rxode2::rxSolve(mod, ev, %s)", args))))
  }
  # a modelled-rate infusion is clean under FALSE and reads the record before it otherwise
  expect_identical(call_with("addlKeepsCov = FALSE"), character())
  expect_identical(call_with("addlKeepsCov = TRUE"), "lagged-dose-pairing")
  expect_identical(call_with("nonmem = TRUE"), "lagged-dose-pairing")
  expect_identical(call_with("control = rxControl(addlKeepsCov = TRUE)"), "lagged-dose-pairing")
  # an unreadable setting is checked under all three
  res <- pair_lint(c(from_db, ideal_table, "sim <- rxode2::rxSolve(mod, ev, addlKeepsCov = flag)"))
  msg <- res$issues$message[res$issues$check == "lagged-dose-pairing"]
  expect_match(msg, "under addlKeepsCov = TRUE, ", fixed = TRUE)
  expect_match(msg, "under nonmem = TRUE, ", fixed = TRUE)
  expect_false(grepl("under addlKeepsCov = FALSE", msg, fixed = TRUE))
  expect_match(msg, "The call's setting could not be read, so all three were checked.", fixed = TRUE)
})

test_that("a lag that cannot be computed is said in the finding", {
  skip_if_not_installed("rxode2")
  m <- make_pair_model(lag = "exp(ltlag) * WT")
  res <- pair_lint(c(from_db, naive_table, "sim <- rxode2::rxSolve(mod, ev)"), model = m)
  msg <- res$issues$message[res$issues$check == "lagged-dose-pairing"]
  expect_match(msg, "The lag time is not known from the model's initial estimates (it reads a random effect, a parameter the call supplies or a covariate the table does not carry, or could not be computed), so every record up to the dose's next dose was checked, not only those inside its lag window.", fixed = TRUE)
})

test_that("the table is built by the article's own tidyverse code", {
  skip_if_not_installed("rxode2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  code <- c(
    from_db,
    "cases <- data.frame(id = 1:3, grp = c('a', 'b', 'c'))",
    "doses <- cases |> tidyr::crossing(time = c(0, 12)) |>",
    "  dplyr::mutate(evid = 1L, amt = 100, cmt = 'depot', rate = -2, OCC = ifelse(time == 0, 2L, 1L))",
    "obs <- cases |> tidyr::crossing(time = c(12, 12.25, 13)) |>",
    "  dplyr::mutate(evid = 0L, amt = NA_real_, cmt = 'central', rate = NA_real_, OCC = 2L)",
    "ev <- dplyr::bind_rows(doses, obs) |> dplyr::arrange(id, time, dplyr::desc(evid)) |> as.data.frame()",
    "sim <- rxode2::rxSolve(mod, ev, addDosing = FALSE)")
  res <- pair_lint(code)
  expect_identical(pair_checks(res), "lagged-dose-pairing")
  expect_match(res$issues$message[res$issues$check == "lagged-dose-pairing"],
               "3 of 6 lagged doses", fixed = TRUE)
})

test_that("an event table built with et() is read", {
  skip_if_not_installed("rxode2")
  code <- c(from_db,
            'ev <- rxode2::et(amt = 100, time = 0, cmt = "depot") |> rxode2::et(c(0.5, 1), cmt = "central")',
            "sim <- rxode2::rxSolve(mod, ev)")
  expect_identical(pair_checks(pair_lint(code)), character())
})

# --- what is not run ----------------------------------------------------------------------------

test_that("an event table built with a call the sandbox does not allow is reported and the call does not run", {
  trace <- withr::local_tempfile(fileext = ".trace")
  code <- c(from_db,
            sprintf("make <- function(f) { file.create(f); data.frame(id = 1, time = 0, evid = 1, amt = 1, OCC = 1) }"),
            sprintf("ev <- make('%s')", trace),
            "sim <- rxode2::rxSolve(mod, ev)")
  res <- pair_lint(code)
  expect_identical(pair_checks(res), "lagged-dose-pairing-unverifiable")
  expect_match(res$issues$message[res$issues$check == "lagged-dose-pairing-unverifiable"],
               "its event table is built with `file.create`, which the pre-lint does not run", fixed = TRUE)
  expect_false(file.exists(trace))
  # and the same for a model read, a solve, and a shell command
  for (call in c("readModelDb('Other')", "rxode2::rxSolve(1)", "system('true')", "read.csv('x.csv')")) {
    res <- pair_lint(c(from_db, sprintf("ev <- %s", call), "sim <- rxode2::rxSolve(mod, ev)"))
    expect_true(all(pair_checks(res) == "lagged-dose-pairing-unverifiable"), info = call)
    expect_true(any(grepl("which the pre-lint does not run", res$issues$message, fixed = TRUE)), info = call)
  }
})

test_that("pairing = FALSE and the option turn the rule off, and it is on by default", {
  skip_if_not_installed("rxode2")
  code <- c(from_db, naive_table, "sim <- rxode2::rxSolve(mod, ev)")
  rmd <- write_article(code)
  expect_identical(pair_checks(lint_vignette(rmd, model = make_pair_model(lag = "0.5"), pairing = FALSE)), character())
  withr::with_options(list(nlmixr2libingest.lint_pairing = NULL), {
    expect_identical(pair_checks(lint_vignette(rmd, model = make_pair_model(lag = "0.5"))), "lagged-dose-pairing")
  })
  withr::with_options(list(nlmixr2libingest.lint_pairing = FALSE), {
    expect_identical(pair_checks(lint_vignette(rmd, model = make_pair_model(lag = "0.5"))), character())
  })
})

test_that("a model with no lagged covariate-dependent dose has no pairing findings", {
  skip_if_not_installed("rxode2")
  code <- c(from_db, naive_table, "sim <- rxode2::rxSolve(mod, ev)")
  expect_identical(pair_checks(pair_lint(code, model = make_lag_model(lag = ""))), character())
  expect_identical(pair_checks(pair_lint(code, model = make_lag_model(f = "f(depot) <- exp(lf)"))), character())
  # an article with no simulation
  expect_identical(pair_checks(pair_lint(c(from_db, naive_table))), character())
})

# --- where the event table is built ---------------------------------------------------------------

test_that("a simulation in a helper function is checked through its calls", {
  skip_if_not_installed("rxode2")
  helper <- c(
    "run <- function(ev, ...) rxode2::rxSolve(mod, ev, addDosing = FALSE, ...)",
    naive_table, "a <- run(ev)")
  res <- pair_lint(c(from_db, helper))
  expect_identical(pair_checks(res), "lagged-dose-pairing")
  # the events built from the function's parameters, by position, name and default
  code <- c(
    from_db,
    "sim_one <- function(occ_dose2, occ_obs = 2) {",
    '  ev <- data.frame(id = 1, time = c(0, 12, 12.25), evid = c(1, 1, 0), amt = c(100, 100, NA),',
    '                   cmt = c("depot", "depot", "central"), rate = c(-2, -2, NA),',
    "                   OCC = c(2, occ_dose2, occ_obs))",
    "  rxode2::rxSolve(mod, ev, addDosing = FALSE)",
    "}",
    "a <- sim_one(2)",
    "b <- sim_one(occ_dose2 = 1)")
  res <- pair_lint(code)
  expect_identical(pair_checks(res), "lagged-dose-pairing")
  msg <- res$issues$message[res$issues$check == "lagged-dose-pairing"]
  expect_match(msg, "1 of 4 lagged doses", fixed = TRUE)  # the second call: OCC 1 vs the record at 12.25 (window)
  # a wrapper that forwards its `...` to the simulation: the events are the call's argument
  fwd <- c(from_db, "run <- function(...) rxode2::rxSolve(mod, ..., addDosing = FALSE)", naive_table, "a <- run(ev)")
  expect_identical(pair_checks(pair_lint(fwd)), "lagged-dose-pairing")
  fwd_named <- c(from_db, "run <- function(...) rxode2::rxSolve(mod, ...)", naive_table, "a <- run(events = ev)")
  expect_identical(pair_checks(pair_lint(fwd_named)), "lagged-dose-pairing")
  fwd_all <- c(from_db, "run <- function(...) rxode2::rxSolve(...)", naive_table, "a <- run(mod, ev)")
  expect_identical(pair_checks(pair_lint(fwd_all)), "lagged-dose-pairing")
  # `...` is passed through
  code <- c(from_db,
            "mk <- function(...) data.frame(id = 1, time = c(0, 12, 12), evid = c(1, 1, 0), amt = c(100, 100, NA),",
            '                              cmt = c("depot", "depot", "central"), rate = c(-2, -2, NA), ...)',
            "run <- function(...) rxode2::rxSolve(mod, mk(...), addDosing = FALSE)",
            "a <- run(OCC = c(2, 1, 2))")
  expect_identical(pair_checks(pair_lint(code)), "lagged-dose-pairing")
})

test_that("a helper the article never calls, or that reads a variable the pre-lint cannot bind, is not verifiable", {
  skip_if_not_installed("rxode2")
  code <- c(from_db, "run <- function(ev) rxode2::rxSolve(mod, ev)")
  res <- pair_lint(code)
  expect_match(res$issues$message[res$issues$check == "lagged-dose-pairing-unverifiable"],
               "it is made inside `run()`, which the article does not call at the top level", fixed = TRUE)
  # called through the data masking of a tibble, so its arguments are not the article's
  code <- c(from_db,
            "run <- function(x) rxode2::rxSolve(mod, data.frame(time = 0, OCC = x))",
            "out <- dplyr::mutate(tbl, r = run(xs))")
  res <- pair_lint(code)
  expect_match(res$issues$message[res$issues$check == "lagged-dose-pairing-unverifiable"],
               "object 'xs' not found", fixed = TRUE)
})

test_that("the first iteration of a loop or an apply-family function is what is checked", {
  skip_if_not_installed("rxode2")
  body <- c(
    'ev <- data.frame(id = 1, time = c(0, 12, 12), evid = c(1, 1, 0), amt = c(100, 100, NA),',
    '                 cmt = c("depot", "depot", "central"), rate = c(-2, -2, NA), OCC = c(2, occ, 2))',
    "sim <- rxode2::rxSolve(mod, ev, addDosing = FALSE)")
  loop <- c(from_db, "for (occ in c(1, 2)) {", body, "}")
  expect_identical(pair_checks(pair_lint(loop)), "lagged-dose-pairing")
  loop_ok <- c(from_db, "for (occ in c(2, 1)) {", body, "}")
  expect_identical(pair_checks(pair_lint(loop_ok)), character())
  lap <- c(from_db, "r <- lapply(c(1, 2), function(occ) {", body, "})")
  expect_identical(pair_checks(pair_lint(lap)), "lagged-dose-pairing")
  mp <- c(from_db, "grid <- data.frame(occ = c(1, 2), k = 1:2)",
          "r <- purrr::pmap(grid, function(occ, k) {", body, "})")
  expect_identical(pair_checks(pair_lint(mp)), "lagged-dose-pairing")
  mapped <- c(from_db, "r <- Map(function(occ, k) {", body, "}, c(1, 2), 1:2)")
  expect_identical(pair_checks(pair_lint(mapped)), "lagged-dose-pairing")
  # a helper handed to lapply() by name is called on the first element
  named <- c(from_db, "go <- function(occ) {", body, "}", "r <- lapply(c(1, 2), go)")
  expect_identical(pair_checks(pair_lint(named)), "lagged-dose-pairing")
  # an anonymous function whose parameter has no first element to bind is said so
  open <- c(from_db, "r <- foreach(occ = c(1, 2)) %do% {", body, "}")
  expect_identical(pair_checks(pair_lint(open)), "lagged-dose-pairing-unverifiable")
})

test_that("an anonymous function's unbound parameter is named in the reason", {
  skip_if_not_installed("rxode2")
  code <- c(from_db, "r <- some_runner(function(occ) {",
            'ev <- data.frame(time = 0, OCC = occ)', "rxode2::rxSolve(mod, ev)", "})")
  res <- pair_lint(code)
  expect_match(res$issues$message[res$issues$check == "lagged-dose-pairing-unverifiable"],
               "it is made inside an anonymous function whose parameter `occ` the pre-lint does not bind",
               fixed = TRUE)
})

test_that("code a function's own variable depends on is run before its simulation", {
  skip_if_not_installed("rxode2")
  code <- c(
    from_db,
    "go <- function(n) {",
    "  occ <- rep(2, n)",
    "  occ[2] <- 1",
    '  ev <- data.frame(id = 1, time = c(0, 12, 12), evid = c(1, 1, 0), amt = c(100, 100, NA),',
    '                   cmt = c("depot", "depot", "central"), rate = c(-2, -2, NA), OCC = c(2, 1, occ[3]))',
    "  rxode2::rxSolve(mod, ev, addDosing = FALSE)",
    "}",
    "a <- go(3)")
  expect_identical(pair_checks(pair_lint(code)), "lagged-dose-pairing")
})

# --- what the table can be -----------------------------------------------------------------------

test_that("tables the rule cannot read are reported with the reason and counted once per reason", {
  skip_if_not_installed("rxode2")
  reason_of <- function(code) {
    res <- pair_lint(c(from_db, code, "sim <- rxode2::rxSolve(mod, ev)"))
    stopifnot(identical(pair_checks(res), "lagged-dose-pairing-unverifiable"))
    sub("^.*pairs covariates with the model's lagged doses: ", "", res$issues$message[res$issues$check == "lagged-dose-pairing-unverifiable"])
  }
  expect_identical(reason_of("ev <- c(a = 1)"), "the events argument is not a data frame or event table.")
  expect_identical(reason_of("ev <- data.frame()"), "the event table is empty.")
  expect_identical(reason_of("ev <- data.frame(id = 1, amt = 1)"),
                   "the event table has no numeric `time` column without missing values.")
  expect_match(reason_of("ev <- data.frame(id = 1, time = 0, amt = 100, evid = 1, cmt = 'depot', addl = 3, ii = 12, OCC = 1)"),
               "the event table uses addl, which the pre-lint does not expand", fixed = TRUE)
  expect_match(reason_of("ev <- data.frame(id = 1, time = 0, amt = 100, evid = 1, cmt = 'depot', ss = 1, OCC = 1)"),
               "steady-state", fixed = TRUE)
  expect_match(reason_of("ev <- stop('no table')"), "building its event table failed (no table)", fixed = TRUE)
  expect_match(reason_of("ev <- undefined_name_9999"), "object 'undefined_name_9999' not found", fixed = TRUE)
})

test_that("simulations that are not rxSolve() calls are reported as not checkable", {
  skip_if_not_installed("rxode2")
  res <- pair_lint(c(from_db, naive_table, "sim <- do.call(rxode2::rxSolve, list(mod, ev, addlKeepsCov = FALSE))"))
  expect_identical(pair_checks(res), "lagged-dose-pairing-unverifiable")
  expect_match(res$issues$message[res$issues$check == "lagged-dose-pairing-unverifiable"],
               "it is not an rxSolve() or solve(model, ...) call whose arguments can be read", fixed = TRUE)
})

test_that("several simulations that cannot be checked for one reason are one finding", {
  skip_if_not_installed("rxode2")
  code <- c(from_db, "ev <- stop('no')", "a <- rxode2::rxSolve(mod, ev)", "b <- rxode2::rxSolve(mod, ev)",
            "c <- rxode2::rxSolve(mod, ev)")
  res <- pair_lint(code)
  expect_identical(pair_checks(res), "lagged-dose-pairing-unverifiable")
  expect_match(res$issues$message[res$issues$check == "lagged-dose-pairing-unverifiable"],
               "(and 2 more simulations)", fixed = TRUE)
  res1 <- pair_lint(c(from_db, "ev <- stop('no')", "a <- rxode2::rxSolve(mod, ev)", "b <- rxode2::rxSolve(mod, ev)"))
  expect_match(res1$issues$message[res1$issues$check == "lagged-dose-pairing-unverifiable"],
               "(and 1 more simulation)", fixed = TRUE)
})

test_that("an article is run for a bounded number of simulations", {
  skip_if_not_installed("rxode2")
  sims <- sprintf("s%d <- rxode2::rxSolve(mod, ev)", seq_len(.lintPairingMaxSites + 3L))
  res <- pair_lint(c(from_db, ideal_table, sims))
  msg <- res$issues$message[res$issues$check == "lagged-dose-pairing-unverifiable"]
  expect_length(msg, 1L)
  expect_match(msg, sprintf("the pre-lint runs at most %d simulations per article", .lintPairingMaxSites), fixed = TRUE)
  expect_match(msg, "(and 2 more simulations)", fixed = TRUE)
})

test_that("a model changed by the article, or one of several that cannot be told apart, is not guessed at", {
  skip_if_not_installed("rxode2")
  code <- c('mod <- readModelDb("Some_Model") |> rxode2::model({ x <- 1 })', naive_table,
            "sim <- rxode2::rxSolve(mod, ev)")
  res <- pair_lint(code)
  expect_match(res$issues$message[res$issues$check == "lagged-dose-pairing-unverifiable"],
               "the model it solves is changed by model() in the article", fixed = TRUE)
  specs <- list(list(label = "`A`"), list(label = "`B`"))
  expect_identical(.lintSiteSpec(list(origin = "B"), specs)$spec$label, "`B`")
  expect_match(.lintSiteSpec(list(origin = NA_character_), specs)$reason, "more than one model with a lagged dose")
  expect_match(.lintSiteSpec(list(origin = "C"), specs)$reason, "more than one model with a lagged dose")
  expect_identical(.lintSiteSpec(list(origin = NA_character_), specs[1L])$spec$label, "`A`")
})

# --- what the lag and the setting depend on ------------------------------------------------------

test_that("a bolus beside another occasion's record is reported even when the lag cannot be computed", {
  skip_if_not_installed("rxode2")
  # f reads WT; the lag reads AGE, which the table does not carry
  m <- make_pair_model(lag = "exp(ltlag) * AGE", f = "exp(lf) * (WT / 70)")
  code <- c(from_db, 'ev <- data.frame(id = 1, time = c(24, 24.25), evid = c(1, 0), amt = c(100, NA),',
            '                 cmt = c("depot", "central"), WT = c(70, 90))',
            "sim <- rxode2::rxSolve(mod, ev, addDosing = FALSE)")
  res <- pair_lint(code, model = m)
  expect_identical(pair_checks(res), "lagged-dose-pairing")
  msg <- res$issues$message[res$issues$check == "lagged-dose-pairing"]
  expect_match(msg, "the dose at time 24 (ID 1) has WT = 70, but its f() may read WT = 90 from the observation or other record at time 24.25", fixed = TRUE)
  expect_match(msg, "The lag time is not known from the model's initial estimates", fixed = TRUE)
})

test_that("an unknown lag with nothing different up to the next dose is clean", {
  skip_if_not_installed("rxode2")
  m <- make_pair_model(lag = "exp(ltlag) * AGE", f = "exp(lf) * (WT / 70)")
  code <- c(from_db, 'ev <- data.frame(id = 1, time = c(0, 6, 12, 18), evid = c(1, 0, 1, 0), amt = c(100, NA, 100, NA),',
            '                 cmt = c("depot", "central", "depot", "central"), WT = c(70, 70, 90, 90))',
            "sim <- rxode2::rxSolve(mod, ev, addDosing = FALSE)")
  expect_identical(pair_checks(pair_lint(code, model = m)), character())
})

test_that("a lag that reads a random effect is not taken at its typical value", {
  skip_if_not_installed("rxode2")
  m_eta <- make_pair_model(lag = "exp(ltlag + etalag) * (1 + OCC)", ini_extra = "etalag ~ 0.1")
  spec <- .lintLagSpec(rxode2::rxode2(m_eta))
  spec$env <- .lintSandboxEnv()
  expect_true(spec$cmts[[1L]]$lag_eta)
  expect_identical(.lintLagTime(spec, spec$cmts[[1L]], list(OCC = 1)), NA_real_)
  expect_false(.lintLagSpec(rxode2::rxode2(make_pair_model()))$cmts[[1L]]$lag_eta)
  # the record 3 h after the dose is outside the typical lag (0.5 h) but inside any lag shorter than the interval
  code <- c(from_db, 'ev <- data.frame(id = 1, time = c(0, 3, 12), evid = c(1, 0, 1), amt = c(100, NA, 100),',
            '                 cmt = c("depot", "central", "depot"), OCC = c(1, 2, 2))',
            "sim <- rxode2::rxSolve(mod, ev, addDosing = FALSE)")
  expect_identical(pair_checks(pair_lint(code, model = make_pair_model(lag = "0.5"))), character())
  res <- pair_lint(code, model = m_eta)
  expect_identical(pair_checks(res), "lagged-dose-pairing")
  expect_match(res$issues$message[res$issues$check == "lagged-dose-pairing"], "The lag time is not known", fixed = TRUE)
})

test_that("parameters the call supplies make the lag unknown unless they cannot touch it", {
  skip_if_not_installed("rxode2")
  # ltlag is assigned in the model body; the lag is 0.5 h with the model's own value
  m <- make_pair_model(lag = "exp(ltlag)", pre = "ltlag <- log(0.5)")
  base_code <- c(from_db, 'ev <- data.frame(id = 1, time = c(0, 3, 12), evid = c(1, 0, 1), amt = c(100, NA, 100),',
                 '                 cmt = c("depot", "central", "depot"), OCC = c(1, 2, 2))')
  checks <- function(args) pair_checks(pair_lint(c(base_code, sprintf("sim <- rxode2::rxSolve(mod, ev%s)", args)), model = m))
  expect_identical(checks(""), character())
  expect_identical(checks(", params = c(unrelated = 1)"), character())
  expect_identical(checks(", params = c(ltlag = log(5))"), "lagged-dose-pairing")
  expect_identical(checks(", params = prm"), "lagged-dose-pairing")
  expect_identical(checks(", theta = prm"), "lagged-dose-pairing")
  expect_identical(checks(", thetaMat = th"), character())  # ltlag is not an estimated theta of this model
  expect_identical(checks(", params = c(1, 2)"), "lagged-dose-pairing")
  # a parameter object given by position
  expect_identical(checks(", prm"), "lagged-dose-pairing")
  # omega touches the lag only through an eta
  expect_identical(checks(", omega = NA"), character())
})

test_that("a call's helpers for the lag read a literal params and a thetaMat on an estimated theta", {
  expect_false(.lintParamsTouch(quote(c(a = 1)), c("b")))
  expect_true(.lintParamsTouch(quote(c(a = 1)), c("a")))
  expect_true(.lintParamsTouch(quote(c(1, 2)), c("a")))
  expect_true(.lintParamsTouch(quote(prm), c("a")))
  expect_true(.lintParamsTouch(quote(c(a = 1, 2)), c("b")))
  call <- function(code) parse(text = code, keep.source = FALSE)[[1L]]
  spec <- list(cmts = list(list(lagnames = c("tlag", "ltlag"), lagcov = "OCC")), est = list(ltlag = 0))
  expect_true(.lintSiteLagKnown(call("rxSolve(mod, ev)"), quote(mod), spec))
  expect_false(.lintSiteLagKnown(call("rxSolve(mod, ev, thetaMat = m)"), quote(mod), spec))
  expect_false(.lintSiteLagKnown(call("rxSolve(mod, ev, iCov = ic)"), quote(mod), spec))
  expect_true(.lintSiteLagKnown(call("rxSolve(mod, ev, iCov = ic)"), quote(mod), list(cmts = list(list(lagnames = "x", lagcov = character())), est = list())))
  expect_identical(.lintExtraPositional(call("rxSolve(mod, ev)"), quote(mod)), 0L)
  expect_identical(.lintExtraPositional(call("rxSolve(mod, ev, p)"), quote(mod)), 1L)
  expect_identical(.lintExtraPositional(call("rxSolve(mod, events = ev)"), quote(mod)), 0L)
  expect_identical(.lintExtraPositional(call("rxSolve(mod, p, events = ev)"), quote(mod)), 1L)
})

test_that("a model that cannot be read or analysed is a finding, not a silent skip", {
  scan <- .lintScanSolves(.lintRchunks(write_article(c(from_db, "sim <- rxode2::rxSolve(mod, ev)"))), character())
  got <- .lintPairing(scan, list(list(label = "`Some_Model`", ui = NULL, error = "no such model")))
  expect_identical(vapply(got, function(i) i$check, ""), "lagged-dose-pairing-unverifiable")
  expect_match(got[[1L]]$message, "the model `Some_Model` could not be read (no such model)", fixed = TRUE)
  got <- .lintPairing(scan, list(list(label = "`Some_Model`", ui = list(), error = NULL)))
  expect_match(got[[1L]]$message, "the model `Some_Model` could not be analysed (the model has no readable statements)", fixed = TRUE)
  # with no simulation there is nothing to report
  scan0 <- .lintScanSolves(.lintRchunks(write_article("x <- 1")), character())
  expect_length(.lintPairing(scan0, list(list(label = "`A`", ui = NULL, error = "boom"))), 0L)
})

test_that("an rxode2 option set in the article, or an object passed by position, makes the setting unreadable", {
  skip_if_not_installed("rxode2")
  code <- c(from_db, ideal_table, "sim <- rxode2::rxSolve(mod, ev)")
  # a modelled-rate infusion is clean under FALSE, flagged under TRUE
  expect_identical(pair_checks(pair_lint(code)), character())
  res <- pair_lint(c("options(rxode2.addlKeepsCov = TRUE)", code))
  expect_identical(pair_checks(res), "lagged-dose-pairing")
  expect_match(res$issues$message[res$issues$check == "lagged-dose-pairing"],
               "The call's setting could not be read, so all three were checked.", fixed = TRUE)
  expect_identical(pair_checks(pair_lint(c("options(other.option = TRUE)", code))), character())
  res <- pair_lint(c(from_db, ideal_table, "sim <- rxode2::rxSolve(mod, ev, ctl)"))
  expect_identical(pair_checks(res), "lagged-dose-pairing")
  expect_true(.lintSetsRxodeOptions(list(quote(options(rxode2.nonmem = TRUE)))))
  expect_true(.lintSetsRxodeOptions(list(quote(base::options(rxode2.x = 1)))))
  expect_false(.lintSetsRxodeOptions(list(quote(options(digits = 3)), quote(x <- 1))))
})

test_that("two dose records at each dose time, into different compartments, are not a mismatch", {
  skip_if_not_installed("rxode2")
  # the library idiom: an infusion (rate = -1) into the lagged compartment and a bolus into another
  # at every dose time, both carrying the dose's own covariates; the record before each dose carries
  # the previous dose's
  code <- c(from_db,
            "ev <- data.frame(id = 1, time = c(0, 0, 12, 24, 24, 30), evid = c(1, 1, 0, 1, 1, 0), amt = c(100, 100, NA, 100, 100, NA),",
            '                 cmt = c("depot", "central", "central", "depot", "central", "central"), rate = c(-1, 0, NA, -1, 0, NA),',
            "                 OCC = c(1, 1, 1, 2, 2, 2))",
            "sim <- rxode2::rxSolve(mod, ev, addDosing = FALSE)")
  expect_identical(pair_checks(pair_lint(code, model = make_pair_model(lag = "0.5"))), character())
  # the second record into the SAME compartment leaves the record before the dose a candidate
  same <- sub('"depot", "central", "central", "depot", "central", "central"', '"depot", "depot", "central", "depot", "depot", "central"', code, fixed = TRUE)
  same <- sub("rate = c(-1, 0, NA, -1, 0, NA)", "rate = c(-1, -1, NA, -1, -1, NA)", same, fixed = TRUE)
  res <- pair_lint(same, model = make_pair_model(lag = "0.5"))
  expect_identical(pair_checks(res), "lagged-dose-pairing")
  expect_match(res$issues$message[res$issues$check == "lagged-dose-pairing"],
               "the dose at time 24 (ID 1) has OCC = 2, but its f() may read OCC = 1 from the observation or other record at time 12", fixed = TRUE)
})

# --- pieces of the targets -----------------------------------------------------------------------

test_that("a call's arguments are bound to a function's parameters by name, position and default", {
  f <- formals(function(a, b = 2, ...) NULL)
  b <- .lintBindCall(f, list(quote(x), b = quote(y), quote(z), w = quote(v)))
  expect_identical(vapply(b$stmts, function(s) paste(deparse(s), collapse = ""), ""), c("a <- x", "b <- y"))
  expect_identical(b$dots, list(quote(z), w = quote(v)))
  d <- .lintBindCall(f, list(quote(x)))
  expect_identical(vapply(d$stmts, function(s) paste(deparse(s), collapse = ""), ""), c("a <- x", "b <- 2"))
  expect_identical(d$dots, list())
  # an argument that is not given and has no default binds nothing
  g <- .lintBindCall(formals(function(a, b) NULL), list(b = quote(1)))
  expect_identical(vapply(g$stmts, function(s) paste(deparse(s), collapse = ""), ""), "b <- 1")
  expect_null(.lintBindCall(formals(function(a) NULL), list(quote(1)))$dots)
})

test_that("`...` is replaced by the call's remaining arguments, an empty argument is kept", {
  e <- quote(list(1, ..., x[1, ]))
  got <- .lintSubstituteDots(e, list(quote(a), k = quote(b)))
  expect_identical(deparse(got), "list(1, a, k = b, x[1, ])")
  expect_identical(deparse(.lintSubstituteDots(quote(f(...)), list())), "f()")
  expect_identical(.lintSubstituteDots(quote(x), list(1)), quote(x))
})

test_that("calls to a function are found at the top level, and as a value handed to lapply()", {
  acc <- new.env(parent = emptyenv())
  acc$found <- list()
  for (k in 1:3) .lintCallsTo(list(quote(a <- f(1, y = 2)), quote(lapply(xs, f, z = 3)),
                                   quote(g <- function() f(0)))[[k]], "f", k, acc)
  got <- vapply(acc$found, function(x) paste(deparse(x$call), collapse = ""), "")
  expect_identical(got, c("f(1, y = 2)", "f((xs)[[1L]], z = 3)"))
  expect_identical(vapply(acc$found, function(x) x$top, 0L), 1:2)
})

test_that("a loop's variable takes its first value and a block's earlier statements run first", {
  frames <- list(list(kind = "for", var = "i", over = quote(seq_len(3))),
                 list(kind = "block", before = list(quote(a <- i + 1), quote(b <- a))),
                 list(kind = "lambda", params = c("p", "q"), apply = list(fn = "Map", args = list(quote(P), quote(Q)))))
  fs <- .lintFrameStatements(frames)
  expect_identical(vapply(fs$stmts, function(s) paste(deparse(s), collapse = ""), ""),
                   c("i <- (seq_len(3))[[1L]]", "a <- i + 1", "b <- a", "p <- (P)[[1L]]", "q <- (Q)[[1L]]"))
  expect_identical(fs$lambda, character())
  open <- .lintFrameStatements(list(list(kind = "lambda", params = "p", apply = NULL)))
  expect_identical(open$lambda, "p")
})

# --- against rxode2 itself -------------------------------------------------------------------------

# Every record of a table carries its own index as the covariate V, and the
# quantity measured is a function of V alone, so a result names the record that
# supplied it. The designated dose goes to `depot`, every other dose to `depot2`
# (same f, lag, rate and dur); the table ends with a measuring record V = 99 that
# supplies nothing of its own.
oracle_models <- function() {
  both <- function(rest) rxode2::rxode2(paste(c(
    "alag(depot) <- LAG", "alag(depot2) <- LAG", rest), collapse = "\n"))
  list(
    f = both(c("f(depot) <- V / 1000", "f(depot2) <- V / 1000",
               "rate(depot) <- 50", "dur(depot) <- 2", "rate(depot2) <- 50", "dur(depot2) <- 2",
               "d/dt(depot) <- -50 * depot", "d/dt(central) <- 50 * depot",
               "d/dt(depot2) <- -50 * depot2", "d/dt(central2) <- 50 * depot2")),
    rate = both(c("rate(depot) <- V", "dur(depot) <- 100 / V", "rate(depot2) <- V",
                  "dur(depot2) <- 100 / V",
                  "d/dt(depot) <- 0", "d/dt(central) <- 0", "d/dt(depot2) <- 0",
                  "d/dt(central2) <- 0")))
}

# One random table: records around a dose at time 20, in random order at equal
# times, and sometimes a record at time 0. The other records are observations,
# or with `doses` also doses into another compartment at the target's time or
# after it (a dose before the target puts an infusion in flight, and the quantity
# measured on one compartment no longer names a single record). `decoy` adds a
# second subject whose records have V + 100 at the same times, in the opposite
# order at equal times; `scramble` writes the groups of equal times in a random
# order (the rows of one time keep their order), as an unsorted table.
# Returns list(ev, target, is_dose, amt, decoy) with the target's row in `ev`.
oracle_table <- function(lag, type, amt = 100, doses = FALSE, decoy = FALSE, scramble = FALSE) {
  t0 <- 20
  pool <- c(0, t0 - 10, t0 - 5, t0 - 1, t0 - 1, t0, t0, t0 + lag / 2, t0 + lag, t0 + lag + 0.25, t0 + 3)
  n <- sample(1:5, 1L)
  time <- c(t0, sample(pool, n, replace = TRUE))
  is_dose <- c(TRUE, if (doses) runif(n) < 0.4 & time[-1L] >= t0 else rep(FALSE, n))
  ord <- order(time, runif(length(time)))
  if (scramble) {
    key <- stats::setNames(runif(length(unique(time))), unique(time))
    ord <- order(key[as.character(time[ord])], seq_along(ord))
    ord <- order(time, runif(length(time)))[ord]
  }
  time <- time[ord]
  is_dose <- is_dose[ord]
  target <- which(ord == 1L)
  k <- length(time)
  one <- function(id, v, tm, dz) {
    ev <- data.frame(id = id, time = tm, evid = ifelse(dz, 1L, 0L), amt = ifelse(dz, amt, NA_real_),
                     cmt = ifelse(dz, ifelse(seq_along(tm) == target & id == 1, "depot", "depot2"), "central"),
                     V = v)
    if (identical(type, "dur")) ev$dur <- ifelse(dz, 2, NA_real_)
    else ev$rate <- ifelse(dz, type, NA_real_)
    ev
  }
  ev <- one(1, seq_len(k), time, is_dose)
  if (decoy) {
    rev_ord <- order(time, -seq_len(k))
    ev <- rbind(ev, one(2, 100 + seq_len(k), time[rev_ord], is_dose[rev_ord]))
  }
  list(ev = ev, target = target, is_dose = is_dose, amt = amt, decoy = decoy, n = k)
}

# The records the solver saw for subject 1: the table and the measuring record
# appended to it, as record times and which of them are doses. The measuring
# record (V = 99) is the last record, so a read of "a later record" can land on it.
oracle_seen <- function(tbl, lag, what) {
  one <- tbl$ev[tbl$ev$id == 1, ]
  t_meas <- if (identical(what, "f")) max(one$time) + 50 else max(one$time, one$time[[tbl$target]] + lag) + 1
  list(time = c(one$time, t_meas), dose = c(one$evid == 1L, FALSE), V = c(one$V, 99))
}

# Which record (its V) supplied f() or rate() of the designated dose.
oracle_supplier <- function(models, tbl, lag, what, setting) {
  one <- tbl$ev[tbl$ev$id == 1, ]
  t0 <- one$time[[tbl$target]]
  tm <- if (identical(what, "f")) max(one$time) + 50 else max(one$time, t0 + lag) + 1
  meas <- tbl$ev[1L, ]
  meas[1L, ] <- NA
  meas$id <- 1; meas$time <- tm
  meas$evid <- 0L; meas$cmt <- "central"; meas$V <- 99
  ev <- rbind(one, meas)
  if (tbl$decoy) {
    other <- tbl$ev[tbl$ev$id == 2, ]
    m2 <- meas
    m2$id <- 2
    m2$V <- 199
    ev <- rbind(ev, other, m2)
  }
  args <- switch(setting, `FALSE` = list(addlKeepsCov = FALSE), `TRUE` = list(addlKeepsCov = TRUE),
                 nonmem = list(nonmem = TRUE))
  r <- do.call(rxode2::rxSolve, c(list(models[[what]], c(LAG = lag), ev, addDosing = FALSE,
                                       returnType = "data.frame"), args))
  if (!is.null(r$id)) r <- r[r$id == 1, ]
  if (identical(what, "f")) {
    round(r$central[r$time == tm][[1L]] / tbl$amt * 1000)
  } else {
    depot <- r$depot[r$time == tm][[1L]]
    # the measuring record's rate (99) has emptied the dose by the time it is read
    if (isTRUE(all.equal(depot, 100))) 99 else round(depot / (tm - (t0 + lag)))
  }
}

oracle_type_of <- function(type) {
  if (identical(type, "dur")) "dur" else if (type == 0) "bolus" else if (type > 0) "fixed" else "modelled"
}

# One differential check: the record the solver read must be the dose's own or a
# candidate of the rule (compared by V, so the order the rows were written in
# does not matter). For a fixed rate under TRUE / nonmem = TRUE, where every other
# record is a candidate, it must also be one of the records the measured layouts
# allow: the record at time 0, a record at the dose's time, one between the dose
# and the end of its infusion (arrival plus amt / 50), or its own; never a record
# before the dose other than the one at time 0.
oracle_check <- function(models, tbl, lag, type, what, st, info) {
  got <- oracle_supplier(models, tbl, lag, what, st)
  seen <- oracle_seen(tbl, lag, what)
  one <- tbl$ev[tbl$ev$id == 1, ]
  pos <- tbl$target
  cand <- .lintPairCandidates(seen$time, pos, lag, what, oracle_type_of(type), st, seen$dose)
  ok <- got %in% c(one$V[[pos]], seen$V[cand])
  testthat::expect_true(ok, info = paste(info, "supplier", got))
  if (identical(type, 50) && st != "FALSE" && identical(what, "f")) {
    t0 <- one$time[[pos]]
    allowed <- c(one$V[[pos]], seen$V[seen$time == 0], seen$V[seen$time == t0],
                 seen$V[seen$time > t0 & seen$time <= t0 + lag + tbl$amt / 50 + 0.5])
    testthat::expect_true(got %in% allowed, info = paste(info, "(measured fixed-rate TRUE layouts) supplier", got))
  }
  invisible(ok)
}

oracle_run <- function(models, seed, n, doses, decoy = FALSE, scramble = FALSE) {
  set.seed(seed)
  checked <- 0L
  for (rep in seq_len(n)) {
    type <- sample(list(0, 50, -1, -2, "dur"), 1L)[[1L]]
    lag <- sample(c(0.25, 0.5, 1, 2), 1L)
    # the amount changes how far a fixed-rate infusion reaches; rate() / dur() are
    # measured with 100 units
    amt <- if (is.numeric(type) && type < 0) 100 else sample(c(50, 100, 101, 150, 200), 1L)
    tbl <- oracle_table(lag, type, amt, doses = doses, decoy = decoy, scramble = scramble)
    for (st in c("FALSE", "TRUE", "nonmem")) {
      for (what in if (is.numeric(type) && type < 0) c("f", "rate") else "f") {
        info <- sprintf("type %s amt %s lag %s %s %s: times %s target %d", type, amt, lag, what, st,
                        paste(tbl$ev$time[tbl$ev$id == 1], collapse = " "), tbl$target)
        oracle_check(models, tbl, lag, type, what, st, info)
        checked <- checked + 1L
      }
    }
  }
  checked
}

test_that("the record rxode2 reads is always one the rule treats as a candidate or the dose's own", {
  skip_on_cran()
  skip_if_not_installed("rxode2")
  expect_gt(oracle_run(oracle_models(), 20260713, 40, doses = FALSE), 100L)
})

test_that("doses into another compartment at or after the dose, and other amounts, stay candidates", {
  skip_on_cran()
  skip_if_not_installed("rxode2")
  expect_gt(oracle_run(oracle_models(), 20260714, 40, doses = TRUE), 100L)
})

test_that("a second subject's records and an unsorted table do not change what the first subject reads", {
  skip_on_cran()
  skip_if_not_installed("rxode2")
  models <- oracle_models()
  expect_gt(oracle_run(models, 20260715, 25, doses = FALSE, decoy = TRUE), 60L)
  expect_gt(oracle_run(models, 20260716, 25, doses = FALSE, decoy = TRUE, scramble = TRUE), 60L)
})

test_that("with several records at the time before the dose, a modelled rate reads one of them", {
  skip_on_cran()
  skip_if_not_installed("rxode2")
  models <- oracle_models()
  for (rate in c(-1, -2)) for (st in c("TRUE", "nonmem")) {
    # three observations at 19 (V 1, 2, 3), one at 5, then the dose at 20 (V 4)
    ev <- data.frame(id = 1, time = c(5, 19, 19, 19, 20), evid = c(0, 0, 0, 0, 1), amt = c(NA, NA, NA, NA, 100),
                     cmt = c("central", "central", "central", "central", "depot"), rate = c(NA, NA, NA, NA, rate),
                     V = c(5, 1, 2, 3, 4))
    tbl <- list(ev = ev, target = 5L, is_dose = ev$evid == 1L, amt = 100, decoy = FALSE, n = 5L)
    got <- oracle_supplier(models, tbl, 0.5, "f", st)
    seen <- oracle_seen(tbl, 0.5, "f")
    cand <- .lintPairCandidates(seen$time, 5L, 0.5, "f", "modelled", st, seen$dose)
    expect_true(got %in% c(1, 2, 3), info = paste(rate, st, got))
    expect_setequal(seen$V[cand], c(1, 2, 3))
  }
})

test_that("the rule's known-answer cells agree with rxode2 for the measured layouts", {
  skip_on_cran()
  skip_if_not_installed("rxode2")
  models <- oracle_models()
  # layout "co_before" and "window" of the table above: dose at 24 among records
  # at 0 and 20, a record at the dose's time before the dose, and one inside the window
  mk <- function(near_time, near_first) {
    rows <- if (near_first) list(c(0, 0), c(20, 0), c(near_time, 0), c(24, 1)) else list(c(0, 0), c(20, 0), c(24, 1), c(near_time, 0))
    time <- vapply(rows, `[[`, 0, 1)
    is_dose <- vapply(rows, `[[`, 0, 2) == 1
    k <- length(time)
    list(ev = data.frame(id = 1, time = time, evid = ifelse(is_dose, 1L, 0L), amt = ifelse(is_dose, 100, NA_real_),
                         cmt = ifelse(is_dose, "depot", "central"), rate = ifelse(is_dose, 0, NA_real_), V = seq_len(k)),
         target = which(is_dose), is_dose = is_dose, amt = 100, decoy = FALSE, n = k)
  }
  # bolus, f(): FALSE reads the record at the dose's time (either order) and one in the window
  for (case in list(list(24, TRUE), list(24, FALSE), list(24.25, FALSE))) {
    tbl <- do.call(mk, case)
    near <- setdiff(seq_along(tbl$ev$time), c(tbl$target, 1:2))
    expect_identical(oracle_supplier(models, tbl, 0.5, "f", "FALSE"), as.numeric(near))
    expect_identical(oracle_supplier(models, tbl, 0.5, "f", "TRUE"), as.numeric(tbl$target))
    expect_identical(oracle_supplier(models, tbl, 0.5, "f", "nonmem"), as.numeric(tbl$target))
  }
  # a record exactly at the arrival time is read for a bolus's f()
  tbl <- mk(24.5, FALSE)
  expect_identical(oracle_supplier(models, tbl, 0.5, "f", "FALSE"), 4)
  # and one after it is not
  tbl <- mk(24.7, FALSE)
  expect_identical(oracle_supplier(models, tbl, 0.5, "f", "FALSE"), 3)
})

test_that("a simulation of another library model is not about the lagged dose", {
  skip_if_not_installed("rxode2")
  models <- list(list(label = "`A`", ui = rxode2::rxode2(make_pair_model(lag = "0.5"))))
  article <- function(code) {
    chunks <- .lintRchunks(write_article(code))
    .lintScanSolves(chunks, "A")
  }
  # `er` was read from "B"; its simulation is skipped whatever its table is
  scan <- article(c('mod <- readModelDb("A")', 'er <- readModelDb("B")', "ev <- stop('no')",
                    "sim <- rxode2::rxSolve(er, ev)"))
  expect_length(.lintPairing(scan, models), 0L)
  # the same table solved with the lagged model is not
  scan <- article(c('mod <- readModelDb("A")', 'er <- readModelDb("B")', "ev <- stop('no')",
                    "sim <- rxode2::rxSolve(mod, ev)"))
  expect_identical(vapply(.lintPairing(scan, models), function(i) i$check, ""), "lagged-dose-pairing-unverifiable")
  # a model given as a function has no name to compare, so nothing is skipped
  models_fn <- list(list(label = "the supplied model", ui = rxode2::rxode2(make_pair_model(lag = "0.5"))))
  scan <- article(c('er <- readModelDb("B")', "ev <- stop('no')", "sim <- rxode2::rxSolve(er, ev)"))
  expect_length(.lintPairing(scan, models_fn), 1L)
  expect_identical(.lintLabelName("`A`"), "A")
  expect_identical(.lintLabelName("the supplied model"), NA_character_)
})
