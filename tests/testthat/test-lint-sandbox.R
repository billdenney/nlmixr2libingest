# The sandbox the pairing rule runs an article's event-table code in
# (R/lint-sandbox.R): what it allows, that everything else is refused without
# running, and the slice of the article it runs.

parse1 <- function(x) parse(text = x, keep.source = FALSE)[[1L]]
sandbox <- function(stmts, target, ...) {
  .lintSandboxRun(lapply(stmts, parse1), parse1(target), ...)
}

test_that("the code of an event-table pipeline runs and gives its value", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")
  skip_if_not_installed("tidyr")
  res <- sandbox(
    c("times <- seq(0, 48, by = 24)",
      paste("mk <- function(id) dplyr::bind_rows(",
            "tibble::tibble(id = id, time = times, evid = 1, OCC = seq_along(times)),",
            "tibble::tibble(id = id, time = seq(0, 48, 4), evid = 0, OCC = 2))"),
      "ev <- dplyr::bind_rows(lapply(1:3, mk)) |> dplyr::arrange(id, time)"),
    "ev")
  expect_true(res$ok)
  expect_identical(dim(res$value), c(48L, 4L))
  expect_identical(sort(unique(res$value$id)), 1:3)
  res <- sandbox(character(), "tidyr::crossing(id = 1:3, time = c(0, 1))")
  expect_identical(dim(res$value), c(6L, 2L))
  res <- sandbox("m <- matrix(1:6, 2)", "m[1, ]")  # an empty argument
  expect_identical(res$value, c(1L, 3L, 5L))
})

test_that("rxode2::et builds a table in the sandbox", {
  skip_if_not_installed("rxode2")
  res <- sandbox("ev <- rxode2::et(amt = 100, time = 0) |> rxode2::et(seq(0, 24, 4))",
                 "as.data.frame(ev)")
  expect_true(res$ok)
  expect_identical(nrow(res$value), 8L)
})

test_that("every listed name resolves, so a name in the wrong package is caught", {
  wl <- .lintSandboxEnv()
  for (pkg in names(.lintSandboxPackages)) {
    if (!requireNamespace(pkg, quietly = TRUE)) next
    missing_names <- setdiff(.lintSandboxPackages[[pkg]], ls(wl, all.names = TRUE))
    expect_identical(missing_names, character(), info = pkg)
  }
})

test_that("the sandbox environment's parent is empty and it holds none of the names that reach outside", {
  skip_if_not_installed("dplyr")
  wl <- .lintSandboxEnv()
  expect_identical(parent.env(wl), emptyenv())
  outside <- c(
    "system", "system2", "shell", "file", "url", "pipe", "socketConnection",
    "readLines", "readRDS", "readline", "scan", "load", "save", "saveRDS",
    "writeLines", "write", "cat", "sink", "unlink", "file.create", "file.remove",
    "file.copy", "file.rename", "dir.create", "setwd", "getwd", "list.files",
    "source", "sys.source", "library", "require", "requireNamespace",
    "loadNamespace", "attachNamespace", "asNamespace", "getNamespace",
    "getFromNamespace", "environment", "globalenv", "baseenv", "emptyenv",
    "as.environment", "parent.frame", "parent.env", "sys.call", "sys.function",
    "eval", "evalq", "eval.parent", "parse", "deparse", "get", "get0", "mget",
    "exists", "assign", "match.fun", "match.arg", "Sys.getenv", "Sys.setenv",
    "Sys.time", "Sys.Date", "Sys.sleep", "Sys.setlocale", "options", "getOption",
    "download.file", "unserialize", "serialize", "dyn.load", ".Internal", ".Call",
    ".External", ".C", ".GlobalEnv", "rxSolve", "rxode2", "readModelDb", "modellib",
    "nlmixr2", "zeroRe", "solve", "local", "body<-", "formals<-", "environment<-")
  for (nm in outside) expect_false(exists(nm, envir = wl), info = nm)
  # the only `::` is the sandbox's own
  expect_identical(get("::", envir = wl)(dplyr, mutate), get("mutate", envir = wl))
})

test_that("`::` returns a listed function and stops on any other", {
  skip_if_not_installed("dplyr")
  expect_identical(.lintSandboxColon(dplyr, mutate), getExportedValue("dplyr", "mutate"))
  expect_identical(.lintSandboxColon(stats, rnorm), stats::rnorm)
  expect_error(.lintSandboxColon(base, system), "`base::system` is not allowed")
  expect_error(.lintSandboxColon(rxode2, rxSolve), "`rxode2::rxSolve` is not allowed")
  expect_error(.lintSandboxColon(nlmixr2lib, readModelDb), "is not allowed")
  expect_error(.lintSandboxColon(dplyr, no_such_function_9999), "is not allowed")
})

# --- what is refused, and that it did not run --------------------------------------------

# Code that would leave a trace if it ran: a file, a global variable, an option.
side_effects <- function(trace) {
  list(
    `writeLines to a file` = sprintf("writeLines('x', '%s')", trace),
    `file.create` = sprintf("file.create('%s')", trace),
    `system` = sprintf("system('touch %s')", trace),
    `system2` = sprintf("system2('touch', '%s')", trace),
    `base::file.create` = sprintf("base::file.create('%s')", trace),
    `base:::file.create` = sprintf("base:::file.create('%s')", trace),
    `do.call with the name as a string` = sprintf("do.call('file.create', list('%s'))", trace),
    `lapply with the name as a string` = sprintf("lapply('%s', 'file.create')", trace),
    `Reduce with the name as a string` = sprintf("Reduce('file.create', list('%s'))", trace),
    `Map with the name as a string` = sprintf("Map('file.create', '%s')", trace),
    `get` = sprintf("get('file.create')('%s')", trace),
    `match.fun` = sprintf("match.fun('file.create')('%s')", trace),
    `eval of a quoted call` = sprintf("eval(quote(file.create('%s')))", trace),
    `eval(parse())` = sprintf("eval(parse(text = \"file.create('%s')\"))", trace),
    `a global assignment` = "assign('pairing_sandbox_canary', 1, envir = globalenv())",
    `<<- to the global environment` = "(function() pairing_sandbox_canary <<- 1)()",
    `Sys.setenv` = "Sys.setenv(PAIRING_SANDBOX_CANARY = '1')",
    `options` = "options(pairing_sandbox_canary = 1)",
    `setwd` = "setwd(tempdir())",
    `library` = "library(tools)",
    `source` = sprintf("source('%s')", trace),
    `rxode2::rxSolve` = "rxode2::rxSolve(1)",
    `rxSolve` = "rxSolve(1)",
    `readModelDb` = "readModelDb('Some_Model')",
    `nlmixr2lib::readModelDb` = "nlmixr2lib::readModelDb('Some_Model')",
    `rxode2::zeroRe` = "rxode2::zeroRe(1)",
    `a call to an undefined function` = "no_such_function_9999(1)"
  )
}

test_that("code that reaches outside the sandbox is refused and does not run", {
  trace <- withr::local_tempfile(fileext = ".trace")
  old <- Sys.getenv("PAIRING_SANDBOX_CANARY", unset = NA)
  expect_false(exists("pairing_sandbox_canary", envir = globalenv()))
  for (nm in names(side_effects(trace))) {
    res <- sandbox(character(), side_effects(trace)[[nm]])
    expect_false(res$ok, info = nm)
    expect_match(res$reason, "^(its event table is built with|building its event table failed)", info = nm)
  }
  expect_false(file.exists(trace))
  expect_false(exists("pairing_sandbox_canary", envir = globalenv()))
  expect_identical(Sys.getenv("PAIRING_SANDBOX_CANARY", unset = NA), old)
  expect_null(getOption("pairing_sandbox_canary"))
})

test_that("a call the sandbox does not allow is named in the reason, before anything runs", {
  trace <- withr::local_tempfile(fileext = ".trace")
  res <- sandbox(sprintf("x <- file.create('%s')", trace), "x")
  expect_false(res$ok)
  expect_identical(res$reason, "its event table is built with `file.create`, which the pre-lint does not run")
  res <- sandbox("ev <- nlmixr2lib::readModelDb('Some_Model')", "ev")
  expect_match(res$reason, "`nlmixr2lib::readModelDb`", fixed = TRUE)
  res <- sandbox("f <- function() system('x')", "f()")
  expect_match(res$reason, "`system`", fixed = TRUE)
  res <- sandbox(c("a <- readLines('x')", "b <- system('y')", "c <- file('z')", "d <- url('w')"), "a")
  expect_match(res$reason, "and others", fixed = TRUE)
  # a function the slice defines itself is its own
  res <- sandbox("helper <- function(x) x + 1", "helper(1)")
  expect_identical(res$value, 2)
})

# --- limits ----------------------------------------------------------------------------

test_that("the run has a time limit and a row cap", {
  res <- sandbox(character(), "{ while (TRUE) NULL }", seconds = 1)
  expect_false(res$ok)
  expect_identical(res$reason, "its event table took longer than 1 seconds to build")
  res <- sandbox(character(), "data.frame(a = 1:10)", max_rows = 5L)
  expect_false(res$ok)
  expect_identical(res$reason, "its event table has 10 rows (the pre-lint stops at 5)")
  expect_true(sandbox(character(), "data.frame(a = 1:5)", max_rows = 5L)$ok)
  # the limit does not outlive the run
  expect_true(sandbox(character(), "1 + 1")$ok)
})

test_that("a failure is reported with its message, not raised", {
  res <- sandbox(character(), "stop('boom')")
  expect_false(res$ok)
  expect_identical(res$reason, "building its event table failed (boom)")
  res <- sandbox(character(), "an_undefined_variable_9999 + 1")
  expect_match(res$reason, "object 'an_undefined_variable_9999' not found", fixed = TRUE)
})

test_that("the session's random-number stream is put back", {
  set.seed(42)
  expected <- runif(2)
  set.seed(42)
  res <- sandbox("set.seed(7)", "rnorm(1)")
  expect_true(res$ok)
  expect_identical(runif(2), expected)
  # and a session with no stream has none afterwards
  if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv())
  sandbox(character(), "runif(1)")
  expect_false(exists(".Random.seed", envir = globalenv()))
  # the draws repeat for the same seed
  a <- sandbox("set.seed(1)", "rnorm(2)")$value
  b <- sandbox("set.seed(1)", "rnorm(2)")$value
  expect_identical(a, b)
})

# --- the slice ---------------------------------------------------------------------------

slice_of <- function(code, wanted, top = length(code) + 1L) {
  exprs <- lapply(code, parse1)
  vapply(.lintSlice(exprs, top, parse1(wanted)), function(e) paste(deparse(e), collapse = ""), "")
}

test_that("the slice keeps what the events depend on and drops the rest", {
  code <- c("library(dplyr)", "a <- 1", "unused <- readLines('x')", "b <- a + 1",
            "ev <- tibble::tibble(x = b)", "other <- 5")
  expect_identical(slice_of(code, "ev"), c("a <- 1", "b <- a + 1", "ev <- tibble::tibble(x = b)"))
})

test_that("a rebinding hides the earlier binding it does not read", {
  code <- c("b <- readLines('x')", "b <- 5", "ev <- data.frame(x = b)")
  expect_identical(slice_of(code, "ev"), c("b <- 5", "ev <- data.frame(x = b)"))
  # one that reads it does not
  code <- c("b <- 5", "b <- b + 1", "ev <- data.frame(x = b)")
  expect_identical(slice_of(code, "ev"), c("b <- 5", "b <- b + 1", "ev <- data.frame(x = b)"))
})

test_that("replacements, loops and conditionals that bind a wanted name are kept", {
  code <- c("ev <- data.frame(x = 1)", "ev$y <- 2", 'names(ev)[1] <- "z"',
            'for (i in 1:2) ev[[paste0("v", i)]] <- i', "if (TRUE) ev$w <- 3", "unrelated <- 1")
  expect_identical(slice_of(code, "ev"), code[1:5])
})

test_that("a helper function and the seed are pulled in", {
  code <- c("set.seed(3)", "mk <- function(n) data.frame(x = rnorm(n))", "noise <- 1",
            "ev <- mk(2)")
  expect_identical(slice_of(code, "ev"), c("set.seed(3)", "mk <- function(n) data.frame(x = rnorm(n))", "ev <- mk(2)"))
})

test_that("the slice stops at the statement before `top`", {
  code <- c("a <- 1", "ev <- data.frame(x = a)", "ev <- data.frame(x = 99)")
  expect_identical(slice_of(code, "ev", top = 3L), c("a <- 1", "ev <- data.frame(x = a)"))
  expect_identical(slice_of(code, "ev", top = 1L), character())
})

test_that("an empty argument and an empty program do not break the walk", {
  expect_identical(slice_of(c("m <- matrix(1:4, 2)", "x <- m[1, ]"), "x"), c("m <- matrix(1:4, 2)", "x <- m[1, ]"))
  expect_identical(.lintSlice(list(), 1L, quote(ev)), list())
  expect_identical(.lintDisallowedCalls(list(quote(x[1, ]))), character())
  expect_identical(.lintDisallowedCalls(list(quote(f <- function(a, b) a))), character())
})

test_that("the names a statement binds follow R's assignment forms", {
  expect_identical(.lintBinds(quote(x <- 1)), "x")
  expect_identical(.lintBinds(quote(x$a$b <- 1)), "x")
  expect_identical(.lintBinds(quote(names(x)[2] <- "b")), "x")
  expect_identical(.lintBinds(quote(1 -> x)), "x")
  expect_identical(.lintBinds(quote(for (i in 1:3) y <- i)), c("i", "y"))
  expect_identical(.lintBinds(quote(f <- function(a) { b <- a; b })), "f")
  expect_identical(.lintBinds(quote(local({ z <- 1 }))), character())
  expect_identical(.lintBinds(quote(print(x))), character())
})

test_that("a constant used as a function's default argument is found by the slice", {
  expect_identical(.lintNames(quote(f(a, b = c))), c("f", "a", "c"))
  expect_identical(.lintNames(quote(function(n = n_arm, m) n + m)), c("function", "n_arm", "+", "n", "m"))
  expect_identical(.lintNames(quote(x[1, ])), c("[", "x"))
  code <- c("n_arm <- 3", "unused <- 1", "mk <- function(n = n_arm) data.frame(id = seq_len(n))", "ev <- mk()")
  expect_identical(slice_of(code, "ev"), c("n_arm <- 3", "mk <- function(n = n_arm) data.frame(id = seq_len(n))", "ev <- mk()"))
  res <- sandbox(code[-2L][-3L], "mk()")
  expect_identical(nrow(res$value), 3L)
})

# The sandbox has two layers: the check of the slice's calls before it runs, and
# the environment it runs in. The second must hold on its own, so these evaluate
# in the environment directly.
test_that("the sandbox environment refuses what the pre-check would, without the pre-check", {
  skip_if_not_installed("dplyr")
  trace <- withr::local_tempfile(fileext = ".trace")
  env <- new.env(parent = .lintSandboxEnv())
  run <- function(code) tryCatch(eval(parse1(code), env), error = function(e) conditionMessage(e))
  expect_match(run(sprintf("base::file.create('%s')", trace)), "`base::file.create` is not allowed", fixed = TRUE)
  expect_match(run(sprintf("base:::file.create('%s')", trace)), "could not find function", fixed = TRUE)
  expect_match(run("nlmixr2lib::readModelDb('x')"), "is not allowed", fixed = TRUE)
  expect_match(run(sprintf("file.create('%s')", trace)), "could not find function", fixed = TRUE)
  expect_match(run(sprintf("do.call('file.create', list('%s'))", trace)), "file.create", fixed = TRUE)
  expect_match(run(sprintf("lapply('%s', 'file.create')", trace)), "file.create", fixed = TRUE)
  expect_match(run(sprintf("sapply('%s', match.fun('file.create'))", trace)), "could not find function", fixed = TRUE)
  expect_match(run("eval(quote(1))"), "could not find function", fixed = TRUE)
  expect_false(file.exists(trace))
  # what is allowed works the same way
  expect_identical(run("dplyr::mutate"), getExportedValue("dplyr", "mutate"))
  expect_identical(run("stats::rnorm"), stats::rnorm)
  expect_identical(run("1 + 1"), 2)
})

test_that("the CLI script parses and names the pairing flag", {
  script <- system.file("scripts", "lint_vignette.R", package = "nlmixr2libingest")
  skip_if(!nzchar(script), "the scripts directory is not installed")
  expect_no_error(parse(file = script))
  expect_match(paste(readLines(script), collapse = "\n"), "--no-pairing", fixed = TRUE)
})
