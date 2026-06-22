#' nlmixr2libingest: token-efficient ingestion tooling for nlmixr2lib
#'
#' Corpus-scale tooling that makes extracting published popPK/PD models into
#' \pkg{nlmixr2lib} cheaper while preserving a strict source-trace quality
#' invariant. See \code{inst/design/ARCHITECTURE.md}.
#'
#' @keywords internal
"_PACKAGE"

# Canonical register file names shipped by nlmixr2lib, mapped to the `kind`
# tag the parser and lookup use.
.registerFiles <- c(
  covariate = "covariate-columns.md",
  compartment = "compartment-names.md",
  parameter = "parameter-names.md"
)

#' Locate the nlmixr2lib canonical-name register directory
#'
#' Resolves, in order of preference: the `nlmixr2libingest.refs_dir` option,
#' the installed `nlmixr2lib` package (`system.file("references", ...)`), then a
#' development checkout under the parent of this package. The directory is the
#' authority that [lookup_canonical()] indexes; it is owned by `nlmixr2lib`,
#' never by this package.
#'
#' @return Absolute path to the references directory, or `""` if none found.
#' @export
registers_dir <- function() {
  opt <- getOption("nlmixr2libingest.refs_dir", NULL)
  if (!is.null(opt) && nzchar(opt) && dir.exists(opt)) {
    return(normalizePath(opt, mustWork = FALSE))
  }
  p <- tryCatch(system.file("references", package = "nlmixr2lib"), error = function(e) "")
  if (nzchar(p) && dir.exists(p)) {
    return(normalizePath(p, mustWork = FALSE))
  }
  # development fallback: sibling source checkout of nlmixr2lib
  guess <- file.path(dirname(getwd()), "nlmixr2lib", "inst", "references")
  if (dir.exists(guess)) {
    return(normalizePath(guess, mustWork = FALSE))
  }
  ""
}

# Full paths to the three register files, in a named vector keyed by `kind`.
# Stops if the directory or any expected file is missing.
.registerPaths <- function(dir = registers_dir()) {
  if (!nzchar(dir) || !dir.exists(dir)) {
    cli::cli_abort(c(
      "Could not locate the nlmixr2lib canonical-name registers.",
      i = "Install {.pkg nlmixr2lib} or set {.code options(nlmixr2libingest.refs_dir=)}."
    ))
  }
  paths <- file.path(dir, .registerFiles)
  names(paths) <- names(.registerFiles)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    cli::cli_abort("Missing register file{?s}: {.file {missing}}.")
  }
  paths
}

#' Default on-disk location of the cached register index (a DuckDB file)
#' @return Absolute path; the parent directory is created on demand.
#' @export
register_db_path <- function() {
  opt <- getOption("nlmixr2libingest.db_path", NULL)
  if (!is.null(opt) && nzchar(opt)) return(opt)
  dir <- tools::R_user_dir("nlmixr2libingest", which = "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "registers.duckdb")
}

# Cheap source-signature of the register files: size + mtime + md5 per file.
# md5 is the authority; size/mtime are the fast pre-check.
.registerSignature <- function(paths = .registerPaths()) {
  info <- file.info(paths)
  data.frame(
    kind = names(paths),
    path = unname(paths),
    size = info$size,
    mtime = as.numeric(info$mtime),
    md5 = unname(tools::md5sum(paths)),
    stringsAsFactors = FALSE
  )
}
