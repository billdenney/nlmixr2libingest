# Build a minimal but format-faithful set of the three registers in a temp dir,
# so parser/lookup tests do not depend on nlmixr2lib being installed.

make_fixture_refs <- function(extra_cov = NULL) {
  dir <- file.path(tempfile("refs-"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  cov <- c(
    "# Canonical covariate columns",
    "## Demographics",
    "### WT (**canonical for body weight**)",
    "- **Description:** Body weight (baseline or time-varying).",
    "- **Units:** kg",
    "- **Type:** continuous",
    "- **Scope:** general",
    "- **Reference category:** n/a -- allometric scaling.",
    "- **Source aliases:**",
    "  - `WEIG` -- used in `Wang_2012_levetiracetam.R`",
    "- **Example models:** `Clegg_2024_nirsevimab.R`, `Hu_2026_clesrovimab.R`",
    "- **Notes:** Universal.",
    "",
    "### CRCL (**canonical for creatinine clearance**)",
    "- **Description:** Creatinine clearance (renal function).",
    "- **Units:** mL/min",
    "- **Type:** continuous",
    "- **Scope:** general",
    "- **Source aliases:** none.",
    "- **Example models:** `Li_2006_meropenem.R`",
    "- **Notes:** Cockcroft-Gault unless stated."
  )
  if (!is.null(extra_cov)) cov <- c(cov, "", extra_cov)

  cmt <- c(
    "# Canonical compartment names",
    "## Standard drug-disposition compartments",
    "### central (**canonical central compartment**)",
    "- **Type:** compartment",
    "- **Role:** Central compartment; the output state for `Cc = central / vc`.",
    "- **Source aliases:** none.",
    "- **Example models:** universal."
  )

  par <- c(
    "# Canonical parameter names",
    "## Log-transformed structural PK parameters",
    "### lcl (**canonical log-transformed total clearance**)",
    "- **Type:** log-transformed-pk",
    "- **Role:** Apparent total drug clearance from the central compartment.",
    "- **Source aliases:** none.",
    "- **Example models:** universal."
  )

  writeLines(cov, file.path(dir, "covariate-columns.md"))
  writeLines(cmt, file.path(dir, "compartment-names.md"))
  writeLines(par, file.path(dir, "parameter-names.md"))
  dir
}
