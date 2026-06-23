make_request <- function(prompt, options, summary = "", id = "q1") {
  list(summary = summary,
       questions = list(list(id = id, prompt = prompt, options = options)))
}
opt <- function(value, label) list(value = value, label = label)

test_that("a review/methodology paper is auto-skipped (single high-conf policy)", {
  skip_if_not_installed("yaml")
  req <- make_request(
    prompt = paste("This appears to be a systematic review / meta-analysis,",
                   "not an original popPK model."),
    options = list(
      opt("A", "Extract the model anyway"),
      opt("B", paste("Skip this task and queue cited primary popPK papers",
                     "if they contain real models"))))
  dec <- sidecar_match(req)
  expect_s3_class(dec, "nli_sidecar_decision")
  expect_true(dec$auto)
  expect_equal(dec$questions[[1L]]$value, "B")
  expect_equal(dec$questions[[1L]]$policy_id, "review_or_methodology_paper_skip")
  expect_equal(dec$questions[[1L]]$confidence, "high")
})

test_that("an already-merged task verifies and exits", {
  skip_if_not_installed("yaml")
  req <- make_request(
    prompt = "This task's deliverable is already merged into origin/main.",
    options = list(
      opt("A", "Verify the merged content and exit cleanly without re-extracting"),
      opt("B", "Re-extract the model from scratch")))
  dec <- sidecar_match(req)
  expect_true(dec$auto)
  expect_equal(dec$questions[[1L]]$value, "A")
  expect_equal(dec$questions[[1L]]$policy_id, "already_merged_verify_and_exit")
})

test_that("scientific-judgement questions escalate (never auto-answered)", {
  skip_if_not_installed("yaml")
  # unit convention -- in escalate_types, not a policy
  unit <- make_request(
    prompt = "Albumin reported in g/L but the register expects g/dL. How to encode?",
    options = list(opt("A", "Convert to g/dL"), opt("B", "Skip the covariate")))
  expect_false(sidecar_match(unit)$auto)
  # missing PDF -- largest type, but needs a human
  pdf <- make_request(
    prompt = "Lead PDF not on disk; open-access acquisition ladder exhausted.",
    options = list(opt("A", "Operator acquires the PDF"), opt("B", "Skip")))
  d <- sidecar_match(pdf)
  expect_false(d$auto)
  expect_equal(d$questions[[1L]]$decision, "escalate")
})

test_that("the responder only ever picks an offered option", {
  skip_if_not_installed("yaml")
  # review prompt but the skip option is NOT offered -> cannot resolve -> escalate
  req <- make_request(
    prompt = "This is a systematic review / meta-analysis, not an original model.",
    options = list(opt("A", "Extract anyway"), opt("B", "Something unrelated")))
  expect_false(sidecar_match(req)$auto)
})

test_that("sidecar_respond writes a response + audit log only when applied", {
  skip_if_not_installed("yaml")
  req <- make_request(
    prompt = "Systematic review / meta-analysis, not an original popPK model.",
    options = list(
      opt("A", "Extract the model anyway"),
      opt("keep", paste("Skip this task and queue cited primary popPK papers",
                        "if they contain real models"))))
  resp <- tempfile(fileext = ".json")
  log  <- tempfile(fileext = ".log")
  dec <- sidecar_respond(req, response_file = resp, apply = TRUE, log_file = log)
  expect_true(dec$applied)
  expect_true(file.exists(resp))
  back <- jsonlite::fromJSON(resp, simplifyVector = FALSE)
  expect_equal(back$answers[[1L]]$value, "keep")
  expect_match(back$notes, "review_or_methodology_paper_skip")
  expect_true(file.exists(log) && length(readLines(log)) == 1L)

  # dry run writes nothing
  resp2 <- tempfile(fileext = ".json")
  dec2 <- sidecar_respond(req, response_file = resp2, apply = FALSE)
  expect_false(dec2$applied)
  expect_false(file.exists(resp2))
})
