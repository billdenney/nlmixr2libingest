test_that("distill_paper returns NULL with no LLM backend (degrades gracefully)", {
  # backend = "none" short-circuits before any network call
  expect_null(distill_paper("A population PK paper with a two-compartment model.",
                            backend = "none"))
})
