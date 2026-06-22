test_that("complexity_tier scales with model structure", {
  expect_equal(complexity_tier(n_ode = 0, n_cov = 0, n_endpoint = 1,
                               pd_type = "none", elimination_type = "linear"), "low")
  expect_equal(complexity_tier(n_ode = 4, n_cov = 3), "medium")
  expect_equal(complexity_tier(n_ode = 40, atypical = TRUE), "high")
  expect_equal(complexity_tier(n_cov = 8, n_endpoint = 2, pd_type = "indirect_response",
                               elimination_type = "tmdd"), "high")
})

test_that("budget_advisor escalates gracefully (never a hard stop)", {
  expect_gt(tier_budget("high"), tier_budget("low"))
  ok <- budget_advisor(10000, tier = "low")
  expect_equal(ok$status, "ok"); expect_equal(ok$action, "continue")
  near <- budget_advisor(35000, tier = "low")          # 35k/40k = 0.875
  expect_equal(near$status, "approaching"); expect_equal(near$action, "checkpoint")
  over <- budget_advisor(50000, tier = "low")          # 50k/40k = 1.25
  expect_equal(over$status, "over")
  expect_equal(over$action, "checkpoint_and_escalate")
  expect_equal(over$remaining, 0)
  expect_equal(budget_advisor(50, budget = 100)$frac, 0.5)
})

test_that("run_token_stats parses runner state YAMLs into per-run usage", {
  skip_if_not_installed("yaml")
  dir <- tempfile("state-"); dir.create(dir)
  yaml::write_yaml(list(id = "t1", runs = list(
    list(usage = list(output_tokens = 1000, input_tokens = 20,
                      cache_read_input_tokens = 500000), cost_usd = 1.5),
    list(usage = list(output_tokens = 2000, cache_read_input_tokens = 800000),
         cost_usd = 2.5))), file.path(dir, "t1.yaml"))
  df <- run_token_stats(dir)
  expect_equal(nrow(df), 2L)
  expect_equal(sum(df$output), 3000)
  expect_equal(sum(df$cost), 4.0)
  expect_false(is.null(attr(df, "summary")))
})
