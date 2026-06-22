test_that("source_trace flags ini values with no supporting number in the paper", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  # paper reports ka (1.57), CL (2.72), prop error (50%) but NOT vc (~31.5)
  paper <- paste("A one-compartment population PK model. First-order absorption,",
                 "ka 1.57 /h. Apparent clearance CL/F 2.72 L/h. Proportional",
                 "residual error 50%.")
  st <- source_trace("PK_1cmt", paper, tol = 0.05)
  expect_s3_class(st, "nli_sourcetrace")
  unverified <- st$ini$param[!st$ini$found]
  expect_true("lvc" %in% unverified)        # value absent -> flagged
  expect_false("lcl" %in% unverified)       # exp(1)=2.72 present
  expect_false("propSd" %in% unverified)    # 0.5 -> 50% present
  expect_equal(st$summary$ini_unverified, 1L)
})

test_that("source_trace flags structural ODE equations for verification", {
  skip_if_not_installed("nlmixr2lib")
  skip_if_not_installed("rxode2")
  st <- source_trace("PK_2cmt_des", "two-compartment model", tol = 0.05)
  expect_true(any(st$equations$type == "ode"))
  expect_true(all(st$equations$needs_verify[st$equations$type == "ode"]))
  # a pure linCmt + reparam model has no structural equations to verify
  st1 <- source_trace("PK_1cmt", "x", tol = 0.05)
  expect_equal(st1$summary$equations_to_verify, 0L)
})
