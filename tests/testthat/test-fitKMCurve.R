# Regression test: fitKMCurve() must not return NA estimates when the
# requested time grid extends past the last observed event in a stratum.
# Previously the function called summary(..., extend = FALSE) and merged with
# NAs, which then propagated through cov() in the bootstrap and broke pated().

test_that("fitKMCurve() carries S(t) forward past last event (no NAs)", {
  skip_if_not_installed("survival")
  library(survival)

  set.seed(99)
  n <- 60
  dat <- data.frame(
    pid  = paste0("p", seq_len(n)),
    arm  = c(rep(0, n/2), rep(1, n/2))
  )
  # arm=0 has all events by t=3; arm=1 has events spread out to t=10. Asking for
  # times past 3 forces the "extend" branch in arm=0.
  dat$time  <- ifelse(dat$arm == 0,
                      runif(n, 0.5, 3),
                      runif(n, 0.5, 10))
  dat$event <- ifelse(dat$arm == 0, 1, rbinom(n, 1, 0.6))

  trans_St <- multipleOutcomes:::fitKMCurve(
    Surv(time, event) ~ arm,
    data      = dat,
    conf_type = "log",
    times     = c(1, 2, 3, 4, 6, 9)
  )

  expect_false(any(is.na(trans_St)))
  # Both strata must have exactly the same set of evaluation times.
  expect_equal(length(trans_St), 2 * 6)
})
