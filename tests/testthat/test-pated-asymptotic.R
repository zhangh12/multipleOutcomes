# Asymptotic (nboot = 0) path used to dispatch to parseTreatmentVariableFromFormula
# which silently produced an empty arm vector once jointCovariance() started
# requiring jc_spec specs. Make sure the call-based parser is now reached and
# the standard non-KM PATED pipeline works end-to-end.

test_that("pated() runs asymptotically with jc_spec inputs", {
  set.seed(11)
  n <- 200
  dat <- data.frame(
    pid  = paste0("p", seq_len(n)),
    trt  = rbinom(n, 1, 0.5),
    z1   = rnorm(n),
    z2   = rbinom(n, 1, 0.4)
  )
  dat$y <- 0.4 * dat$trt + 0.6 * dat$z1 + rnorm(n)

  fit <- pated(
    glm_(y  ~ trt, family = "gaussian", data_index = 1),
    glm_(z1 ~ trt, family = "gaussian", data_index = 1),
    glm_(z2 ~ trt, family = "binomial", data_index = 1),
    data = list(dat)
  )

  expect_s3_class(fit, "pated")
  expect_true("PATED" %in% fit$method)
  pated_row <- fit[fit$method == "PATED", ]
  expect_equal(nrow(pated_row), 1)
  expect_true(is.finite(pated_row$estimate))
  expect_true(is.finite(pated_row$stderr))
  expect_lt(pated_row$pvalue, 0.05)  # 0.4 effect on n=200 should be detectable
})
