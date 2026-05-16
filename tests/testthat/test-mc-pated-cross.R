# Monte Carlo validation of pated() for heterogeneous primary + prognostic
# pairings (cox × glm and binomial-glm × glm). The existing
# test-mc-covariance.R already covers the same-engine gaussian case; here we
# specifically exercise the cross-engine cov21 block that pated() uses for
# its prognostic adjustment, then check that the resulting SE is calibrated
# and the 95% CI coverage is nominal.
#
# Helpers (mc_enabled, mc_run, expect_cov_close) come from helper-mc.R.

# Generic engine-agnostic calibration assertion for a pated() result. Pass
# the K-vector of point estimates, the K-vector of reported SEs, plus the
# *true* null value (typically 0 for the simulators we use here).
expect_pated_calibrated <- function(est, se, true = 0,
                                    bias_tol      = 0.05,
                                    se_rel_tol    = 0.15,
                                    cov_lo        = 0.92,
                                    cov_hi        = 0.98,
                                    label         = "") {
  expect_lt(abs(mean(est) - true), bias_tol,
            label = paste("mean estimate", label))
  expect_lt(abs(mean(se) - sd(est)) / sd(est), se_rel_tol,
            label = paste("SE calibration", label))
  cover <- mean(abs((est - true) / se) < 1.96)
  expect_gt(cover, cov_lo, label = paste("CI coverage lower", label))
  expect_lt(cover, cov_hi, label = paste("CI coverage upper", label))
}

# ---------------------------------------------------------------------------
test_that("MC: pated(coxph_, glm_) is unbiased + calibrated under H0", {
  skip_on_cran()
  skip_if_not(mc_enabled(),
              "set MULTIPLEOUTCOMES_RUN_MC=1 to run Monte Carlo tests")
  skip_if_not_installed("survival")
  library(survival)
  K <- 400; n <- 400

  est_p <- numeric(K); se_p <- numeric(K)
  est_s <- numeric(K); se_s <- numeric(K)

  for (k in seq_len(K)) {
    dat <- sim_coxph(n = n, beta_arm = 0, beta_z = -0.5,
                     seed = 6000L + k)
    pa <- pated(
      coxph_(Surv(time, event) ~ arm,                       data_index = 1),
      glm_(z ~ arm, family = "gaussian",                    data_index = 1),
      data = list(dat)
    )
    p_row <- pa[pa$method == "PATED",    ]
    s_row <- pa[pa$method == "Standard", ]
    est_p[k] <- p_row$estimate; se_p[k] <- p_row$stderr
    est_s[k] <- s_row$estimate; se_s[k] <- s_row$stderr
  }

  expect_pated_calibrated(est_p, se_p, true = 0, label = "PATED cox×glm")
  expect_pated_calibrated(est_s, se_s, true = 0, label = "Standard cox")
  # Prognostic z should tighten the SE.
  expect_lt(mean(se_p), mean(se_s))
})

# ---------------------------------------------------------------------------
test_that("MC: pated(glm_binomial, glm_) is unbiased + calibrated under H0", {
  skip_on_cran()
  skip_if_not(mc_enabled(),
              "set MULTIPLEOUTCOMES_RUN_MC=1 to run Monte Carlo tests")
  K <- 400; n <- 500

  est_p <- numeric(K); se_p <- numeric(K)
  est_s <- numeric(K); se_s <- numeric(K)

  for (k in seq_len(K)) {
    dat <- sim_binomial(n = n, beta_arm = 0, beta_z = 0.8,
                        seed = 7000L + k)
    pa <- pated(
      glm_(y ~ arm, family = "binomial", data_index = 1),
      glm_(z ~ arm, family = "gaussian", data_index = 1),
      data = list(dat)
    )
    p_row <- pa[pa$method == "PATED",    ]
    s_row <- pa[pa$method == "Standard", ]
    est_p[k] <- p_row$estimate; se_p[k] <- p_row$stderr
    est_s[k] <- s_row$estimate; se_s[k] <- s_row$stderr
  }

  expect_pated_calibrated(est_p, se_p, true = 0, label = "PATED bin×glm")
  expect_pated_calibrated(est_s, se_s, true = 0, label = "Standard binomial")
  expect_lt(mean(se_p), mean(se_s))
})
