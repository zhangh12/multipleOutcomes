# Monte Carlo validation: for several model types, simulate K independent
# datasets, fit jointCovariance() on each, and check that the empirical
# covariance of the coefficient vector across replicates agrees with the
# theoretical covariance that jointCovariance() reports.
#
# These tests are slow (hundreds of fits each) and statistical (no exact
# answer). They run under devtools::test() / R CMD check --as-cran with the
# right flags, but are skipped on CRAN and in the default test run.

# mc_enabled(), mc_run(), and expect_cov_close() live in helper-mc.R.

# ---------------------------------------------------------------------------
test_that("MC: empirical cov matches theoretical for glm_(gaussian) joint", {
  skip_on_cran()
  skip_if_not(mc_enabled(),
              "set MULTIPLEOUTCOMES_RUN_MC=1 to run Monte Carlo tests")
  K <- 400; n <- 250

  res <- mc_run(
    K,
    simulate = function(k) sim_gaussian(n = n, seed = 1000L + k),
    fit_fn   = function(dat) jointCovariance(
      glm_(y ~ arm + z, family = "gaussian", data_index = 1),
      data = list(dat)
    )
  )
  expect_cov_close(cov(res$beta_hat), res$vcov_mean,
                   rel_diag = 0.20, abs_offdiag = 0.02,
                   label = "glm gaussian joint")
})

# ---------------------------------------------------------------------------
test_that("MC: glm_(gaussian) + glm_(gaussian) joint matrix is calibrated", {
  skip_on_cran()
  skip_if_not(mc_enabled(),
              "set MULTIPLEOUTCOMES_RUN_MC=1 to run Monte Carlo tests")
  K <- 400; n <- 250

  res <- mc_run(
    K,
    simulate = function(k) sim_gaussian(n = n, seed = 2000L + k),
    fit_fn   = function(dat) jointCovariance(
      glm_(y ~ arm, family = "gaussian", data_index = 1),
      glm_(z ~ arm, family = "gaussian", data_index = 1),
      data = list(dat)
    )
  )
  expect_cov_close(cov(res$beta_hat), res$vcov_mean,
                   rel_diag = 0.25, abs_offdiag = 0.03,
                   label = "two-glm joint")
})

# ---------------------------------------------------------------------------
test_that("MC: empirical cov matches theoretical for glm_(binomial)", {
  skip_on_cran()
  skip_if_not(mc_enabled(),
              "set MULTIPLEOUTCOMES_RUN_MC=1 to run Monte Carlo tests")
  K <- 400; n <- 500

  res <- mc_run(
    K,
    simulate = function(k) sim_binomial(n = n, seed = 3000L + k),
    fit_fn   = function(dat) jointCovariance(
      glm_(y ~ arm + z, family = "binomial", data_index = 1),
      data = list(dat)
    )
  )
  expect_cov_close(cov(res$beta_hat), res$vcov_mean,
                   rel_diag = 0.25, abs_offdiag = 0.03,
                   label = "glm binomial")
})

# ---------------------------------------------------------------------------
test_that("MC: empirical cov matches theoretical for coxph_", {
  skip_on_cran()
  skip_if_not(mc_enabled(),
              "set MULTIPLEOUTCOMES_RUN_MC=1 to run Monte Carlo tests")
  skip_if_not_installed("survival")
  library(survival)
  K <- 400; n <- 500

  res <- mc_run(
    K,
    simulate = function(k) sim_coxph(n = n, seed = 4000L + k),
    fit_fn   = function(dat) jointCovariance(
      coxph_(Surv(time, event) ~ arm + z, data_index = 1),
      data = list(dat)
    )
  )
  expect_cov_close(cov(res$beta_hat), res$vcov_mean,
                   rel_diag = 0.25, abs_offdiag = 0.03,
                   label = "coxph")
})

# ---------------------------------------------------------------------------
test_that("MC: pated() variance reduction is honest (calibrated SE & coverage)", {
  skip_on_cran()
  skip_if_not(mc_enabled(),
              "set MULTIPLEOUTCOMES_RUN_MC=1 to run Monte Carlo tests")
  K <- 400; n <- 250

  est_pated <- numeric(K); se_pated <- numeric(K)
  est_std   <- numeric(K); se_std   <- numeric(K)

  for (k in seq_len(K)) {
    # arm coefficient is 0 by construction; z is the only prognostic signal.
    dat <- sim_gaussian(n = n, beta_arm = 0, beta_z = 0.7,
                       seed = 5000L + k)
    pa <- pated(
      glm_(y ~ arm, family = "gaussian", data_index = 1),
      glm_(z ~ arm, family = "gaussian", data_index = 1),
      data = list(dat)
    )
    p_row <- pa[pa$method == "PATED",    ]
    s_row <- pa[pa$method == "Standard", ]
    est_pated[k] <- p_row$estimate; se_pated[k] <- p_row$stderr
    est_std[k]   <- s_row$estimate; se_std[k]   <- s_row$stderr
  }

  # Both estimators are unbiased for the null effect.
  expect_lt(abs(mean(est_pated)), 0.05,
            label = "PATED mean estimate near 0")
  expect_lt(abs(mean(est_std)),   0.05,
            label = "Standard mean estimate near 0")

  # SE reported by pated() ~ empirical SD of the estimates.
  expect_lt(abs(mean(se_pated) - sd(est_pated)) / sd(est_pated),
            0.15, label = "PATED SE calibration")
  expect_lt(abs(mean(se_std)   - sd(est_std))   / sd(est_std),
            0.15, label = "Standard SE calibration")

  # 95% CI coverage close to nominal (within MC tolerance ~ 2*sqrt(.95*.05/K)).
  cov_p <- mean(abs(est_pated / se_pated) < 1.96)
  cov_s <- mean(abs(est_std   / se_std)   < 1.96)
  expect_gt(cov_p, 0.92); expect_lt(cov_p, 0.98)
  expect_gt(cov_s, 0.92); expect_lt(cov_s, 0.98)

  # The whole point of PATED: SE smaller than unadjusted when z is prognostic.
  expect_lt(mean(se_pated), mean(se_std))
})
