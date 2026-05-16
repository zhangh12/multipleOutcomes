# Monte Carlo validation of the *cross-engine* block of the joint covariance
# matrix. The single-engine and same-engine-pair tests live in
# test-mc-covariance.R; here we explicitly target the diagonal blocks of
# different engines AND the off-diagonal blocks between them.
#
# The kitchen-sink test uses sim_correlated_multi() to draw four endpoints
# (continuous, binary, survival, longitudinal) from a shared latent factor
# vector with exchangeable correlation, so the off-diagonal cross-engine
# blocks of the population covariance are non-zero by construction; if
# jointCovariance() silently set them to zero, the test would catch it via
# both the diagonal/off-diagonal closeness check AND the additional
# non-trivial-correlation sanity check at the end of each test.

# mc_enabled(), mc_run(), expect_cov_close() are in helper-mc.R.

# Sanity check: with rho > 0 in the generator the cross-model correlations
# must be detectable above MC noise.
expect_nontrivial_cross_cor <- function(emp_cov, label = "") {
  cor_mat <- cov2cor(emp_cov)
  offdiag <- cor_mat[upper.tri(cor_mat)]
  expect_gt(max(abs(offdiag)), 0.05,
            label = paste("non-trivial cross-model correlation present", label))
}

# ---------------------------------------------------------------------------
test_that("MC: gee + glm cross-engine covariance is calibrated", {
  skip_on_cran()
  skip_if_not(mc_enabled(),
              "set MULTIPLEOUTCOMES_RUN_MC=1 to run Monte Carlo tests")
  K <- 200; n <- 250

  res <- mc_run(
    K,
    simulate = function(k) sim_correlated_multi(n_subj = n, seed = 9000L + k),
    fit_fn   = function(dat) jointCovariance(
      gee_(y_long ~ arm, family = "gaussian",
           corstr = "exchangeable", data_index = 1),
      glm_(y_cont ~ arm, family = "gaussian", data_index = 2),
      data = list(dat$long, dat$wide)
    )
  )
  expect_cov_close(cov(res$beta_hat), res$vcov_mean,
                   rel_diag = 0.30, abs_offdiag = 0.04,
                   label = "gee x glm")
  expect_nontrivial_cross_cor(cov(res$beta_hat), label = "(gee x glm)")
})

# ---------------------------------------------------------------------------
test_that("MC: mmrm + glm cross-engine covariance is calibrated", {
  skip_on_cran()
  skip_if_not(mc_enabled(),
              "set MULTIPLEOUTCOMES_RUN_MC=1 to run Monte Carlo tests")
  skip_if_not_installed("mmrm")
  K <- 200; n <- 250

  res <- mc_run(
    K,
    simulate = function(k) sim_correlated_multi(n_subj = n, seed = 10000L + k),
    fit_fn   = function(dat) jointCovariance(
      mmrm_(y_long ~ arm + us(visit | pid), reml = TRUE, data_index = 1),
      glm_(y_cont ~ arm, family = "gaussian", data_index = 2),
      data = list(dat$long, dat$wide)
    )
  )
  expect_cov_close(cov(res$beta_hat), res$vcov_mean,
                   rel_diag = 0.30, abs_offdiag = 0.04,
                   label = "mmrm x glm")
  expect_nontrivial_cross_cor(cov(res$beta_hat), label = "(mmrm x glm)")
})

# ---------------------------------------------------------------------------
# Kitchen-sink: 4 engines, 4 endpoint types, correlated outcomes.
test_that("MC: kitchen-sink (glm-gaussian, glm-binomial, coxph, gee) is calibrated", {
  skip_on_cran()
  skip_if_not(mc_enabled(),
              "set MULTIPLEOUTCOMES_RUN_MC=1 to run Monte Carlo tests")
  skip_if_not_installed("survival")
  library(survival)
  K <- 200; n <- 250

  res <- mc_run(
    K,
    simulate = function(k) sim_correlated_multi(n_subj = n, seed = 11000L + k),
    fit_fn   = function(dat) jointCovariance(
      glm_(y_cont ~ arm,                family = "gaussian", data_index = 1),
      glm_(y_bin  ~ arm,                family = "binomial", data_index = 1),
      coxph_(Surv(time, event) ~ arm,                         data_index = 1),
      gee_(y_long ~ arm, family = "gaussian",
           corstr = "exchangeable",                            data_index = 2),
      data = list(dat$wide, dat$long)
    )
  )
  expect_cov_close(cov(res$beta_hat), res$vcov_mean,
                   rel_diag = 0.30, abs_offdiag = 0.05,
                   label = "kitchen-sink")
  expect_nontrivial_cross_cor(cov(res$beta_hat), label = "(kitchen-sink)")
})
