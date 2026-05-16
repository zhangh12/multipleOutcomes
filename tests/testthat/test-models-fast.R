# Fast smoke tests for every supported model engine going through both
# jointCovariance() and pated(). These run in well under a second each.

# ---------------------------------------------------------------------------
# Shared assertion: a fit returned by jointCovariance() is well-formed.
expect_well_formed_jc <- function(fit, expected_models) {
  expect_s3_class(fit, "jointCovariance")
  expect_true(all(is.finite(coef(fit))))
  V <- vcov(fit)
  expect_true(is.matrix(V))
  expect_equal(dim(V), c(length(coef(fit)), length(coef(fit))))
  expect_lt(max(abs(V - t(V))), 1e-8)  # symmetric
  expect_gte(min(eigen(V, symmetric = TRUE, only.values = TRUE)$values), -1e-8)
  expect_equal(length(fit$id_map), expected_models)
}

# A pated() fit has 1 PATED row, 1 Standard row, and >=1 Prognostic row.
expect_well_formed_pated <- function(fit) {
  expect_s3_class(fit, "pated")
  expect_true(all(c("PATED", "Standard") %in% fit$method))
  expect_true(any(grepl("^Prognostic", fit$method)))
  expect_true(all(is.finite(fit$estimate)))
  expect_true(all(is.finite(fit$stderr)))
  expect_true(all(is.finite(fit$pvalue)))
}

# ---------------------------------------------------------------------------
test_that("glm_ gaussian works through jointCovariance + pated", {
  dat <- sim_gaussian(n = 200, seed = 1)
  jc <- jointCovariance(
    glm_(y ~ arm, family = "gaussian", data_index = 1),
    glm_(z ~ arm, family = "gaussian", data_index = 1),
    data = list(dat)
  )
  expect_well_formed_jc(jc, expected_models = 2)

  pa <- pated(
    glm_(y ~ arm, family = "gaussian", data_index = 1),
    glm_(z ~ arm, family = "gaussian", data_index = 1),
    data = list(dat)
  )
  expect_well_formed_pated(pa)
})

# ---------------------------------------------------------------------------
test_that("glm_ binomial works through jointCovariance + pated", {
  dat <- sim_binomial(n = 400, seed = 2)
  jc <- jointCovariance(
    glm_(y ~ arm, family = "binomial", data_index = 1),
    glm_(z ~ arm, family = "gaussian", data_index = 1),
    data = list(dat)
  )
  expect_well_formed_jc(jc, expected_models = 2)

  pa <- pated(
    glm_(y ~ arm, family = "binomial", data_index = 1),
    glm_(z ~ arm, family = "gaussian", data_index = 1),
    data = list(dat)
  )
  expect_well_formed_pated(pa)
})

# ---------------------------------------------------------------------------
test_that("coxph_ works through jointCovariance + pated", {
  skip_if_not_installed("survival")
  library(survival)
  dat <- sim_coxph(n = 400, seed = 3)
  jc <- jointCovariance(
    coxph_(Surv(time, event) ~ arm, data_index = 1),
    glm_(z ~ arm, family = "gaussian", data_index = 1),
    data = list(dat)
  )
  expect_well_formed_jc(jc, expected_models = 2)

  pa <- pated(
    coxph_(Surv(time, event) ~ arm, data_index = 1),
    glm_(z ~ arm, family = "gaussian", data_index = 1),
    data = list(dat)
  )
  expect_well_formed_pated(pa)
})

# ---------------------------------------------------------------------------
test_that("logrank_ works through jointCovariance + pated", {
  skip_if_not_installed("survival")
  library(survival)
  dat <- sim_coxph(n = 400, seed = 4)
  jc <- jointCovariance(
    logrank_(Surv(time, event) ~ arm, data_index = 1),
    glm_(z ~ arm, family = "gaussian", data_index = 1),
    data = list(dat)
  )
  expect_well_formed_jc(jc, expected_models = 2)

  pa <- pated(
    logrank_(Surv(time, event) ~ arm, data_index = 1),
    glm_(z ~ arm, family = "gaussian", data_index = 1),
    data = list(dat)
  )
  expect_well_formed_pated(pa)
})

# ---------------------------------------------------------------------------
test_that("gee_ works through jointCovariance + pated", {
  long <- sim_repeated(n_subj = 120, n_visits = 3, seed = 5)
  # Aux glm_ has one row per subject; reduce to a single observation per pid.
  aux <- aggregate(y ~ pid + arm, data = long, FUN = mean)

  jc <- jointCovariance(
    gee_(y ~ arm, family = "gaussian", corstr = "exchangeable", data_index = 1),
    glm_(y ~ arm, family = "gaussian", data_index = 2),
    data = list(long, aux)
  )
  expect_well_formed_jc(jc, expected_models = 2)

  pa <- pated(
    gee_(y ~ arm, family = "gaussian", corstr = "exchangeable", data_index = 1),
    glm_(y ~ arm, family = "gaussian", data_index = 2),
    data = list(long, aux)
  )
  expect_well_formed_pated(pa)
})

# ---------------------------------------------------------------------------
test_that("quantile_ works through jointCovariance + pated (bootstrap)", {
  dat <- sim_gaussian(n = 200, seed = 7)
  jc <- jointCovariance(
    quantile_(y ~ arm, probs = c(.25, .5, .75), data_index = 1),
    glm_(z ~ arm, family = "gaussian", data_index = 1),
    data = list(dat),
    nboot = 30,
    seed  = 11
  )
  expect_s3_class(jc, "jointCovariance")
  # quantile_ contributes 3 parameters (one per probability)
  expect_equal(length(jc$id_map[[1]]), 3L)
  expect_true(all(grepl("^arm_\\d+%$", names(jc$id_map[[1]]))))
  expect_true(all(is.finite(coef(jc))))
  V <- vcov(jc); expect_true(is.matrix(V)); expect_lt(max(abs(V - t(V))), 1e-8)

  pa <- pated(
    quantile_(y ~ arm, probs = c(.25, .5, .75), data_index = 1),
    glm_(z ~ arm, family = "gaussian", data_index = 1),
    data  = list(dat),
    nboot = 30,
    seed  = 11
  )
  expect_well_formed_pated(pa)
})

test_that("quantile_ errors with nboot = 0 (no closed-form score)", {
  dat <- sim_gaussian(n = 100, seed = 8)
  expect_error(
    jointCovariance(
      quantile_(y ~ arm, probs = c(.25, .5, .75), data_index = 1),
      data = list(dat)
    ),
    regexp = "no closed-form score"
  )
})

# ---------------------------------------------------------------------------
test_that("mmrm_ works through jointCovariance + pated", {
  skip_if_not_installed("mmrm")
  long <- sim_repeated(n_subj = 120, n_visits = 3, seed = 6)
  aux  <- aggregate(y ~ pid + arm, data = long, FUN = mean)

  jc <- jointCovariance(
    mmrm_(y ~ arm + us(visit | pid), reml = TRUE, data_index = 1),
    glm_(y ~ arm, family = "gaussian", data_index = 2),
    data = list(long, aux)
  )
  expect_well_formed_jc(jc, expected_models = 2)

  pa <- pated(
    mmrm_(y ~ arm + us(visit | pid), reml = TRUE, data_index = 1),
    glm_(y ~ arm, family = "gaussian", data_index = 2),
    data = list(long, aux)
  )
  expect_well_formed_pated(pa)
})
