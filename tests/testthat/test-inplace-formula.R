# Verify that pated() / jointCovariance() handle in-place R formula
# transformations identically to pre-computed columns.
#
# For each scenario we build two versions of the same data: one with a
# derived column added by hand, one without. Then we call pated() with
# matching formulas — one referencing the derived column, the other
# inlining the expression — and require every estimate, standard error,
# and p-value to agree exactly.
#
# Uses the built-in `actg` dataset so the tests are self-contained.

library(survival)

build_data <- function() {
  data(actg, package = "multipleOutcomes", envir = environment())
  d <- actg[, c("id", "time", "censor", "tx", "cd4", "age", "sex")]
  d <- d[complete.cases(d), ]
  d$pid  <- d$id
  d$cnsr <- 1L - d$censor   # so we can also test `Surv(time, 1 - cnsr)`
  d
}

# pated sorts prognostic rows by |corr|, which may change when an
# expression is inlined vs pre-computed only because of label-order ties.
# Compare numerically after sorting by estimate so the row alignment is
# robust to the display order.
expect_pated_identical <- function(a, b, tol = 0) {
  expect_equal(nrow(a), nrow(b))
  ord_a <- order(round(a$estimate, 12))
  ord_b <- order(round(b$estimate, 12))
  expect_equal(a$estimate[ord_a], b$estimate[ord_b], tolerance = tol)
  expect_equal(a$stderr[ord_a],   b$stderr[ord_b],   tolerance = tol)
  expect_equal(a$pvalue[ord_a],   b$pvalue[ord_b],   tolerance = tol)
}

# ---------------------------------------------------------------------------
test_that("baseline: 3 prognostic specs with mixed in-place transformations", {
  d <- build_data()

  d_pre <- d
  d_pre$age_hi <- as.integer(d_pre$age > 40)
  d_pre$cd4_log <- log(d_pre$cd4 + 1)
  d_pre$old_male <- as.integer(d_pre$age > 40 & d_pre$sex == 0)

  fit_pre <- pated(
    coxph_(Surv(time, censor) ~ tx),
    glm_(age_hi   ~ tx, family = "binomial"),
    glm_(cd4_log  ~ tx, family = "gaussian"),
    glm_(old_male ~ tx, family = "binomial"),
    data = d_pre)

  fit_inp <- pated(
    coxph_(Surv(time, 1 - cnsr) ~ tx),
    glm_(I(age > 40)               ~ tx, family = "binomial"),
    glm_(log(cd4 + 1)              ~ tx, family = "gaussian"),
    glm_(I(age > 40 & sex == 0)    ~ tx, family = "binomial"),
    data = d)

  expect_pated_identical(fit_pre, fit_inp)
})

# ---------------------------------------------------------------------------
test_that("arithmetic on the LHS of a glm spec", {
  d <- build_data()
  d_pre <- d
  d_pre$cd4_centered <- d_pre$cd4 - mean(d_pre$cd4)

  fit_pre <- pated(
    coxph_(Surv(time, censor) ~ tx),
    glm_(cd4_centered ~ tx, family = "gaussian"),
    data = d_pre)

  fit_inp <- pated(
    coxph_(Surv(time, censor) ~ tx),
    glm_(I(cd4 - mean(cd4)) ~ tx, family = "gaussian"),
    data = d)

  expect_pated_identical(fit_pre, fit_inp)
})

# ---------------------------------------------------------------------------
test_that("scale() on the RHS of the primary Cox spec", {
  d <- build_data()
  d_pre <- d
  d_pre$age_z <- scale(d_pre$age)[, 1]

  fit_pre <- pated(
    coxph_(Surv(time, censor) ~ tx + age_z),
    glm_(cd4 ~ tx, family = "gaussian"),
    data = d_pre)

  fit_inp <- pated(
    coxph_(Surv(time, censor) ~ tx + scale(age)),
    glm_(cd4 ~ tx, family = "gaussian"),
    data = d)

  expect_pated_identical(fit_pre, fit_inp)
})

# ---------------------------------------------------------------------------
test_that("Surv() with arithmetic in both time and event arguments", {
  d <- build_data()
  d_pre <- d
  d_pre$time_wk <- d_pre$time / 7

  fit_pre <- pated(
    coxph_(Surv(time_wk, censor) ~ tx),
    glm_(cd4 ~ tx, family = "gaussian"),
    data = d_pre)

  fit_inp <- pated(
    coxph_(Surv(time / 7, 1 - cnsr) ~ tx),
    glm_(cd4 ~ tx, family = "gaussian"),
    data = d)

  expect_pated_identical(fit_pre, fit_inp)
})

# ---------------------------------------------------------------------------
test_that("factor() on an integer covariate", {
  d <- build_data()
  d_pre <- d
  d_pre$sex_f <- factor(d_pre$sex)

  fit_pre <- pated(
    coxph_(Surv(time, censor) ~ tx + sex_f),
    glm_(cd4 ~ tx, family = "gaussian"),
    data = d_pre)

  fit_inp <- pated(
    coxph_(Surv(time, censor) ~ tx + factor(sex)),
    glm_(cd4 ~ tx, family = "gaussian"),
    data = d)

  expect_pated_identical(fit_pre, fit_inp)
})
