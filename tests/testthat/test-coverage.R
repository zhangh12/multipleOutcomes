# Targeted tests to lift coverage of accessor / printing / utility code paths
# that the engine-specific tests don't naturally exercise.

helper_simple_fit <- function(seed = 13) {
  set.seed(seed)
  n <- 200
  dat <- data.frame(
    pid = paste0("p", seq_len(n)),
    arm = rbinom(n, 1, 0.5),
    z   = rnorm(n)
  )
  dat$y <- 0.3 * dat$arm + 0.5 * dat$z + rnorm(n)
  jointCovariance(
    glm_(y ~ arm, family = "gaussian"),
    glm_(z ~ arm, family = "gaussian"),
    data = dat
  )
}

# ---------------------------------------------------------------------------
test_that("summary(jc) and print(summary(jc)) work", {
  fit <- helper_simple_fit()
  s   <- summary(fit)
  expect_s3_class(s, "summary.jointCovariance")
  expect_true(is.matrix(s$coefficients))
  expect_true(all(c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
                  %in% colnames(s$coefficients)))
  expect_output(print(s), "Estimate")
})

test_that("summary(jc, model_index = N) restricts to that model", {
  fit <- helper_simple_fit()
  s_all <- summary(fit)
  s_one <- summary(fit, model_index = 1)
  expect_lt(nrow(s_one$coefficients), nrow(s_all$coefficients))
  expect_equal(nrow(s_one$coefficients), length(fit$id_map[[1]]))
  expect_error(summary(fit, model_index = 99), "Invalid model index")
})

# ---------------------------------------------------------------------------
test_that("coef(jc, model_index) and vcov(jc, model_index) restrict properly", {
  fit <- helper_simple_fit()
  c1 <- coef(fit, 1)
  v1 <- vcov(fit, 1)
  expect_true(!is.null(names(c1)))
  expect_equal(dim(v1), c(length(c1), length(c1)))
  expect_equal(rownames(v1), names(c1))
  expect_error(coef(fit, 99), "Invalid model index")
  expect_error(vcov(fit, 99), "Invalid model index")
})

test_that("coef(jc) and vcov(jc) (no model_index) return the full vector / matrix", {
  fit <- helper_simple_fit()
  expect_equal(length(coef(fit)), 4L)        # 2 models x (Intercept, arm)
  expect_equal(dim(vcov(fit)),   c(4L, 4L))
})

# ---------------------------------------------------------------------------
test_that("simulateMoData() produces a list of two data frames", {
  d <- simulateMoData(n = 80, hr = 0.7, seed = 1)
  expect_type(d, "list")
  expect_true(all(c("dat1", "dat2") %in% names(d)))
  expect_s3_class(d$dat1, "data.frame")
  expect_s3_class(d$dat2, "data.frame")
  expect_equal(nrow(d$dat1), 80)
  expect_true("trt" %in% names(d$dat1))
})

# ---------------------------------------------------------------------------
test_that("plot.pated() draws without error", {
  set.seed(17)
  n <- 200
  dat <- data.frame(
    pid = paste0("p", seq_len(n)),
    arm = rbinom(n, 1, 0.5),
    z   = rnorm(n)
  )
  dat$y <- 0.3 * dat$arm + 0.5 * dat$z + rnorm(n)
  fit <- pated(
    glm_(y ~ arm, family = "gaussian"),
    glm_(z ~ arm, family = "gaussian"),
    data = dat
  )
  pdf(file = tempfile(fileext = ".pdf"))
  on.exit(dev.off())
  expect_invisible(plot(fit))
})

# ---------------------------------------------------------------------------
# Bootstrap path: exercises LogRankAdapter$refit() and MMRMAdapter$refit(),
# neither of which is hit by any existing test. Use small nboot for speed.
test_that("bootstrap-mode coxph_ refits cleanly", {
  skip_if_not_installed("survival")
  library(survival)
  set.seed(19)
  n <- 200
  dat <- data.frame(
    pid   = paste0("p", seq_len(n)),
    arm   = rbinom(n, 1, 0.5),
    z     = rnorm(n),
    time  = rexp(n, rate = 0.2),
    event = rbinom(n, 1, 0.8)
  )
  fit <- jointCovariance(
    coxph_(Surv(time, event) ~ arm),
    glm_(z ~ arm, family = "gaussian"),
    data  = dat,
    nboot = 10,
    seed  = 1
  )
  expect_s3_class(fit, "jointCovariance")
  expect_true(all(is.finite(coef(fit))))
})

test_that("make_adapter() rejects unsupported spec class", {
  bogus <- structure(list(engine = "unknown", data_index = 1, id_col = "pid"),
                     class = c("jc_spec_bogus", "jc_spec"))
  expect_error(
    multipleOutcomes:::make_adapter(bogus, data_list = list(data.frame(pid = "x"))),
    "Unsupported spec type"
  )
})

test_that("bootstrap-mode logrank_ refits cleanly", {
  skip_if_not_installed("survival")
  library(survival)
  set.seed(21)
  n <- 200
  dat <- data.frame(
    pid   = paste0("p", seq_len(n)),
    arm   = rbinom(n, 1, 0.5),
    z     = rnorm(n),
    time  = rexp(n, rate = 0.2),
    event = rbinom(n, 1, 0.8)
  )
  fit <- jointCovariance(
    logrank_(Surv(time, event) ~ arm),
    glm_(z ~ arm, family = "gaussian"),
    data  = dat,
    nboot = 10,
    seed  = 1
  )
  expect_s3_class(fit, "jointCovariance")
  expect_true(all(is.finite(coef(fit))))
})

test_that("bootstrap-mode mmrm_ refits cleanly", {
  skip_if_not_installed("mmrm")
  long <- sim_repeated(n_subj = 80, n_visits = 3, seed = 22)
  fit <- jointCovariance(
    mmrm_(y ~ arm + us(visit | pid), reml = TRUE),
    data  = long,
    nboot = 5,
    seed  = 1
  )
  expect_s3_class(fit, "jointCovariance")
  expect_true(all(is.finite(coef(fit))))
})

# ---------------------------------------------------------------------------
# MMRMAdapter early-validation paths in mmrm_().
test_that("mmrm_() rejects bad control argument", {
  skip_if_not_installed("mmrm")
  expect_error(
    mmrm_(y ~ arm + us(visit | pid), control = "not a control"),
    "control must be an object of class mmrm_control"
  )
})

test_that("mmrm_() rejects Kenward-Roger with reml = FALSE", {
  skip_if_not_installed("mmrm")
  ctrl <- mmrm::mmrm_control(method = "Kenward-Roger")
  expect_error(
    mmrm_(y ~ arm + us(visit | pid), reml = FALSE, control = ctrl),
    "Kenward-Roger only works for REML"
  )
})

# ---------------------------------------------------------------------------
# sampleWithReplacement(): the "repeated-measurements per pid" branch.
test_that("sampleWithReplacement handles repeated measurements per pid", {
  set.seed(31)
  wide <- data.frame(
    pid = paste0("p", seq_len(40)),
    arm = rbinom(40, 1, 0.5),
    y   = rnorm(40)
  )
  long <- data.frame(
    pid = rep(paste0("p", seq_len(40)), each = 3),
    arm = rep(rbinom(40, 1, 0.5), each = 3),
    visit = factor(rep(1:3, 40)),
    y     = rnorm(120)
  )
  # Path used by bootstrapJointCovariance() with mixed wide+long input.
  fit <- jointCovariance(
    glm_(y ~ arm, family = "gaussian", data_index = 1),
    mmrm_(y ~ arm + us(visit | pid), reml = TRUE, data_index = 2),
    data  = list(wide, long),
    nboot = 5,
    seed  = 1
  )
  expect_s3_class(fit, "jointCovariance")
})
