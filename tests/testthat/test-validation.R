# data_index validation, exercised at two layers:
#   1. validate_data_index() called inside each *_() constructor
#   2. a bounds check (data_index <= length(data)) inside jointCovariance.default()

# ---------------------------------------------------------------------------
test_that("validate_data_index() rejects bad inputs", {
  bad_inputs <- list(
    "NA"             = NA_integer_,
    "NA_real"        = NA_real_,
    "NULL"           = NULL,
    "zero"           = 0,
    "negative"       = -1L,
    "non-integer"    = 1.5,
    "string"         = "foo",
    "vector-length2" = c(1L, 2L),
    "logical"        = TRUE
  )
  for (lab in names(bad_inputs)) {
    expect_error(multipleOutcomes:::validate_data_index(bad_inputs[[lab]]),
                 regexp  = "positive integer scalar",
                 label   = lab)
  }
})

test_that("validate_data_index() accepts valid inputs", {
  expect_equal(multipleOutcomes:::validate_data_index(1L), 1L)
  expect_equal(multipleOutcomes:::validate_data_index(1),  1L)
  expect_equal(multipleOutcomes:::validate_data_index(2.0), 2L)
  expect_equal(multipleOutcomes:::validate_data_index(7L), 7L)
})

# ---------------------------------------------------------------------------
test_that("every *_() constructor calls validate_data_index()", {
  # Each constructor should error on data_index = -1 with the validator message.
  expect_error(glm_(y ~ arm, family = "gaussian", data_index = -1),
               "positive integer scalar")
  expect_error(coxph_(Surv(time, event) ~ arm, data_index = 0),
               "positive integer scalar")
  expect_error(logrank_(Surv(time, event) ~ arm, data_index = 1.7),
               "positive integer scalar")
  expect_error(gee_(y ~ arm, family = "gaussian", corstr = "exchangeable",
                    data_index = NA),
               "positive integer scalar")
  expect_error(km_(Surv(time, event) ~ arm, conf_type = "log",
                   data_index = "foo"),
               "positive integer scalar")
  expect_error(quantile_(y ~ arm, probs = c(.25, .5),
                         data_index = c(1L, 2L)),
               "positive integer scalar")
  # mmrm_ goes through additional validation (control checks) before reaching
  # the data_index check, so just verify the error still surfaces. mmrm is in
  # Imports so it's safe to require here.
  expect_error(mmrm_(y ~ arm + us(visit | pid), reml = TRUE, data_index = -3),
               "positive integer scalar")
})

# ---------------------------------------------------------------------------
test_that("jointCovariance() rejects data_index that exceeds length(data)", {
  set.seed(1)
  n <- 50
  d1 <- data.frame(pid = paste0("p", seq_len(n)),
                   arm = rbinom(n, 1, 0.5), y = rnorm(n))

  # One data frame supplied; spec asks for index 2.
  expect_error(
    jointCovariance(
      glm_(y ~ arm, family = "gaussian", data_index = 2L),
      data = list(d1)
    ),
    regexp = "Model 1 uses data_index = 2 but only 1 data frame"
  )

  # Two data frames supplied; second spec asks for index 3.
  d2 <- d1
  expect_error(
    jointCovariance(
      glm_(y ~ arm, family = "gaussian", data_index = 1L),
      glm_(y ~ arm, family = "gaussian", data_index = 3L),
      data = list(d1, d2)
    ),
    regexp = "Model 2 uses data_index = 3 but only 2 data frame"
  )
})

# ---------------------------------------------------------------------------
test_that("default data_index = 1 still works (no explicit arg)", {
  set.seed(2)
  n <- 50
  d <- data.frame(pid = paste0("p", seq_len(n)),
                  arm = rbinom(n, 1, 0.5), y = rnorm(n))

  # No data_index, single data.frame, no list() wrap — the shortest form.
  fit <- jointCovariance(
    glm_(y ~ arm, family = "gaussian"),
    data = d
  )
  expect_s3_class(fit, "jointCovariance")
  expect_equal(length(coef(fit)), 2L)
})
