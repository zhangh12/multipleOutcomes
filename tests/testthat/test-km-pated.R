# Regression tests for the km_ + pated() pipeline.
#
# The reference fixture inst/testdata/legacy_km_reference.rds was produced from
# the legacy `kmMO + pated(family=..., data_index=...)` API on the main HEAD of
# this repository, run with the same data generator and the same seed.
#
# Quantities split into two tiers:
#   1. Deterministic (data-only): unadjusted KM transformed-S(t), and GLM
#      coefficients of the prognostic covariates. These do not depend on the
#      bootstrap RNG state. We test them at high precision.
#   2. Bootstrap-driven: PATED estimates and SEs, KM SEs. The legacy and new
#      implementations sample different bootstrap sequences (legacy keyed on
#      integer `id`; new keys on string `pid`), so we only sanity-check
#      orderings and inferential conclusions, not pointwise equality.

ref <- readRDS(
  system.file("testdata", "legacy_km_reference.rds", package = "multipleOutcomes")
)

eval_times <- ref$data_signature$eval_times

dat <- gen_delayed_effect_data(seed = ref$data_signature$seed_data)

test_that("data signature matches the legacy reference", {
  expect_equal(nrow(dat),       ref$data_signature$n)
  expect_equal(sum(dat$event),  ref$data_signature$n_event)
  expect_equal(as.list(table(dat$arm)), ref$data_signature$arm_table)
})

# Single pated() fit reused by all downstream assertions.
fit <- pated(
  km_(Surv(time, event) ~ arm, conf_type = "log",
      times = eval_times, data_index = 1),
  glm_(x1 ~ arm, family = "gaussian", data_index = 1),
  glm_(x2 ~ arm, family = "binomial", data_index = 1),
  glm_(z1 ~ arm, family = "gaussian", data_index = 1),
  data        = list(dat),
  nboot       = ref$data_signature$nboot,
  compute_cov = FALSE,
  seed        = ref$data_signature$seed_boot
)

test_that("pated() returns a well-formed result", {
  expect_s3_class(fit, "pated")
  expect_true(is.data.frame(fit))
  expect_true(all(c("term","family","estimate","stderr","pvalue","method","corr")
                  %in% names(fit)))
  # legacy had 47 rows: 22 PATED rows + 22 unadjusted KM rows + 3 prognostic glm rows
  expect_equal(nrow(fit), nrow(ref$pated_table))
})

test_that("unadjusted KM transformed-S(t) matches legacy exactly", {
  new_km <- fit$estimate[fit$family == "km"]
  expect_equal(length(new_km), length(ref$unadjusted_km))
  # log-transformed S(t) is computed deterministically on the original data,
  # so legacy vs new must agree to numeric precision.
  expect_equal(unname(new_km), unname(ref$unadjusted_km), tolerance = 1e-10)
})

test_that("GLM prognostic coefficients match legacy exactly", {
  prog <- fit$method %in% c("Prognostic", "Prognostic*")
  new_glm <- setNames(fit$estimate[prog], fit$term[prog])
  expect_equal(sort(names(new_glm)), sort(names(ref$glm_estimates)))
  expect_equal(new_glm[names(ref$glm_estimates)],
               ref$glm_estimates, tolerance = 1e-10)
})

test_that("KM curve coordinates agree with legacy on S(t) point estimates", {
  new_curve <- attr(attr(fit, "km curve"), "coordinate")
  m <- merge(new_curve, ref$km_curve_data,
             by = c("Time", "Strata"), suffixes = c(".new", ".old"))
  expect_equal(nrow(m), nrow(ref$km_curve_data))
  # S(t) is gInverse(log-S(t)); both should match to numeric precision.
  expect_equal(m$St.new, m$St.old, tolerance = 1e-10)
  # LCI/UCI use bootstrap SEs and differ across runs, but stay within MC noise.
  expect_lt(max(abs(m$LCI.new - m$LCI.old)), 0.05)
  expect_lt(max(abs(m$UCI.new - m$UCI.old)), 0.05)
})

test_that("PATED treatment-effect estimates are close to legacy", {
  new_pated <- fit$estimate[fit$method == "PATED"]
  old_pated <- ref$pated_table$estimate[ref$pated_table$method == "PATED"]
  expect_equal(length(new_pated), length(old_pated))
  expect_lt(max(abs(new_pated - old_pated)), 0.05)
})

test_that("KMAdapter preserves names on its estimate (regression for (a))", {
  jc <- jointCovariance(
    km_(Surv(time, event) ~ arm, conf_type = "log",
        times = eval_times, data_index = 1),
    glm_(x1 ~ arm, family = "gaussian", data_index = 1),
    data  = list(dat),
    nboot = 5,
    seed  = 1
  )
  expect_false(is.null(names(jc$id_map[[1]])))
  expect_true(all(grepl("^time_\\(arm=[01]\\)_\\(", names(jc$id_map[[1]]))))
})

test_that("inferential conclusions on prognostic covariates are stable", {
  pvals <- setNames(fit$pvalue[fit$method %in% c("Prognostic", "Prognostic*")],
                    fit$term[fit$method %in% c("Prognostic", "Prognostic*")])
  # x2 is the prognostic covariate with the strongest signal in this dataset.
  expect_lt(pvals[["x2"]], 0.10)
  # x1 and z1 are not informative for the survival endpoint here.
  expect_gt(pvals[["x1"]], 0.30)
  expect_gt(pvals[["z1"]], 0.05)
})
