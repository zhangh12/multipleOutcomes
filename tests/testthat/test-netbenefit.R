## Tests for netbenefit_() and its endpoint helpers.

make_nb_data <- function(n_per_arm = 60, seed = 1L) {
  set.seed(seed)
  rng <- function(n, b_pfs, b_os, y_mean) {
    pfs <- -log(runif(n)) / (log(2) / 5  * exp(b_pfs))
    os  <- -log(runif(n)) / (log(2) / 10 * exp(b_os))
    data.frame(pfs = pfs, pfs_event = 1L,
               os  = os,  os_event  = 1L,
               y   = rnorm(n, y_mean))
  }
  dat <- rbind(
    cbind(arm = 0L, rng(n_per_arm, log(1.0),  log(1.0),  0.0)),
    cbind(arm = 1L, rng(n_per_arm, log(0.78), log(0.85), 0.3))
  )
  dat$pid <- paste0("s-", seq_len(nrow(dat)))
  dat$x1  <- rnorm(nrow(dat))
  dat$x2  <- rnorm(nrow(dat))
  dat
}

std_endpoints <- function() {
  list(
    nb_tte("os",  "os_event"),
    nb_tte("pfs", "pfs_event"),
    nb_continuous("y")
  )
}

# ---------------------------------------------------------------------------
# Constructor argument validation

test_that("netbenefit_() rejects bad inputs", {
  expect_error(netbenefit_("not a formula", endpoints = std_endpoints()),
               "two-sided R formula")
  expect_error(netbenefit_(~ arm, endpoints = std_endpoints()),
               "two-sided R formula")
  expect_error(netbenefit_(y ~ arm, endpoints = list()),
               "non-empty list")
  expect_error(netbenefit_(y ~ arm, endpoints = "not a list"),
               "non-empty list")
  expect_error(netbenefit_(y ~ arm,
                           endpoints = list(nb_tte("os", "os_event"),
                                            "not an endpoint")),
               "not an nb_endpoint")
  expect_error(netbenefit_(y ~ arm, endpoints = std_endpoints(),
                           data_index = -1),
               "positive integer scalar")
})

test_that("netbenefit_() returns a well-formed jc_spec", {
  spec <- netbenefit_(net_benefit ~ arm, endpoints = std_endpoints())
  expect_s3_class(spec, c("jc_spec_netbenefit", "jc_spec"))
  expect_equal(spec$engine, "netbenefit")
  expect_equal(spec$outcome, "net_benefit")
  expect_equal(spec$id_col, "pid")
  expect_equal(spec$data_index, 1L)
  expect_length(spec$endpoints, 3L)
})

# ---------------------------------------------------------------------------
# Endpoint helpers

test_that("nb_tte() validates its arguments", {
  expect_error(nb_tte(1, "event"),    "non-empty column name")
  expect_error(nb_tte("os", ""),      "non-empty column name")
  expect_error(nb_tte("os", "event", margin = -1), "non-negative finite scalar")
  expect_error(nb_tte("os", "event", margin = Inf), "non-negative finite scalar")
  expect_error(nb_tte("os", "event", direction = "wrong"))
  expect_error(nb_tte("os", "event", censor_rule = "wrong"))

  e <- nb_tte("os", "os_event", direction = "shorter_better",
              margin = 30, censor_rule = "ignore")
  expect_s3_class(e, c("nb_endpoint_tte", "nb_endpoint"))
  expect_equal(e$direction, "shorter_better")
  expect_equal(e$margin, 30)
  expect_equal(e$censor_rule, "ignore")
})

test_that("nb_continuous() and nb_binary() validate their arguments", {
  expect_error(nb_continuous(1),  "non-empty column name")
  expect_error(nb_continuous("y", margin = -0.1), "non-negative")
  expect_s3_class(nb_continuous("y"), c("nb_endpoint_continuous", "nb_endpoint"))

  expect_error(nb_binary(1), "non-empty column name")
  expect_error(nb_binary("r", direction = "wrong"))
  expect_s3_class(nb_binary("r"), c("nb_endpoint_binary", "nb_endpoint"))
})

# ---------------------------------------------------------------------------
# jointCovariance: standalone net-benefit fit

test_that("jointCovariance(netbenefit_(...)) produces sensible coef + vcov", {
  dat <- make_nb_data()
  fit <- jointCovariance(
    netbenefit_(net_benefit ~ arm, endpoints = std_endpoints()),
    data = dat
  )
  est <- coef(fit)
  v   <- vcov(fit)
  expect_length(est, 1L)
  expect_true(est >= -1 && est <= 1)
  expect_equal(dim(v), c(1L, 1L))
  expect_true(v[1, 1] > 0)
})

# ---------------------------------------------------------------------------
# Influence function is centered (mean ≈ 0 per BaseAdapter contract)

test_that("per-subject IF is centered at zero", {
  dat <- make_nb_data()
  spec <- netbenefit_(net_benefit ~ arm, endpoints = std_endpoints())
  ad <- multipleOutcomes:::NetBenefitAdapter$new(spec, dat)
  ad$fit_model()
  score <- ad$get_score()
  expect_lt(max(abs(colSums(score) / nrow(score))), 1e-10)
})

# ---------------------------------------------------------------------------
# Score rows must stay aligned with `dat` rows under arbitrary row order.
# Regression for a class of bugs where a future refactor of
# compute_netbenefit() or NetBenefitAdapter$fit_model() introduces a row
# reordering that isn't mirrored when the per-subject IF is scattered back
# into the n × 1 score matrix.

test_that("score rows align with dat rows under arbitrary permutations", {
  base <- make_nb_data(n_per_arm = 30, seed = 1L)
  spec <- netbenefit_(net_benefit ~ arm, endpoints = std_endpoints())

  fit_once <- function(d) {
    a <- multipleOutcomes:::NetBenefitAdapter$new(spec, d); a$fit_model()
    a$get_score()
  }

  ## Original (block-sorted arm) order.
  s_blk <- fit_once(base)
  expect_identical(rownames(s_blk), base$pid)

  ## Random row permutation.
  set.seed(42)
  perm <- sample(nrow(base))
  d_perm <- base[perm, ]
  s_perm <- fit_once(d_perm)
  expect_identical(rownames(s_perm), d_perm$pid)

  ## Per-pid IFs must be identical between the two orderings.
  align <- function(s) s[order(rownames(s)), , drop = FALSE]
  expect_equal(unname(align(s_blk)), unname(align(s_perm)),
               tolerance = 1e-12)

  ## Arm-interleaved order (worst case for any silent block-sort assumption).
  d_int <- base[order(seq_len(nrow(base)) %% 2), ]
  s_int <- fit_once(d_int)
  expect_identical(rownames(s_int), d_int$pid)
  expect_equal(unname(align(s_blk)), unname(align(s_int)),
               tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# NA in endpoint columns is non-fatal: comp_pair() treats it as "tied at
# this level, fall through to the next." The adapter warns once per fit
# so the missingness isn't silent.

test_that("NA in a continuous endpoint does not error and emits a warning", {
  dat <- make_nb_data(n_per_arm = 30, seed = 11L)
  dat$y[5] <- NA
  spec <- netbenefit_(net_benefit ~ arm,
                      endpoints = list(nb_continuous("y")))
  expect_warning(
    fit <- jointCovariance(spec, data = dat),
    "NAs found in endpoint column.*'y'"
  )
  expect_true(is.finite(coef(fit)))
  expect_true(vcov(fit)[1, 1] > 0)
})

test_that("NA in a TTE time column does not error and emits a warning", {
  dat <- make_nb_data(n_per_arm = 30, seed = 12L)
  dat$pfs[3] <- NA
  spec <- netbenefit_(net_benefit ~ arm,
                      endpoints = list(nb_tte("os",  "os_event"),
                                       nb_tte("pfs", "pfs_event"),
                                       nb_continuous("y")))
  expect_warning(
    fit <- jointCovariance(spec, data = dat),
    "NAs found in endpoint column.*'pfs'"
  )
  expect_true(is.finite(coef(fit)))
})

test_that("NA at endpoint k is equivalent to that subject tying at k", {
  ## Equivalence: a row with NA at endpoint k behaves the same as a row
  ## whose value at k is the *exact same* as the partner's value (so
  ## every pair involving that row ties at k). We can't reach that
  ## exact equivalence on a 2D dataset because each NA-row pairs with n
  ## different partners, but we CAN reach a stronger equivalence on the
  ## single-endpoint case: with K = 1 endpoint, NA at row i means
  ## row i contributes 0 to every pair, so the estimate must equal the
  ## one computed after dropping row i from BOTH the numerator and the
  ## denominator's pair count.
  ##
  ## Concretely: estimate-with-NA == (N_W' - N_L') / (n_C * n_T),
  ## where (N_W', N_L') count only pairs NOT involving the NA-row.
  set.seed(99); n <- 30
  dat <- data.frame(
    pid = paste0("p", seq_len(n)),
    arm = rep(0:1, each = n/2),
    y   = rnorm(n)
  )
  na_row <- 7L                          # a control-arm row
  dat_na <- dat; dat_na$y[na_row] <- NA

  spec <- netbenefit_(net_benefit ~ arm,
                      endpoints = list(nb_continuous("y")))
  fit_full <- jointCovariance(spec, data = dat)

  ## Suppress the (expected) warning from the NA fit.
  fit_na <- suppressWarnings(jointCovariance(spec, data = dat_na))

  ## Manual reference: drop the NA-row entirely from the pair grid by
  ## setting its 'y' to a value that ties with every treatment subject
  ## — impossible in general, so instead we compare against the kernel
  ## sum we expect: the same numerator as fit_full minus the contribution
  ## of pairs (na_row, j), all divided by the SAME denominator nC*nT.
  d0 <- dat[dat$arm == 0L, ]; d1 <- dat[dat$arm == 1L, ]
  nC <- nrow(d0); nT <- nrow(d1)
  na_row_in_d0 <- which(d0$pid == dat$pid[na_row])
  comp <- function(v0, v1) sign(v1 - v0)            # +1 if treatment wins
  ψ_full <- outer(d0$y, d1$y, comp)
  ψ_na   <- ψ_full
  ψ_na[na_row_in_d0, ] <- 0                          # NA-row ties everyone
  expect_equal(unname(coef(fit_full)), sum(ψ_full) / (nC * nT),
               tolerance = 1e-12, label = "full reference")
  expect_equal(unname(coef(fit_na)),   sum(ψ_na)   / (nC * nT),
               tolerance = 1e-12, label = "NA-as-tie equivalence")
})

test_that("score rows still align with dat rows when NAs are present", {
  base <- make_nb_data(n_per_arm = 30, seed = 1L)
  base$y[c(2, 17)] <- NA
  spec <- netbenefit_(net_benefit ~ arm, endpoints = std_endpoints())

  ad <- multipleOutcomes:::NetBenefitAdapter$new(spec, base)
  suppressWarnings(ad$fit_model())
  s <- ad$get_score()
  expect_identical(rownames(s), base$pid)
  ## IF column-sum still zero (BaseAdapter contract).
  expect_lt(max(abs(colSums(s) / nrow(s))), 1e-10)
})

# ---------------------------------------------------------------------------
# Cross-engine off-diagonal block is populated and non-trivial: cheap
# single-dataset sanity check that catches "off-diagonal silently zero"
# bugs without going through Monte Carlo. The full empirical calibration
# of this block lives in test-mc-cross-engine.R.

test_that("netbenefit_ × glm_ off-diagonal carries real correlation", {
  dat <- sim_netbenefit(n_per_arm = 80, seed = 7L)
  fit <- jointCovariance(
    netbenefit_(net_benefit ~ arm, endpoints = std_endpoints()),
    glm_(x1 ~ arm, family = "gaussian"),
    glm_(x2 ~ arm, family = "gaussian"),
    data = dat
  )
  corr <- cov2cor(vcov(fit))
  ## NB at id 1; x1~arm at id 3; x2~arm at id 5.
  expect_gt(abs(corr[1, 3]), 0.10)
  expect_gt(abs(corr[1, 5]), 0.10)
})

# ---------------------------------------------------------------------------
# pated() integration alongside glm_ prognostic specs

test_that("pated() works with netbenefit_ primary + glm_ prognostic", {
  dat <- make_nb_data()
  fit <- pated(
    netbenefit_(net_benefit ~ arm, endpoints = std_endpoints()),
    glm_(x1 ~ arm, family = "gaussian"),
    glm_(x2 ~ arm, family = "gaussian"),
    data = dat
  )
  expect_s3_class(fit, "pated")
  expect_true(all(c("PATED", "Standard") %in% sub("\\*$", "", fit$method)))
  expect_true(is.finite(fit$estimate[1]))
  expect_true(fit$stderr[1] > 0)
  expect_true(fit$pvalue[1] >= 0 && fit$pvalue[1] <= 1)
})

# ---------------------------------------------------------------------------
# Bootstrap path

test_that("bootstrap path returns finite estimate + vcov", {
  dat <- make_nb_data(n_per_arm = 40)
  fit <- jointCovariance(
    netbenefit_(net_benefit ~ arm, endpoints = std_endpoints()),
    data = dat, nboot = 20, seed = 7L
  )
  expect_length(coef(fit), 1L)
  expect_true(is.finite(coef(fit)))
  expect_true(vcov(fit)[1, 1] > 0)
})

# ---------------------------------------------------------------------------
# Direction flip negates the estimate

test_that("flipping all direction arguments flips the sign of the estimate", {
  dat <- make_nb_data()
  fit_default <- jointCovariance(
    netbenefit_(net_benefit ~ arm, endpoints = std_endpoints()),
    data = dat
  )
  fit_flipped <- jointCovariance(
    netbenefit_(net_benefit ~ arm,
                endpoints = list(
                  nb_tte("os",  "os_event", direction = "shorter_better"),
                  nb_tte("pfs", "pfs_event", direction = "shorter_better"),
                  nb_continuous("y", direction = "smaller_better")
                )),
    data = dat
  )
  expect_equal(unname(coef(fit_default)), -unname(coef(fit_flipped)))
})

# ---------------------------------------------------------------------------
# Large margin saturates: nobody beats anyone on the leading endpoint

test_that("a margin larger than the OS range zeros out OS contribution", {
  dat <- make_nb_data()
  os_range <- diff(range(dat$os))
  ## With OS margin = huge, OS always ties; PFS becomes the decider.
  fit_pfs_only <- jointCovariance(
    netbenefit_(net_benefit ~ arm,
                endpoints = list(
                  nb_tte("os",  "os_event", margin = 10 * os_range),
                  nb_tte("pfs", "pfs_event"),
                  nb_continuous("y")
                )),
    data = dat
  )
  fit_no_os <- jointCovariance(
    netbenefit_(net_benefit ~ arm,
                endpoints = list(
                  nb_tte("pfs", "pfs_event"),
                  nb_continuous("y")
                )),
    data = dat
  )
  expect_equal(unname(coef(fit_pfs_only)), unname(coef(fit_no_os)),
               tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Priority is encoded by list order

test_that("reordering the endpoints list changes the estimate", {
  ## Build a dataset where OS and PFS disagree, so the priority order matters.
  set.seed(13)
  n <- 30
  dat <- data.frame(
    arm       = rep(0:1, each = n),
    os        = c(runif(n, 0, 5), runif(n, 5, 10)),  # trt has longer OS
    os_event  = 1L,
    pfs       = c(runif(n, 5, 10), runif(n, 0, 5)),  # trt has shorter PFS
    pfs_event = 1L,
    y         = c(rnorm(n), rnorm(n))
  )
  dat$pid <- paste0("s-", seq_len(nrow(dat)))

  fit_os_first <- jointCovariance(
    netbenefit_(net_benefit ~ arm,
                endpoints = list(nb_tte("os",  "os_event"),
                                 nb_tte("pfs", "pfs_event"),
                                 nb_continuous("y"))),
    data = dat
  )
  fit_pfs_first <- jointCovariance(
    netbenefit_(net_benefit ~ arm,
                endpoints = list(nb_tte("pfs", "pfs_event"),
                                 nb_tte("os",  "os_event"),
                                 nb_continuous("y"))),
    data = dat
  )
  expect_true(coef(fit_os_first)  > 0.5)   # OS-decided: treatment dominates
  expect_true(coef(fit_pfs_first) < -0.5)  # PFS-decided: control dominates
})

# ---------------------------------------------------------------------------
# Arm-level inference matches glm_'s convention (numeric: smaller = reference)

test_that("numeric arm uses smaller value as reference", {
  dat <- make_nb_data()
  spec <- netbenefit_(net_benefit ~ arm, endpoints = std_endpoints())
  ad <- multipleOutcomes:::NetBenefitAdapter$new(spec, dat)
  ad$fit_model()
  expect_equal(unname(ad$get_coef()), coef(jointCovariance(
    netbenefit_(net_benefit ~ arm, endpoints = std_endpoints()),
    data = dat
  )))
})

test_that("factor arm uses levels[1] as reference (overridable)", {
  dat <- make_nb_data()
  dat$arm_fct <- factor(ifelse(dat$arm == 1L, "trt", "pbo"),
                        levels = c("pbo", "trt"))
  fit1 <- jointCovariance(
    netbenefit_(net_benefit ~ arm,     endpoints = std_endpoints()),
    data = dat
  )
  fit2 <- jointCovariance(
    netbenefit_(net_benefit ~ arm_fct, endpoints = std_endpoints()),
    data = dat
  )
  expect_equal(unname(coef(fit1)), unname(coef(fit2)))

  ## Flip the factor levels: estimate should flip sign.
  dat$arm_fct_rev <- factor(ifelse(dat$arm == 1L, "trt", "pbo"),
                            levels = c("trt", "pbo"))
  fit3 <- jointCovariance(
    netbenefit_(net_benefit ~ arm_fct_rev, endpoints = std_endpoints()),
    data = dat
  )
  expect_equal(unname(coef(fit1)), -unname(coef(fit3)))
})

# ---------------------------------------------------------------------------
# Arm-column validation

test_that("non-binary arm column errors", {
  dat <- make_nb_data()
  dat$arm <- sample(0:2, nrow(dat), replace = TRUE)
  expect_error(
    jointCovariance(
      netbenefit_(net_benefit ~ arm, endpoints = std_endpoints()),
      data = dat
    ),
    "exactly two distinct"
  )
})
