## Tests for winratio_() and its log-scale IF.

make_wr_data <- function(n_per_arm = 60, seed = 1L,
                         trt_shift = 0.25, no_ties = TRUE) {
  set.seed(seed)
  y0 <- rnorm(n_per_arm, mean = 0)
  y1 <- rnorm(n_per_arm, mean = trt_shift)
  if (no_ties) {
    y0 <- y0 + seq_along(y0) * 1e-10
    y1 <- y1 + seq_along(y1) * 1e-10 + 1e-7
  }
  dat <- data.frame(
    pid = paste0("p", seq_len(2L * n_per_arm)),
    arm = rep(0:1, each = n_per_arm),
    y   = c(y0, y1)
  )
  dat
}

manual_wr_continuous <- function(dat) {
  d0 <- dat[dat$arm == 0L, , drop = FALSE]
  d1 <- dat[dat$arm == 1L, , drop = FALSE]
  cmp <- outer(d0$y, d1$y, function(v0, v1) sign(v1 - v0))
  wins   <- sum(cmp > 0)
  losses <- sum(cmp < 0)
  wins / losses
}

wilcox_wr_continuous <- function(dat) {
  nC <- sum(dat$arm == 0L)
  nT <- sum(dat$arm == 1L)
  w_control <- unname(stats::wilcox.test(y ~ arm, data = dat,
                                         exact = FALSE)$statistic)
  control_wins <- w_control
  treatment_wins <- nC * nT - control_wins
  treatment_wins / control_wins
}

manual_wr_hier_continuous <- function(dat, endpoints) {
  d0 <- dat[dat$arm == 0L, , drop = FALSE]
  d1 <- dat[dat$arm == 1L, , drop = FALSE]
  wins <- 0
  losses <- 0
  for (i in seq_len(nrow(d0))) {
    for (j in seq_len(nrow(d1))) {
      pair <- 0L
      for (ep in endpoints) {
        v0 <- d0[[ep]][i]
        v1 <- d1[[ep]][j]
        if (is.na(v0) || is.na(v1)) next
        if (v1 > v0) {
          pair <- 1L
          break
        }
        if (v1 < v0) {
          pair <- -1L
          break
        }
      }
      if (pair > 0L) wins <- wins + 1L
      if (pair < 0L) losses <- losses + 1L
    }
  }
  wins / losses
}

test_that("winratio_() rejects bad inputs", {
  expect_error(winratio_("not a formula", endpoints = list(nb_continuous("y"))),
               "two-sided R formula")
  expect_error(winratio_(~ arm, endpoints = list(nb_continuous("y"))),
               "two-sided R formula")
  expect_error(winratio_(y ~ arm + x, endpoints = list(nb_continuous("y"))),
               "exactly one RHS variable")
  expect_error(winratio_(y ~ arm, endpoints = list()),
               "non-empty list")
  expect_error(winratio_(y ~ arm, endpoints = "not a list"),
               "non-empty list")
  expect_error(winratio_(y ~ arm,
                         endpoints = list(nb_continuous("y"),
                                          "not an endpoint")),
               "not an nb_endpoint")
})

test_that("winratio_() returns a well-formed jc_spec", {
  spec <- winratio_(log_win_ratio ~ arm,
                    endpoints = list(nb_continuous("y")))
  expect_s3_class(spec, c("jc_spec_winratio", "jc_spec"))
  expect_equal(spec$engine, "winratio")
  expect_equal(spec$outcome, "log_win_ratio")
  expect_equal(spec$id_col, "pid")
  expect_equal(spec$data_index, 1L)
})

test_that("raw win-ratio estimate matches manual pair counts and wilcox.test", {
  dat <- make_wr_data(n_per_arm = 45, seed = 11L, trt_shift = 0.35)
  fit <- jointCovariance(
    winratio_(log_win_ratio ~ arm, endpoints = list(nb_continuous("y"))),
    data = dat
  )
  wr <- exp(unname(coef(fit)))
  expect_equal(wr, manual_wr_continuous(dat), tolerance = 1e-12)
  expect_equal(wr, wilcox_wr_continuous(dat), tolerance = 1e-12)
})

test_that("jointCovariance(winratio_(...)) produces finite logWR + vcov", {
  dat <- make_wr_data(n_per_arm = 60, seed = 2L, trt_shift = 0.2)
  fit <- jointCovariance(
    winratio_(log_win_ratio ~ arm, endpoints = list(nb_continuous("y"))),
    data = dat
  )
  est <- coef(fit)
  v   <- vcov(fit)
  expect_length(est, 1L)
  expect_true(is.finite(est))
  expect_equal(dim(v), c(1L, 1L))
  expect_true(is.finite(v[1, 1]))
  expect_true(v[1, 1] > 0)
})

test_that("per-subject logWR IF is centered and row-aligned", {
  dat <- make_wr_data(n_per_arm = 40, seed = 3L, trt_shift = 0.3)
  spec <- winratio_(log_win_ratio ~ arm,
                    endpoints = list(nb_continuous("y")))
  ad <- multipleOutcomes:::WinRatioAdapter$new(spec, dat)
  ad$fit_model()
  score <- ad$get_score()
  expect_identical(rownames(score), dat$pid)
  expect_lt(max(abs(colSums(score) / nrow(score))), 1e-10)
})

test_that("score rows align with dat rows under arbitrary permutations", {
  base <- make_wr_data(n_per_arm = 35, seed = 4L, trt_shift = 0.2)
  spec <- winratio_(log_win_ratio ~ arm,
                    endpoints = list(nb_continuous("y")))

  fit_once <- function(d) {
    a <- multipleOutcomes:::WinRatioAdapter$new(spec, d); a$fit_model()
    a$get_score()
  }

  s_blk <- fit_once(base)
  set.seed(42)
  d_perm <- base[sample(nrow(base)), ]
  s_perm <- fit_once(d_perm)

  align <- function(s) s[order(rownames(s)), , drop = FALSE]
  expect_identical(rownames(s_perm), d_perm$pid)
  expect_equal(unname(align(s_blk)), unname(align(s_perm)),
               tolerance = 1e-12)
})

test_that("winratio_() errors clearly when logWR is undefined", {
  dat <- data.frame(
    pid = paste0("p", 1:6),
    arm = rep(0:1, each = 3),
    y   = c(1, 2, 3, 4, 5, 6)
  )
  expect_error(
    jointCovariance(
      winratio_(log_win_ratio ~ arm, endpoints = list(nb_continuous("y"))),
      data = dat
    ),
    "log win ratio is undefined"
  )
})

test_that("single-run null p-value is well-formed", {
  dat <- make_wr_data(n_per_arm = 80, seed = 5L, trt_shift = 0)
  fit <- jointCovariance(
    winratio_(log_win_ratio ~ arm, endpoints = list(nb_continuous("y"))),
    data = dat
  )
  z <- unname(coef(fit)) / sqrt(unname(vcov(fit))[1, 1])
  p <- 2 * stats::pnorm(-abs(z))
  expect_true(is.finite(p))
  expect_true(p >= 0 && p <= 1)
})

test_that("hierarchical endpoint order is honored", {
  dat <- data.frame(
    pid = paste0("p", 1:8),
    arm = rep(0:1, each = 4),
    y1  = c(0, 0, 1, 1, 0, 0, 0, 0),
    y2  = c(1, 1, 0, 0, 0, 2, 0, 2)
  )

  fit_y1_first <- jointCovariance(
    winratio_(log_win_ratio ~ arm,
              endpoints = list(nb_continuous("y1"),
                               nb_continuous("y2"))),
    data = dat
  )
  fit_y2_first <- jointCovariance(
    winratio_(log_win_ratio ~ arm,
              endpoints = list(nb_continuous("y2"),
                               nb_continuous("y1"))),
    data = dat
  )

  expect_equal(exp(unname(coef(fit_y1_first))),
               manual_wr_hier_continuous(dat, c("y1", "y2")),
               tolerance = 1e-12)
  expect_equal(exp(unname(coef(fit_y2_first))),
               manual_wr_hier_continuous(dat, c("y2", "y1")),
               tolerance = 1e-12)
  expect_false(isTRUE(all.equal(unname(coef(fit_y1_first)),
                                unname(coef(fit_y2_first)))))
})

test_that("flipping direction reciprocates raw win ratio", {
  dat <- make_wr_data(n_per_arm = 50, seed = 12L, trt_shift = 0.25)
  fit_default <- jointCovariance(
    winratio_(log_win_ratio ~ arm, endpoints = list(nb_continuous("y"))),
    data = dat
  )
  fit_flipped <- jointCovariance(
    winratio_(log_win_ratio ~ arm,
              endpoints = list(nb_continuous("y",
                                             direction = "smaller_better"))),
    data = dat
  )
  expect_equal(unname(coef(fit_default)), -unname(coef(fit_flipped)),
               tolerance = 1e-12)
  expect_equal(exp(unname(coef(fit_default))),
               1 / exp(unname(coef(fit_flipped))),
               tolerance = 1e-12)
})

test_that("factor arm uses levels[1] as reference", {
  dat <- make_wr_data(n_per_arm = 45, seed = 21L, trt_shift = 0.3)
  dat$arm_fct <- factor(ifelse(dat$arm == 1L, "trt", "pbo"),
                        levels = c("pbo", "trt"))
  fit_num <- jointCovariance(
    winratio_(log_win_ratio ~ arm, endpoints = list(nb_continuous("y"))),
    data = dat
  )
  fit_fct <- jointCovariance(
    winratio_(log_win_ratio ~ arm_fct, endpoints = list(nb_continuous("y"))),
    data = dat
  )
  expect_equal(unname(coef(fit_num)), unname(coef(fit_fct)),
               tolerance = 1e-12)

  dat$arm_fct_rev <- factor(ifelse(dat$arm == 1L, "trt", "pbo"),
                            levels = c("trt", "pbo"))
  fit_rev <- jointCovariance(
    winratio_(log_win_ratio ~ arm_fct_rev,
              endpoints = list(nb_continuous("y"))),
    data = dat
  )
  expect_equal(unname(coef(fit_num)), -unname(coef(fit_rev)),
               tolerance = 1e-12)
})

test_that("NA endpoint values warn, fall through as ties, and keep IF centered", {
  dat <- make_wr_data(n_per_arm = 45, seed = 31L, trt_shift = 0.2)
  dat$y[c(2, 61)] <- NA
  spec <- winratio_(log_win_ratio ~ arm, endpoints = list(nb_continuous("y")))

  expect_warning(
    fit <- jointCovariance(spec, data = dat),
    "NAs found in endpoint column.*'y'"
  )
  expect_true(is.finite(coef(fit)))
  expect_true(vcov(fit)[1, 1] > 0)

  ad <- multipleOutcomes:::WinRatioAdapter$new(spec, dat)
  suppressWarnings(ad$fit_model())
  score <- ad$get_score()
  expect_identical(rownames(score), dat$pid)
  expect_lt(max(abs(colSums(score) / nrow(score))), 1e-10)
})

test_that("bootstrap path returns finite logWR estimate and covariance", {
  dat <- make_wr_data(n_per_arm = 45, seed = 41L, trt_shift = 0.25)
  fit <- jointCovariance(
    winratio_(log_win_ratio ~ arm, endpoints = list(nb_continuous("y"))),
    data = dat, nboot = 20, seed = 9L
  )
  expect_length(coef(fit), 1L)
  expect_true(is.finite(coef(fit)))
  expect_true(vcov(fit)[1, 1] > 0)
})

test_that("winratio_ x glm_ off-diagonal covariance is non-trivial", {
  set.seed(51)
  n_per_arm <- 80
  u <- rnorm(2L * n_per_arm)
  dat <- data.frame(
    pid = paste0("p", seq_len(2L * n_per_arm)),
    arm = rep(0:1, each = n_per_arm),
    y   = 0.2 * rep(0:1, each = n_per_arm) + u + rnorm(2L * n_per_arm),
    x   = u + rnorm(2L * n_per_arm, sd = 0.5)
  )
  fit <- jointCovariance(
    winratio_(log_win_ratio ~ arm, endpoints = list(nb_continuous("y"))),
    glm_(x ~ arm, family = "gaussian"),
    data = dat
  )
  corr <- cov2cor(vcov(fit))
  ## logWR at id 1; x~arm treatment coefficient at id 3.
  expect_gt(abs(corr[1, 3]), 0.10)
})

test_that("pated() works with winratio_ primary and glm_ prognostic specs", {
  set.seed(61)
  n_per_arm <- 90
  u <- rnorm(2L * n_per_arm)
  arm <- rep(0:1, each = n_per_arm)
  dat <- data.frame(
    pid = paste0("p", seq_len(2L * n_per_arm)),
    arm = arm,
    y   = 0.25 * arm + u + rnorm(2L * n_per_arm),
    x1  = u + rnorm(2L * n_per_arm, sd = 0.7),
    x2  = -0.5 * u + rnorm(2L * n_per_arm, sd = 0.7)
  )

  fit <- pated(
    winratio_(log_win_ratio ~ arm, endpoints = list(nb_continuous("y"))),
    glm_(x1 ~ arm, family = "gaussian"),
    glm_(x2 ~ arm, family = "gaussian"),
    data = dat
  )

  expect_s3_class(fit, "pated")
  expect_true(all(c("PATED", "Standard") %in% sub("\\*$", "", fit$method)))
  primary <- fit[fit$term == "log_win_ratio" &
                   sub("\\*$", "", fit$method) %in% c("PATED", "Standard"), ]
  expect_equal(nrow(primary), 2L)
  expect_true(all(is.finite(primary$estimate)))
  expect_true(all(primary$stderr > 0))
  expect_true(all(primary$pvalue >= 0 & primary$pvalue <= 1))
})

test_that("MC: winratio_ null type I error is calibrated", {
  skip_on_cran()
  skip_if_not(mc_enabled(),
              "set MULTIPLEOUTCOMES_RUN_MC=1 to run Monte Carlo tests")
  set.seed(2026)
  K <- 500
  pval <- numeric(K)
  for (k in seq_len(K)) {
    dat <- make_wr_data(n_per_arm = 80, seed = 10000L + k, trt_shift = 0)
    fit <- jointCovariance(
      winratio_(log_win_ratio ~ arm, endpoints = list(nb_continuous("y"))),
      data = dat
    )
    z <- unname(coef(fit)) / sqrt(unname(vcov(fit))[1, 1])
    pval[k] <- 2 * stats::pnorm(-abs(z))
  }
  type1 <- mean(pval < 0.05)
  expect_gt(type1, 0.02)
  expect_lt(type1, 0.08)
})
