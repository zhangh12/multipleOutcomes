## Cross-check netbenefit_() against the reference implementation
## `fit_win_ratio()` from /Users/zhhan/Documents/Github/win_ratio/R/simulate.R.
##
## Exact relationships (verified empirically, then derived from the IF math):
##
##   est_orig    == est_new                              # identical
##   orig$sd     == SE_new * sqrt(n / (n - 1))           # sample vs population var
##   orig$score  == + IF_new / sqrt(n)                   # NetBenefitAdapter
##                                                       # stores the negated
##                                                       # IF to match the
##                                                       # package's
##                                                       # framework-wide
##                                                       # sign convention
##
## The sign on est does NOT flip even though comp_pair() in fit_win_ratio
## returns +1 for "control wins": its outer `comp()` wrapper negates that,
## so row_sum and col_sum carry the same "treatment-wins" sign convention
## as compute_netbenefit().
##
## The SE gap is the standard correction between
##   * R's sd()  : sample variance with (n - 1) denominator
##   * jointCovariance() : the FisherInformation() pass applies an
##                        (n - 1)/n adjustment to produce the population
##                        variance, so the variance of Δ̂ is Var_pop(IF)/n.

skip_if_no_fit_win_ratio <- function() {
  ## Reference implementation lives outside the package tree, so this test
  ## file is a developer-only cross-check. Always skip on CRAN, plus skip
  ## gracefully if the source file isn't present locally.
  testthat::skip_on_cran()
  path <- "/Users/zhhan/Documents/Github/win_ratio/R/simulate.R"
  if (!file.exists(path)) {
    skip("fit_win_ratio source not available on this machine")
  }
  src   <- readLines(path)
  start <- grep("^fit_win_ratio\\s*<-\\s*function", src)
  end_b <- which(src == "}")
  end   <- end_b[end_b > start][1]
  if (length(start) == 0 || is.na(end)) {
    skip("could not locate fit_win_ratio() definition")
  }
  env <- new.env(parent = globalenv())
  if (requireNamespace("dplyr", quietly = TRUE)) {
    env$`%>%`   <- dplyr::`%>%`
    env$filter  <- dplyr::filter
  } else {
    skip("dplyr not available")
  }
  eval(parse(text = paste(src[start:end], collapse = "\n")), envir = env)
  env$fit_win_ratio
}

make_data_pbo_trt <- function(n_per_arm = 80, seed = 1L) {
  set.seed(seed)
  rng <- function(n, b_pfs, b_os, y_mean) {
    pfs <- -log(runif(n)) / (log(2) / 5  * exp(b_pfs))
    os  <- -log(runif(n)) / (log(2) / 10 * exp(b_os))
    data.frame(pfs = pfs, pfs_event = 1L,
               os  = os,  os_event  = 1L,
               y   = rnorm(n, y_mean))
  }
  dat <- rbind(
    cbind(arm = "pbo", rng(n_per_arm, log(1.0),  log(1.0),  0.0)),
    cbind(arm = "trt", rng(n_per_arm, log(0.78), log(0.85), 0.3))
  )
  dat$pid <- paste0("s-", seq_len(nrow(dat)))
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
# Point estimate: bit-for-bit identical (no sign flip)

test_that("netbenefit_() matches fit_win_ratio() on point estimate", {
  fwr <- skip_if_no_fit_win_ratio()
  dat <- make_data_pbo_trt()

  orig <- fwr(dat)
  fit  <- jointCovariance(
    netbenefit_(net_benefit ~ arm, endpoints = std_endpoints()),
    data = dat
  )
  expect_equal(unname(coef(fit)), orig$est, tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Standard error: differ by the population/sample variance factor

test_that("netbenefit_() SE matches fit_win_ratio() up to (n-1)/n correction", {
  fwr <- skip_if_no_fit_win_ratio()
  dat <- make_data_pbo_trt()
  n   <- nrow(dat)

  orig <- fwr(dat)
  fit  <- jointCovariance(
    netbenefit_(net_benefit ~ arm, endpoints = std_endpoints()),
    data = dat
  )

  SE_new <- sqrt(unname(vcov(fit))[1, 1])
  ## SE_new corresponds to the population-variance estimator;
  ## orig$sd is the sample-variance estimator.
  expect_equal(SE_new, orig$sd * sqrt((n - 1) / n), tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Influence function

test_that("per-subject IF matches fit_win_ratio()'s score up to 1/sqrt(n)", {
  fwr <- skip_if_no_fit_win_ratio()
  dat <- make_data_pbo_trt(n_per_arm = 60)
  n   <- nrow(dat)

  orig <- fwr(dat)
  spec <- netbenefit_(net_benefit ~ arm, endpoints = std_endpoints())
  ad   <- multipleOutcomes:::NetBenefitAdapter$new(spec, dat)
  ad$fit_model()
  IF_new <- ad$get_score()[, 1L]

  expect_equal(unname(orig$score), unname(IF_new) / sqrt(n),
               tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Agreement holds across multiple seeds and sample sizes

test_that("agreement holds across multiple seeds and sizes", {
  fwr <- skip_if_no_fit_win_ratio()
  cases <- expand.grid(seed = 1:5, n_per_arm = c(40, 80))
  for (k in seq_len(nrow(cases))) {
    s    <- cases$seed[k]
    npa  <- cases$n_per_arm[k]
    dat  <- make_data_pbo_trt(n_per_arm = npa, seed = s)
    n    <- nrow(dat)
    orig <- fwr(dat)
    fit  <- jointCovariance(
      netbenefit_(net_benefit ~ arm, endpoints = std_endpoints()),
      data = dat
    )
    lab <- sprintf("seed=%d, n_per_arm=%d", s, npa)
    expect_equal(unname(coef(fit)), orig$est, tolerance = 1e-12,
                 label = paste(lab, "est"))
    expect_equal(sqrt(unname(vcov(fit))[1, 1]),
                 orig$sd * sqrt((n - 1) / n),
                 tolerance = 1e-12,
                 label = paste(lab, "SE"))
  }
})
