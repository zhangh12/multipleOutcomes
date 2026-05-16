# Shared helpers for the Monte Carlo covariance tests
# (test-mc-covariance.R, test-mc-cross-engine.R). Loaded automatically by
# testthat from any helper-*.R file under tests/testthat/.

# Gate: skip MC tests unless explicitly opted in via env var.
# skip_on_cran() already handles the CRAN case; this lets contributors run
# `devtools::test()` without the slow MC tier by default.
mc_enabled <- function() {
  v <- Sys.getenv("MULTIPLEOUTCOMES_RUN_MC", unset = "")
  isTRUE(v %in% c("1", "true", "TRUE", "yes"))
}

# Run K replicates of a jointCovariance() spec on freshly-simulated data and
# return:
#   beta_hat  : K x p matrix of stacked coefficient vectors
#   vcov_mean : p x p mean of the per-replicate vcov(fit) (theoretical)
#
# `simulate` is a function of one integer arg (the replicate index) returning
# the dataset (either a data.frame or a list, whatever `fit_fn` expects).
# `fit_fn` takes that dataset and returns a jointCovariance object.
# If `model_index` is non-NULL, only that model's coefficients/cov-block are
# kept.
mc_run <- function(K, simulate, fit_fn, model_index = NULL) {
  beta_list <- vector("list", K)
  vcov_list <- vector("list", K)
  for (k in seq_len(K)) {
    dat <- simulate(k)
    fit <- fit_fn(dat)
    if (is.null(model_index)) {
      beta_list[[k]] <- coef(fit)
      vcov_list[[k]] <- vcov(fit)
    } else {
      ids <- fit$id_map[[model_index]]
      beta_list[[k]] <- coef(fit)[ids]
      vcov_list[[k]] <- vcov(fit)[ids, ids, drop = FALSE]
    }
  }
  list(beta_hat  = do.call(rbind, beta_list),
       vcov_mean = Reduce("+", vcov_list) / K)
}

# Assert that an empirical covariance matrix is close to a theoretical one,
# with a relative tolerance on diagonals (variance terms) and an absolute
# tolerance on off-diagonals (covariance terms).
expect_cov_close <- function(emp, thr,
                             rel_diag    = 0.30,
                             abs_offdiag = 0.05,
                             label       = "") {
  d_e <- diag(emp); d_t <- diag(thr)
  rel <- abs(d_e - d_t) / pmax(abs(d_t), 1e-8)
  expect_lt(max(rel),
            rel_diag,
            label = paste("diagonal rel-error", label))

  offdiag_e <- emp; diag(offdiag_e) <- 0
  offdiag_t <- thr; diag(offdiag_t) <- 0
  expect_lt(max(abs(offdiag_e - offdiag_t)),
            abs_offdiag,
            label = paste("off-diagonal abs-error", label))
}
