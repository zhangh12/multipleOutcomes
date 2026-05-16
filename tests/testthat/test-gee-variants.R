# Cover the dispatch branches in src/ugee.c that handle different
# (family, corstr) combinations. The default fast gee_ smoke test exercises
# only (gaussian, exchangeable); these variants hit the other arms of the
# C-level switch on family and correlation structure.
#
# Each fit is a smoke test: we only assert the engine runs end-to-end through
# jointCovariance() and produces a finite coefficient. No PATED here, so we
# don't have to worry about arm-independence of prognostics.

# Build a longitudinal dataset with the appropriate outcome type for the
# requested family. Within-subject correlation comes from a shared per-subject
# random effect plus AR(1) noise.
make_long_for_family <- function(family, n_subj = 80, n_visits = 3,
                                 beta_arm = 0.30, seed = 1) {
  set.seed(seed)
  pid   <- paste0("p", seq_len(n_subj))
  arm   <- rbinom(n_subj, 1, 0.5)
  b     <- rnorm(n_subj, sd = 0.5)            # subject random intercept
  eta   <- outer(beta_arm * arm + b, rep(1, n_visits)) +
           matrix(rnorm(n_subj * n_visits, sd = 0.3), n_subj, n_visits)

  y <- switch(family,
    gaussian = as.vector(t(eta)),
    binomial = as.integer(as.vector(t(plogis(eta))) > runif(n_subj * n_visits)),
    poisson  = stats::rpois(n_subj * n_visits, lambda = exp(as.vector(t(eta))))
  )

  data.frame(
    pid   = rep(pid, each = n_visits),
    arm   = rep(arm, each = n_visits),
    visit = factor(rep(seq_len(n_visits), times = n_subj)),
    y     = y
  )
}

# (family, corstr, Mv) tuples to exercise. Each combination touches different
# code in src/ugee.c's main loop.
gee_configs <- list(
  list(family = "gaussian", corstr = "independence"),
  list(family = "gaussian", corstr = "AR-M",          Mv = 1),
  list(family = "gaussian", corstr = "unstructured"),
  list(family = "gaussian", corstr = "stat_M_dep",    Mv = 1),
  list(family = "binomial", corstr = "exchangeable"),
  list(family = "binomial", corstr = "independence"),
  list(family = "poisson",  corstr = "exchangeable"),
  list(family = "poisson",  corstr = "independence")
)

for (cfg in gee_configs) {
  label <- sprintf("gee_(%s, %s)", cfg$family, cfg$corstr)
  test_that(paste(label, "fits via jointCovariance"), {
    dat <- make_long_for_family(cfg$family, seed = 100L + which(
      sapply(gee_configs, identical, cfg)
    ))
    args <- c(list(formula = y ~ arm,
                   family  = cfg$family,
                   corstr  = cfg$corstr),
              if (!is.null(cfg$Mv)) list(Mv = cfg$Mv))
    spec <- do.call(gee_, args)
    fit  <- jointCovariance(spec, data = dat)
    expect_s3_class(fit, "jointCovariance")
    expect_true(all(is.finite(coef(fit))))
    V <- vcov(fit)
    expect_true(is.matrix(V))
    expect_lt(max(abs(V - t(V))), 1e-6)
  })
}
