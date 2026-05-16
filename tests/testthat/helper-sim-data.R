# Reproducible simulators used by both fast smoke tests and the slower
# Monte Carlo empirical-vs-theoretical covariance test.

# Continuous outcome + one prognostic continuous covariate.
sim_gaussian <- function(n = 200,
                         beta_arm = 0.30,
                         beta_z   = 0.50,
                         sigma    = 1,
                         seed     = NULL) {
  if (!is.null(seed)) set.seed(seed)
  arm <- rbinom(n, 1, 0.5)
  z   <- rnorm(n)
  y   <- beta_arm * arm + beta_z * z + rnorm(n, sd = sigma)
  data.frame(pid = paste0("p", seq_len(n)), arm = arm, z = z, y = y)
}

# Binary outcome.
sim_binomial <- function(n = 300,
                         beta_arm = 0.50,
                         beta_z   = 0.50,
                         seed     = NULL) {
  if (!is.null(seed)) set.seed(seed)
  arm <- rbinom(n, 1, 0.5)
  z   <- rnorm(n)
  p   <- plogis(beta_arm * arm + beta_z * z)
  data.frame(pid = paste0("p", seq_len(n)), arm = arm, z = z, y = rbinom(n, 1, p))
}

# Exponential survival under a proportional-hazards model with admin censoring.
sim_coxph <- function(n = 300,
                      beta_arm  = -0.40,
                      beta_z    = -0.30,
                      lam0      = 0.30,
                      admin_tau = 5,
                      seed      = NULL) {
  if (!is.null(seed)) set.seed(seed)
  arm <- rbinom(n, 1, 0.5)
  z   <- rnorm(n)
  lam <- lam0 * exp(beta_arm * arm + beta_z * z)
  t   <- rexp(n, rate = lam)
  event <- as.integer(t <= admin_tau)
  time  <- pmin(t, admin_tau)
  data.frame(pid = paste0("p", seq_len(n)), arm = arm, z = z,
             time = time, event = event)
}

# Multi-type correlated outcomes. A single subject contributes:
#   - a continuous outcome y_cont
#   - a binary outcome y_bin
#   - a right-censored survival outcome (time, event)
#   - a longitudinal trajectory y_long with `n_visits` rows
# Correlation across endpoints is induced by a shared latent vector
# u ~ N(0, Sigma) with Sigma having exchangeable correlation `rho`. The
# treatment `arm` is randomized independently. Returns a list with
# `wide` (one row per subject) and `long` (one row per subject-visit),
# both keyed on the same `pid`. With rho = 0 the endpoints are
# independent given `arm`; with rho > 0 the joint covariance of the
# four `arm`-coefficients has non-trivial off-diagonal blocks, which is
# what jointCovariance() is supposed to estimate.
sim_correlated_multi <- function(n_subj    = 250,
                                 n_visits  = 3,
                                 beta_cont = 0.30,
                                 beta_bin  = 0.50,
                                 beta_tte  = -0.40,
                                 beta_long = 0.30,
                                 alpha     = 0.80,
                                 rho       = 0.50,
                                 lam0      = 0.30,
                                 admin_tau = 5,
                                 sigma_y   = 0.50,
                                 sigma_e   = 0.30,
                                 seed      = NULL) {
  if (!is.null(seed)) set.seed(seed)

  pid <- paste0("p", seq_len(n_subj))
  arm <- rbinom(n_subj, 1, 0.5)

  Sigma <- matrix(rho, 4, 4); diag(Sigma) <- 1
  u <- mvtnorm::rmvnorm(n_subj, sigma = Sigma)

  y_cont <- beta_cont * arm + alpha * u[, 1] + rnorm(n_subj, sd = sigma_y)
  y_bin  <- rbinom(n_subj, 1, plogis(beta_bin * arm + alpha * u[, 2]))
  lam    <- lam0 * exp(beta_tte * arm + alpha * u[, 3])
  t_evt  <- rexp(n_subj, rate = lam)
  time   <- pmin(t_evt, admin_tau)
  event  <- as.integer(t_evt <= admin_tau)

  wide <- data.frame(pid = pid, arm = arm,
                     y_cont = y_cont, y_bin = y_bin,
                     time = time, event = event)

  b <- alpha * u[, 4]
  visit_offset <- 0.10 * (seq_len(n_visits) - 1)
  long_y <- outer(beta_long * arm + b, rep(1, n_visits)) +
            matrix(visit_offset, n_subj, n_visits, byrow = TRUE) +
            matrix(rnorm(n_subj * n_visits, sd = sigma_e), n_subj, n_visits)

  long <- data.frame(
    pid   = rep(pid, each = n_visits),
    visit = factor(rep(seq_len(n_visits), times = n_subj)),
    arm   = rep(arm, each = n_visits),
    y_long = as.vector(t(long_y))
  )

  list(wide = wide, long = long)
}

# Repeated measurements with within-subject AR(1) correlation.
# Returns one row per (subject, visit) with `pid` as the cluster id.
sim_repeated <- function(n_subj = 120,
                         n_visits = 3,
                         beta_arm = 0.30,
                         beta_visit = 0.10,
                         rho = 0.5,
                         sigma = 1,
                         seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  arm <- rbinom(n_subj, 1, 0.5)
  Sigma <- sigma^2 * rho^abs(outer(seq_len(n_visits), seq_len(n_visits), "-"))
  e <- mvtnorm::rmvnorm(n_subj, sigma = Sigma)
  visit_offset <- beta_visit * (seq_len(n_visits) - 1)
  y <- outer(beta_arm * arm, rep(1, n_visits)) +
       matrix(visit_offset, n_subj, n_visits, byrow = TRUE) + e
  long <- data.frame(
    pid   = rep(paste0("p", seq_len(n_subj)), each = n_visits),
    visit = factor(rep(seq_len(n_visits), times = n_subj)),
    arm   = rep(arm, each = n_visits),
    y     = as.vector(t(y))
  )
  long
}
