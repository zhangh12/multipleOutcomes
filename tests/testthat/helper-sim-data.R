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
