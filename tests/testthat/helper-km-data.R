# Reproducible piecewise-exponential dataset with a delayed treatment effect,
# matching the legacy regression fixture saved under inst/testdata/.
gen_delayed_effect_data <- function(n = 600,
                                    lam0       = 0.20,
                                    tau_delay  = 3,
                                    HR_late    = 0.55,
                                    admin_tau  = 12,
                                    gamma1     = -0.40,
                                    gamma2     = -0.50,
                                    seed       = 20260516,
                                    id_col     = "pid") {
  set.seed(seed)

  arm <- rbinom(n, 1, 0.5)
  x_raw <- matrix(rnorm(2 * n), ncol = 2) %*% chol(matrix(c(1, .4, .4, 1), 2))
  x1 <- as.numeric(x_raw[, 1])
  x2 <- as.numeric(x_raw[, 2] > 0)
  z1 <- rnorm(n)

  lam_subj <- lam0 * exp(gamma1 * x1 + gamma2 * x2)
  u <- runif(n)
  S_at_delay <- exp(-lam_subj * tau_delay)

  surv_time <- numeric(n)
  early <- u > S_at_delay
  surv_time[early] <- -log(u[early]) / lam_subj[early]
  late <- !early
  eta <- ifelse(arm[late] == 1, HR_late, 1)
  surv_time[late] <- tau_delay +
    (-log(u[late]) - lam_subj[late] * tau_delay) / (lam_subj[late] * eta)

  event <- as.integer(surv_time <= admin_tau)
  time  <- pmin(surv_time, admin_tau)

  out <- data.frame(
    time  = time,
    event = event,
    arm   = arm,
    x1    = x1,
    x2    = x2,
    z1    = z1
  )
  out[[id_col]] <- paste0("subj-", seq_len(n))
  out
}
