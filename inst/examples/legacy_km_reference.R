# ---------------------------------------------------------------------------
# Reference example for the legacy KM workflow (kmMO + pated).
#
# Runs against the *main* HEAD interface:
#   - kmMO(...) returns transformed S(t) at requested times
#   - pated(... , family = NULL, data_index = ..., nboot > 0, compute_cov = FALSE)
#     dispatches through bootstrapMultipleOutcomes, which evaluates each
#     `*MO()` call on each bootstrap replicate.
#
# The synthetic dataset has a delayed treatment effect on survival (HR = 1
# before tau_delay, HR = HR_late after), plus two prognostic covariates that
# are correlated with the survival endpoint and one nuisance covariate that
# is independent of it. This is a setting where PATED should tighten the
# pointwise CIs of S(t) relative to the unadjusted KM.
#
# Output: writes legacy_km_reference.rds with everything we will need to
# regression-test the new km_/pated implementation.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(multipleOutcomes)
  library(survival)
  library(dplyr)
})

# ---- 1. data generator ----------------------------------------------------

genDelayedEffectData <- function(n = 600,
                                 lam0       = 0.20,    # baseline hazard
                                 tau_delay  = 3,       # delayed-effect onset
                                 HR_late    = 0.55,    # HR after tau_delay
                                 admin_tau  = 12,      # admin censoring time
                                 gamma1     = -0.40,   # x1 effect on log-hazard
                                 gamma2     = -0.50,   # x2 effect on log-hazard
                                 seed       = 20260516) {

  set.seed(seed)

  arm <- rbinom(n, 1, 0.5)

  # two prognostic covariates, mildly correlated with each other
  x_raw <- matrix(rnorm(2 * n), ncol = 2) %*% chol(matrix(c(1, .4, .4, 1), 2))
  x1 <- as.numeric(x_raw[, 1])              # continuous prognostic
  x2 <- as.numeric(x_raw[, 2] > 0)          # binary prognostic

  # nuisance: independent of survival
  z1 <- rnorm(n)

  # subject-specific multiplier on baseline hazard
  lam_subj <- lam0 * exp(gamma1 * x1 + gamma2 * x2)

  # piecewise-exponential survival time:
  #   t <= tau_delay : hazard = lam_subj
  #   t  > tau_delay : hazard = lam_subj * eta, eta = 1 if arm==0 else HR_late
  u <- runif(n)
  S_at_delay <- exp(-lam_subj * tau_delay)

  surv_time <- numeric(n)
  early <- u > S_at_delay
  # event before delay onset: -log(u) = lam_subj * t
  surv_time[early] <- -log(u[early]) / lam_subj[early]

  # event after delay: -log(u) = lam_subj * tau + lam_subj * eta * (t - tau)
  late <- !early
  eta <- ifelse(arm[late] == 1, HR_late, 1)
  surv_time[late] <- tau_delay +
    (-log(u[late]) - lam_subj[late] * tau_delay) / (lam_subj[late] * eta)

  event <- as.integer(surv_time <= admin_tau)
  time  <- pmin(surv_time, admin_tau)

  data.frame(
    id    = seq_len(n),    # legacy bootstrap path keys on `id`
    time  = time,
    event = event,
    arm   = arm,
    x1    = x1,
    x2    = x2,
    z1    = z1
  )
}

# ---- 2. fit ---------------------------------------------------------------

dat <- genDelayedEffectData()

cat("Sample size by arm:\n"); print(table(dat$arm))
cat("\nEvent rate by arm:\n"); print(tapply(dat$event, dat$arm, mean))

# Pre-pick a small grid of evaluation times so the bootstrap and the
# unadjusted KM are computed at the same points (without this, jump-time
# differences across bootstrap replicates can leak NAs).
eval_times <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

set.seed(1)
fit <- pated(
  kmMO(Surv(time, event) ~ arm, conf.type = "log", times = eval_times),
  glmMO(x1 ~ arm, family = "gaussian"),
  glmMO(x2 ~ arm, family = "binomial"),
  glmMO(z1 ~ arm, family = "gaussian"),
  data       = list(dat),
  data_index = c(1, 1, 1, 1),
  nboot      = 200,
  compute_cov = FALSE,    # KM forces this anyway
  seed       = 42
)

print(fit)

# ---- 3. capture everything we need to regression-test against -------------

ref <- list(
  call             = attr(fit, "call"),
  pated_table      = as.data.frame(fit),
  rel_eff          = attr(fit, "Rel. Eff."),
  conf_type        = attr(fit, "conf.type"),
  pated_curve_data = attr(attr(fit, "pated curve"), "coordinate"),
  km_curve_data    = attr(attr(fit, "km curve"),    "coordinate"),
  comp_ci_data     = lapply(attr(fit, "comp ci"), function(p) p$data),
  data_signature   = list(
    n          = nrow(dat),
    n_event    = sum(dat$event),
    arm_table  = as.list(table(dat$arm)),
    seed_data  = 20260516,
    seed_boot  = 42,
    nboot      = 200,
    eval_times = eval_times
  )
)

# Always write into the new_interface working tree so the reference snapshot
# can be picked up later regardless of which branch we ran the legacy code on.
out_dir <- Sys.getenv(
  "LEGACY_KM_OUT_DIR",
  "/Users/zhhan/Documents/Github/multipleOutcomes/inst/examples"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(ref, file.path(out_dir, "legacy_km_reference.rds"))

# Plain-text snapshot for quick visual / diff comparison.
sink(file.path(out_dir, "legacy_km_reference.txt"))
cat("=== call ===\n");           print(ref$call)
cat("\n=== pated table ===\n");  print(ref$pated_table, digits = 6)
cat("\n=== Rel. Eff. ===\n");    print(ref$rel_eff, digits = 6)
cat("\n=== conf.type ===\n");    print(ref$conf_type)
cat("\n=== pated curve data ===\n"); print(ref$pated_curve_data, digits = 6)
cat("\n=== km curve data ===\n");    print(ref$km_curve_data,    digits = 6)
cat("\n=== data signature ===\n"); print(ref$data_signature)
sink()

cat("\nWrote ", file.path(out_dir, "legacy_km_reference.rds"), "\n", sep = "")
cat("Wrote ",  file.path(out_dir, "legacy_km_reference.txt"), "\n", sep = "")
