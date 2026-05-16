# ---------------------------------------------------------------------------
# Mirror of legacy_km_reference.R using the new_interface API.
#
#   kmMO(...)  -> km_(...)
#   glmMO(...) -> glm_(...)
#   pated(... , family=..., data_index=..., ...) -> pated(... , ...)
#
# Same data-generation function (genDelayedEffectData) and same seeds, so any
# residual difference vs the legacy run is attributable to the new
# implementation, not to randomness.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(multipleOutcomes)
  library(survival)
  library(dplyr)
})

genDelayedEffectData <- function(n = 600,
                                 lam0       = 0.20,
                                 tau_delay  = 3,
                                 HR_late    = 0.55,
                                 admin_tau  = 12,
                                 gamma1     = -0.40,
                                 gamma2     = -0.50,
                                 seed       = 20260516) {

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

  data.frame(
    pid   = paste0("subj-", seq_len(n)),  # new_interface keys on `pid`
    time  = time,
    event = event,
    arm   = arm,
    x1    = x1,
    x2    = x2,
    z1    = z1
  )
}

dat <- genDelayedEffectData()
cat("Sample size by arm:\n"); print(table(dat$arm))
cat("\nEvent rate by arm:\n"); print(tapply(dat$event, dat$arm, mean))

eval_times <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

set.seed(1)
fit <- pated(
  km_(Surv(time, event) ~ arm, conf_type = "log",
      times = eval_times, data_index = 1),
  glm_(x1 ~ arm, family = "gaussian", data_index = 1),
  glm_(x2 ~ arm, family = "binomial", data_index = 1),
  glm_(z1 ~ arm, family = "gaussian", data_index = 1),
  data        = list(dat),
  nboot       = 200,
  compute_cov = FALSE,
  seed        = 42
)

print(fit)

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

out_dir <- Sys.getenv(
  "NEW_KM_OUT_DIR",
  "/Users/zhhan/Documents/Github/multipleOutcomes/inst/examples"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(ref, file.path(out_dir, "new_km_reference.rds"))

sink(file.path(out_dir, "new_km_reference.txt"))
cat("=== call ===\n");           print(ref$call)
cat("\n=== pated table ===\n");  print(ref$pated_table, digits = 6)
cat("\n=== Rel. Eff. ===\n");    print(ref$rel_eff, digits = 6)
cat("\n=== conf.type ===\n");    print(ref$conf_type)
cat("\n=== pated curve data ===\n"); print(ref$pated_curve_data, digits = 6)
cat("\n=== km curve data ===\n");    print(ref$km_curve_data,    digits = 6)
cat("\n=== data signature ===\n");   print(ref$data_signature)
sink()

cat("\nWrote ", file.path(out_dir, "new_km_reference.rds"), "\n", sep = "")
cat("Wrote ",  file.path(out_dir, "new_km_reference.txt"), "\n", sep = "")
