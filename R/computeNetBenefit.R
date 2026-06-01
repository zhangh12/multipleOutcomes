## compute_netbenefit() — internal kernel for the netbenefit_ engine.
##
## Given the per-arm data frames and a list of nb_endpoint specs (in
## hierarchical priority order), runs the all-pairs comparison and returns
## the net-benefit point estimate together with the per-subject influence
## function on its NATURAL scale (i.e., what BaseAdapter expects in
## `self$score` with `inv_hess = 1`).
##
## Inputs
##   data0, data1 : data.frames for the two arms (control, treatment)
##   endpoints    : list of nb_endpoint specs
##
## Returns a list with components
##   est         : scalar  Δ̂ = (N_W − N_L) / (n_C * n_T)
##   if_treat    : numeric of length n_T, IF for treatment subjects
##                 indexed in the order they appear in data1
##   if_control  : numeric of length n_C, IF for control subjects
##                 indexed in the order they appear in data0

compute_netbenefit <- function(data0, data1, endpoints) {

  nC <- nrow(data0)
  nT <- nrow(data1)
  if (nC == 0L || nT == 0L) {
    stop("Both arms must be non-empty.", call. = FALSE)
  }

  ## Pre-extract per-endpoint vectors so the inner loops never touch
  ## data.frame `$` / `[` accessors (the bench_general.R speed trick).
  cache0 <- build_cache(data0, endpoints)
  cache1 <- build_cache(data1, endpoints)

  types       <- vapply(endpoints, `[[`, character(1L), "type")
  directions  <- vapply(endpoints, function(r) {
    if (r$direction %in% c("longer_better", "larger_better")) 1L else -1L
  }, integer(1L))
  margins     <- vapply(endpoints, function(r) {
    as.numeric(r$margin %||% 0)
  }, numeric(1L))
  censor_rule <- vapply(endpoints, function(r) {
    if (identical(r$type, "tte")) r$censor_rule else NA_character_
  }, character(1L))

  K <- length(endpoints)

  ## comp_pair: returns +1 (treatment wins), -1 (control wins), or 0 (tie at
  ## every endpoint).
  comp_pair <- function(i, j) {
    for (k in seq_len(K)) {
      d <- directions[k]
      m <- margins[k]
      c0 <- cache0[[k]]
      c1 <- cache1[[k]]
      if (types[k] == "tte") {
        t0 <- c0$time[i];  e0 <- c0$event[i]
        t1 <- c1$time[j];  e1 <- c1$event[j]
        ## NA in time or event for either subject -> can't decide this
        ## endpoint; treat as tied at this level and fall through.
        if (is.na(t0) || is.na(t1) || is.na(e0) || is.na(e1)) next
        if (e0 == 1L && e1 == 1L) {
          gap <- d * (t1 - t0)
          if (gap >  m) return( 1L)
          if (gap < -m) return(-1L)
        } else if (e0 == 1L && e1 == 0L) {
          ## treatment censored, control observed
          if (censor_rule[k] == "informative") {
            gap <- d * (t1 - t0)
            if (gap >  m) return( 1L)  # censored time already past obs
          }
        } else if (e0 == 0L && e1 == 1L) {
          if (censor_rule[k] == "informative") {
            gap <- d * (t1 - t0)
            if (gap < -m) return(-1L)
          }
        }
        ## both censored, or "ignore" rule: tie at this level, fall through
      } else {
        v0 <- c0$value[i]
        v1 <- c1$value[j]
        ## NA on either side -> tie at this level, fall through.
        if (is.na(v0) || is.na(v1)) next
        gap <- d * (v1 - v0)
        if (gap >  m) return( 1L)
        if (gap < -m) return(-1L)
      }
    }
    0L
  }

  ## Accumulate marginal sums:
  ##   row_sum[i] = sum_j  comp_pair(i, j)         (for control i)
  ##   col_sum[j] = sum_i  comp_pair(i, j)         (for treatment j)
  row_sum <- double(nC)
  col_sum <- double(nT)
  for (i in seq_len(nC)) {
    rs <- 0
    for (j in seq_len(nT)) {
      x <- comp_pair(i, j)
      rs <- rs + x
      col_sum[j] <- col_sum[j] + x
    }
    row_sum[i] <- rs
  }

  est <- sum(row_sum) / (nC * nT)

  ## Per-subject IF (Hájek projection of the two-sample U-statistic):
  ##   IF_treat(j)   = (1/p_T) * ( E[psi(I,j)|j] - Δ )
  ##                ≈ (n / n_T) * ( col_sum[j] / n_C - Δ̂ )
  ##   IF_control(i) = (1/p_C) * ( E[psi(i,J)|i] - Δ )
  ##                ≈ (n / n_C) * ( row_sum[i] / n_T - Δ̂ )
  ##
  ## We return the IFs on their natural scale (mean ≈ 0 by construction;
  ## Var(IF)/n = Var(Δ̂)). BaseAdapter expects this with inv_hess = 1.
  n  <- nC + nT
  if_treat   <- (n / nT) * (col_sum / nC - est)
  if_control <- (n / nC) * (row_sum / nT - est)

  list(est = est, if_treat = if_treat, if_control = if_control)
}

build_cache <- function(dat, endpoints) {
  lapply(endpoints, function(r) {
    if (r$type == "tte") {
      if (!all(c(r$time, r$event) %in% names(dat))) {
        stop("Columns ", r$time, " / ", r$event,
             " not found in data for nb_tte() endpoint.", call. = FALSE)
      }
      list(time = as.numeric(dat[[r$time]]),
           event = as.integer(dat[[r$event]]))
    } else {
      if (!(r$value %in% names(dat))) {
        stop("Column ", r$value,
             " not found in data for nb_continuous() / nb_binary() endpoint.",
             call. = FALSE)
      }
      list(value = as.numeric(dat[[r$value]]))
    }
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
