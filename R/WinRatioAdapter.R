
WinRatioAdapter <- R6::R6Class(
  "WinRatioAdapter",
  inherit = BaseAdapter,
  public = list(

    fit_model = function() {

      self$type <- "winratio"
      spec      <- self$spec
      dat       <- self$data

      arm_var <- private$arm_var()
      if (!(arm_var %in% names(dat))) {
        stop('Arm column "', arm_var, '" not found in data.', call. = FALSE)
      }
      arm_col <- dat[[arm_var]]
      levs    <- private$infer_arm_levels(arm_col, arm_var)

      idx0 <- which(arm_col == levs[1])   # reference, like glm_'s baseline
      idx1 <- which(arm_col == levs[2])   # comparison ("treatment")
      if (length(idx0) + length(idx1) != length(arm_col)) {
        stop('Column "', arm_var, '" contains values other than the two ',
             'inferred levels (', paste(shQuote(levs), collapse = ", "), ').',
             call. = FALSE)
      }

      private$warn_if_endpoints_have_na(dat, spec$endpoints)

      out <- private$compute_winratio(dat[idx0, , drop = FALSE],
                                      dat[idx1, , drop = FALSE],
                                      spec$endpoints)

      ## estimate: log(WR), named after the arm variable so treatment-effect
      ## discovery in pated() / parseTreatmentVariableFromCall can find it.
      self$estimate <- stats::setNames(out$est, arm_var)

      ## Per-subject IF, n x 1 with rownames = pid. Store the NEGATED natural
      ## IF to follow the same estimating-equation sign convention as
      ## NetBenefitAdapter and the other model adapters.
      n         <- nrow(dat)
      score_mat <- matrix(0, n, 1L,
                          dimnames = list(dat[[spec$id_col]], arm_var))
      score_mat[idx1, 1L] <- -out$if_treat
      score_mat[idx0, 1L] <- -out$if_control
      self$score <- score_mat

      self$inv_hess  <- matrix(1, 1L, 1L)
      self$n         <- n
      self$sample_id <- dat[[spec$id_col]]

      invisible(self)
    },

    refit = function(bdata) {
      spec    <- self$spec
      arm_var <- private$arm_var()
      arm_col <- bdata[[arm_var]]
      levs    <- private$infer_arm_levels(arm_col, arm_var)
      idx0    <- which(arm_col == levs[1])
      idx1    <- which(arm_col == levs[2])
      out     <- private$compute_winratio(bdata[idx0, , drop = FALSE],
                                          bdata[idx1, , drop = FALSE],
                                          spec$endpoints)
      stats::setNames(out$est, arm_var)
    }
  ),

  private = list(

    arm_var = function() {
      all.vars(self$spec$formula[[3L]])[1L]
    },

    infer_arm_levels = function(arm_col, arm_var) {
      levs <-
        if (is.factor(arm_col))       levels(arm_col)
        else if (is.logical(arm_col)) c(FALSE, TRUE)
        else if (is.numeric(arm_col)) sort(unique(arm_col))
        else                          sort(unique(as.character(arm_col)))
      if (is.factor(arm_col)) {
        levs <- intersect(levs, unique(as.character(arm_col)))
      }
      if (length(levs) != 2L) {
        stop('Arm column "', arm_var, '" must take exactly two distinct ',
             'values (found ', length(levs), '). ', call. = FALSE)
      }
      levs
    },

    warn_if_endpoints_have_na = function(dat, endpoints) {
      affected <- character(0)
      for (r in endpoints) {
        cols <- if (r$type == "tte") c(r$time, r$event) else r$value
        for (cn in cols) {
          if (cn %in% names(dat) && anyNA(dat[[cn]])) {
            affected <- c(affected, cn)
          }
        }
      }
      if (length(affected)) {
        warning(
          "winratio_(): NAs found in endpoint column(s) ",
          paste(shQuote(unique(affected)), collapse = ", "),
          ". Pairs with NA at an endpoint are treated as tied at that ",
          "level and fall through to the next; affected pairs that ",
          "reach the last endpoint contribute as full ties.",
          call. = FALSE
        )
      }
    },

    compute_winratio = function(data0, data1, endpoints) {

      nC <- nrow(data0)
      nT <- nrow(data1)
      if (nC == 0L || nT == 0L) {
        stop("Both arms must be non-empty.", call. = FALSE)
      }

      cache0 <- private$build_cache(data0, endpoints)
      cache1 <- private$build_cache(data1, endpoints)

      types       <- vapply(endpoints, `[[`, character(1L), "type")
      directions  <- vapply(endpoints, function(r) {
        if (r$direction %in% c("longer_better", "larger_better")) 1L else -1L
      }, integer(1L))
      margins     <- vapply(endpoints, function(r) {
        if (is.null(r$margin)) 0 else as.numeric(r$margin)
      }, numeric(1L))
      censor_rule <- vapply(endpoints, function(r) {
        if (identical(r$type, "tte")) r$censor_rule else NA_character_
      }, character(1L))

      K <- length(endpoints)

      ## Returns +1 (treatment win), -1 (control win), or 0 (tie).
      comp_pair <- function(i, j) {
        for (k in seq_len(K)) {
          d  <- directions[k]
          m  <- margins[k]
          c0 <- cache0[[k]]
          c1 <- cache1[[k]]
          if (types[k] == "tte") {
            t0 <- c0$time[i];  e0 <- c0$event[i]
            t1 <- c1$time[j];  e1 <- c1$event[j]
            if (is.na(t0) || is.na(t1) || is.na(e0) || is.na(e1)) next
            if (e0 == 1L && e1 == 1L) {
              gap <- d * (t1 - t0)
              if (gap >  m) return( 1L)
              if (gap < -m) return(-1L)
            } else if (e0 == 1L && e1 == 0L) {
              if (censor_rule[k] == "informative") {
                gap <- d * (t1 - t0)
                if (gap >  m) return( 1L)
              }
            } else if (e0 == 0L && e1 == 1L) {
              if (censor_rule[k] == "informative") {
                gap <- d * (t1 - t0)
                if (gap < -m) return(-1L)
              }
            }
          } else {
            v0 <- c0$value[i]
            v1 <- c1$value[j]
            if (is.na(v0) || is.na(v1)) next
            gap <- d * (v1 - v0)
            if (gap >  m) return( 1L)
            if (gap < -m) return(-1L)
          }
        }
        0L
      }

      win_row_sum  <- double(nC)
      loss_row_sum <- double(nC)
      win_col_sum  <- double(nT)
      loss_col_sum <- double(nT)

      for (i in seq_len(nC)) {
        wrs <- 0
        lrs <- 0
        for (j in seq_len(nT)) {
          x <- comp_pair(i, j)
          if (x > 0L) {
            wrs <- wrs + 1
            win_col_sum[j] <- win_col_sum[j] + 1
          } else if (x < 0L) {
            lrs <- lrs + 1
            loss_col_sum[j] <- loss_col_sum[j] + 1
          }
        }
        win_row_sum[i]  <- wrs
        loss_row_sum[i] <- lrs
      }

      piW <- sum(win_row_sum)  / (nC * nT)
      piL <- sum(loss_row_sum) / (nC * nT)
      if (!is.finite(piW) || !is.finite(piL) || piW <= 0 || piL <= 0) {
        stop("winratio_(): log win ratio is undefined because the observed ",
             "win and loss proportions must both be positive.", call. = FALSE)
      }

      est <- log(piW / piL)
      n   <- nC + nT

      ifW_treat   <- (n / nT) * (win_col_sum  / nC - piW)
      ifL_treat   <- (n / nT) * (loss_col_sum / nC - piL)
      ifW_control <- (n / nC) * (win_row_sum  / nT - piW)
      ifL_control <- (n / nC) * (loss_row_sum / nT - piL)

      if_treat   <- ifW_treat   / piW - ifL_treat   / piL
      if_control <- ifW_control / piW - ifL_control / piL

      list(est = est,
           raw_wr = piW / piL,
           piW = piW,
           piL = piL,
           if_treat = if_treat,
           if_control = if_control)
    },

    build_cache = function(dat, endpoints) {
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
  )
)
