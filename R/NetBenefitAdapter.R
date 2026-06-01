
NetBenefitAdapter <- R6::R6Class(
  "NetBenefitAdapter",
  inherit = BaseAdapter,
  public = list(

    fit_model = function() {

      self$type <- "netbenefit"
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

      out <- compute_netbenefit(dat[idx0, , drop = FALSE],
                                dat[idx1, , drop = FALSE],
                                spec$endpoints)

      ## estimate: a single scalar named after the arm variable, so the
      ## arm-discovery in pated() / parseTreatmentVariableFromCall picks it up.
      self$estimate <- stats::setNames(out$est, arm_var)

      ## per-subject IF, n x 1 with rownames = pid.
      ## We store the NEGATED IF so that `inv_hess %*% mean(score)` carries
      ## the same sign as the framework's other engines (e.g., GLMAdapter
      ## negates its bread to the same effect: inv_hess * mean(score) ≈
      ## -(estimate - true)). Without this sign, off-diagonal blocks between
      ## netbenefit_ and glm_/coxph_ would have a flipped sign.
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
      ## bootstrap path: recompute the U-statistic on the resampled data and
      ## return a named scalar. Bootstrap inherits its arm-discovery from
      ## the same private helper.
      spec    <- self$spec
      arm_var <- private$arm_var()
      arm_col <- bdata[[arm_var]]
      levs    <- private$infer_arm_levels(arm_col, arm_var)
      idx0    <- which(arm_col == levs[1])
      idx1    <- which(arm_col == levs[2])
      out     <- compute_netbenefit(bdata[idx0, , drop = FALSE],
                                    bdata[idx1, , drop = FALSE],
                                    spec$endpoints)
      stats::setNames(out$est, arm_var)
    }
  ),

  private = list(

    arm_var = function() {
      all.vars(self$spec$formula[[3L]])[1L]
    },

    ## Same reference-level rule as model.matrix(~ arm):
    ##   factor    → levels(arm)
    ##   logical   → c(FALSE, TRUE)
    ##   numeric   → sort(unique(arm))
    ##   character → sort(unique(arm))   (matches as-factor coercion)
    infer_arm_levels = function(arm_col, arm_var) {
      levs <-
        if (is.factor(arm_col))       levels(arm_col)
        else if (is.logical(arm_col)) c(FALSE, TRUE)
        else if (is.numeric(arm_col)) sort(unique(arm_col))
        else                          sort(unique(as.character(arm_col)))
      ## drop unused factor levels
      if (is.factor(arm_col)) {
        levs <- intersect(levs, unique(as.character(arm_col)))
      }
      if (length(levs) != 2L) {
        stop('Arm column "', arm_var, '" must take exactly two distinct ',
             'values (found ', length(levs), '). ', call. = FALSE)
      }
      levs
    },

    ## NAs in endpoint columns aren't fatal — comp_pair() treats them as
    ## "tied at this level, fall through" — but they silently shrink the
    ## effective number of decided pairs at affected endpoints. Warn the
    ## user once per fit so the missingness isn't invisible.
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
          "netbenefit_(): NAs found in endpoint column(s) ",
          paste(shQuote(unique(affected)), collapse = ", "),
          ". Pairs with NA at an endpoint are treated as tied at that ",
          "level and fall through to the next; affected pairs that ",
          "reach the last endpoint contribute as full ties.",
          call. = FALSE
        )
      }
    }
  )
)
