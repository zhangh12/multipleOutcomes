
QuantileAdapter <- R6::R6Class(
  "QuantileAdapter",
  inherit = BaseAdapter,
  public = list(

    fit_model = function() {

      self$type <- "quantile"
      self$estimate <- compute_quantile_diff(self$spec$formula,
                                             self$data,
                                             self$spec$probs)

      self$n <- nrow(self$data)
      self$sample_id <- self$data[[self$spec$id_col]]

      invisible(self)
    },

    refit = function(bdata) {
      compute_quantile_diff(self$spec$formula, bdata, self$spec$probs)
    }
  )
)

# Two-group quantile difference. Returns a named vector with one entry per
# probability, where the names follow the legacy `arm_25%`, `arm_50%`, ...
# convention so pated()'s arm-lookup regex picks them up.
compute_quantile_diff <- function(formula, data, probs) {

  mf <- stats::model.frame(formula, data)
  outcome <- mf[[1L]]
  group   <- mf[[2L]]
  if (length(unique(group)) != 2L) {
    stop("Only two groups are allowed in quantile_.")
  }

  group_name <- names(mf)[2L]
  split_data <- split(outcome, group)

  qs <- do.call(
    cbind,
    lapply(split_data,
           function(sdata) stats::quantile(sdata, probs = probs))
  )

  qdiff <- qs[, 1] - qs[, 2]
  names(qdiff) <- paste0(group_name, "_", rownames(qs))
  qdiff
}
