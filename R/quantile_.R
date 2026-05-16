#' Creating Objects of Group Quantile Differences
#'
#' @description
#' `quantile_` is a wrapper function creating an object that, for each
#' requested probability, computes the difference between the two arms'
#' sample quantiles of the outcome. The object is passed to `jointCovariance`
#' or `pated` through `...`. Because the empirical-quantile estimator has no
#' tractable closed-form score, this engine is available only through the
#' bootstrap path (`nboot > 0`).
#'
#' @param formula a two-sided formula `y ~ arm` where the right-hand side is a
#'   binary grouping variable.
#' @param probs numeric vector of probabilities in `(0, 1)`. By default the
#'   first quartile, median, and third quartile.
#' @param data_index integer. Index of the data frame in the `data` argument
#'   of `jointCovariance` to be used.
#'
#' @export
quantile_ <- function(formula, probs = c(0.25, 0.5, 0.75), data_index = 1) {

  if (!inherits(formula, "formula")) {
    stop('"formula" in quantile_ must be a R formula.')
  }
  if (!is.numeric(probs) || any(probs <= 0 | probs >= 1)) {
    stop('"probs" must be numeric values in (0, 1).')
  }
  data_index <- validate_data_index(data_index)

  structure(
    list(
      engine    = "quantile",
      formula   = formula,
      probs     = probs,
      data_index = data_index,
      id_col    = "pid"
    ),
    class = c("jc_spec_quantile", "jc_spec")
  )
}
