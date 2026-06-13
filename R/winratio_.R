#' Creating Objects of Hierarchical Log Win-Ratio Statistic
#'
#' @description
#' `winratio_` creates an object to be passed into `jointCovariance` or
#' `pated` through its `...` argument. The object defines a hierarchical
#' win-ratio statistic across a list of endpoints, each declared by
#' `nb_tte()`, `nb_continuous()`, or `nb_binary()`.
#'
#' @param formula a two-sided R formula `<label> ~ arm`. The LHS is used
#'   purely as a row label in `pated()` output; the RHS must contain exactly
#'   one variable, the column in `data` holding treatment-arm assignment.
#' @param endpoints a non-empty list of endpoint specs built by `nb_tte()`,
#'   `nb_continuous()`, or `nb_binary()`. The order of the list encodes the
#'   hierarchical priority: endpoint 1 is checked first; if it ties, the
#'   pair falls through to endpoint 2; and so on.
#' @param data_index integer. Index of the data frame in the `data`
#'   argument of `jointCovariance` to be used.
#'
#' @details
#' The fitted coefficient is on the log scale:
#' \deqn{\widehat\theta = \log(\widehat{WR})
#'       = \log(\widehat\pi_W / \widehat\pi_L)}
#' where \eqn{\widehat\pi_W} and \eqn{\widehat\pi_L} are the proportions of
#' all control-treatment pairs where the treatment subject wins or loses,
#' respectively. Tied pairs remain in the denominator of both proportions
#' but do not contribute to either numerator.
#'
#' The raw win ratio can be recovered by exponentiating the coefficient.
#' The log scale is used because it gives the standard large-sample Wald
#' representation and maps the null win ratio of 1 to 0.
#'
#' The arm reference level is inferred from the arm column the same way
#' `model.matrix(~ arm)` would: `levels(arm)[1]` for factor, the smaller
#' value for numeric or logical, and the alphabetically first value for
#' character. To override, convert the column to a factor with the desired
#' level order before calling `winratio_()`.
#'
#' @return An object of class `c("jc_spec_winratio", "jc_spec")`.
#'
#' @seealso [nb_tte()], [nb_continuous()], [nb_binary()], [jointCovariance()],
#'   [pated()].
#' @export
winratio_ <- function(formula, endpoints, data_index = 1) {

  if (!inherits(formula, "formula") || length(formula) != 3L) {
    stop('"formula" in winratio_ must be a two-sided R formula like ',
         '`log_win_ratio ~ arm`.', call. = FALSE)
  }
  rhs_vars <- all.vars(formula[[3L]])
  if (length(rhs_vars) != 1L) {
    stop('"formula" in winratio_ must have exactly one RHS variable like ',
         '`log_win_ratio ~ arm`.', call. = FALSE)
  }
  if (!is.list(endpoints) || length(endpoints) == 0L) {
    stop('"endpoints" in winratio_ must be a non-empty list of nb_tte() / ',
         'nb_continuous() / nb_binary() specs.', call. = FALSE)
  }
  bad <- !vapply(endpoints, inherits, logical(1), "nb_endpoint")
  if (any(bad)) {
    stop('endpoints[[', which(bad)[1], ']] is not an nb_endpoint. Use ',
         'nb_tte(), nb_continuous(), or nb_binary() to construct each entry.',
         call. = FALSE)
  }

  data_index <- validate_data_index(data_index)
  outcome    <- trimws(sub("\\s*~.*", "", deparse1(formula)))

  structure(
    list(
      engine     = "winratio",
      formula    = formula,
      outcome    = outcome,
      endpoints  = endpoints,
      data_index = data_index,
      id_col     = "pid"
    ),
    class = c("jc_spec_winratio", "jc_spec")
  )
}
