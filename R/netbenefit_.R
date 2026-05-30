#' Creating Objects of Hierarchical Net-Benefit (Win-Difference) Statistic
#'
#' @description
#' `netbenefit_` creates an object to be passed into `jointCovariance` or
#' `pated` through its `...` argument. The object defines a hierarchical
#' net-benefit (a.k.a. win-difference, proportion-in-favor) statistic
#' across a list of endpoints, each declared by `nb_tte()`,
#' `nb_continuous()`, or `nb_binary()`.
#'
#' @param formula a two-sided R formula `<label> ~ arm`. The LHS is used
#'   purely as a row label in `pated()` output; the first RHS variable is
#'   the name of the column in `data` holding treatment-arm assignment.
#' @param endpoints a non-empty list of endpoint specs built by `nb_tte()`,
#'   `nb_continuous()`, or `nb_binary()`. The order of the list encodes the
#'   hierarchical priority: endpoint 1 is checked first; if it ties, the
#'   pair falls through to endpoint 2; and so on.
#' @param data_index integer. Index of the data frame in the `data`
#'   argument of `jointCovariance` to be used.
#'
#' @details
#' The estimator is
#' \deqn{\widehat\Delta = \frac{N_W - N_L}{N_W + N_L + N_T}}
#' where \eqn{N_W}, \eqn{N_L}, and \eqn{N_T} are the numbers of treatment
#' wins, losses, and overall ties across all \eqn{n_C \times n_T} pairs
#' (control vs. treatment subject). The per-subject influence function is
#' available in closed form, so both the asymptotic and the bootstrap paths
#' of `jointCovariance` are supported.
#'
#' The arm reference level is inferred from the arm column the same way
#' `model.matrix(~ arm)` would: `levels(arm)[1]` for factor, the smaller
#' value for numeric or logical, and the alphabetically first value for
#' character. To override, convert the column to a factor with the desired
#' level order before calling `netbenefit_()`.
#'
#' @return An object of class `c("jc_spec_netbenefit", "jc_spec")`.
#'
#' @seealso [nb_tte()], [nb_continuous()], [nb_binary()], [jointCovariance()],
#'   [pated()].
#' @export
netbenefit_ <- function(formula, endpoints, data_index = 1) {

  if (!inherits(formula, "formula") || length(formula) != 3L) {
    stop('"formula" in netbenefit_ must be a two-sided R formula like ',
         '`net_benefit ~ arm`.', call. = FALSE)
  }
  if (!is.list(endpoints) || length(endpoints) == 0L) {
    stop('"endpoints" in netbenefit_ must be a non-empty list of nb_tte() / ',
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
      engine     = "netbenefit",
      formula    = formula,
      outcome    = outcome,
      endpoints  = endpoints,
      data_index = data_index,
      id_col     = "pid"
    ),
    class = c("jc_spec_netbenefit", "jc_spec")
  )
}
