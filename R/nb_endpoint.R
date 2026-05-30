#' Endpoint Constructors for netbenefit_()
#'
#' @description
#' These helpers build the individual endpoint specifications that
#' `netbenefit_()` consumes through its `endpoints` argument. Each call
#' returns a small list describing one comparison rule; the order in
#' which they appear in the `endpoints` list defines the hierarchical
#' priority (first = highest).
#'
#' @section Direction:
#' For each endpoint type, `direction` says whether a larger value (or
#' longer time, for TTE) counts as a win for the subject. With the
#' default convention, a treatment subject "wins" the endpoint when its
#' value, possibly minus a margin, exceeds the control subject's value.
#' Flip the default for endpoints where smaller (or shorter) is better
#' (e.g., pain scores; time to symptom relief).
#'
#' @section Margin:
#' `margin` is a non-negative clinically meaningful difference threshold.
#' A pair only declares a winner when the directional gap strictly exceeds
#' `margin`; otherwise the pair ties at this endpoint and falls through to
#' the next one. `nb_binary()` does not expose `margin` because binary
#' values admit only two outcomes.
#'
#' @section Censoring:
#' `nb_tte()` accepts a `censor_rule` argument. The default
#' `"informative"` matches the conventional Pocock/Buyse rule: if one
#' subject is censored and the other observed, a winner can only be
#' declared in the direction the censoring is uninformative about (i.e.,
#' the censored time is already past the event time). `"ignore"` drops
#' pairs where either subject is censored and treats them as ties at this
#' level.
#'
#' @param time character. Name of the time column in `data`.
#' @param event character. Name of the event-indicator column in `data`
#'   (1 = event observed, 0 = censored).
#' @param value character. Name of the outcome column in `data`.
#' @param direction one of `"longer_better"` / `"shorter_better"`
#'   (`nb_tte()`) or `"larger_better"` / `"smaller_better"`
#'   (`nb_continuous()`, `nb_binary()`).
#' @param margin non-negative numeric. Minimum directional gap required
#'   to declare a winner on this endpoint. Defaults to 0.
#' @param censor_rule (`nb_tte()` only) one of `"informative"` /
#'   `"ignore"`.
#'
#' @return An object of class `c("nb_endpoint_<type>", "nb_endpoint")`
#'   carrying the rule and its arguments.
#'
#' @name nb_endpoint
#' @seealso [netbenefit_()]
NULL

#' @rdname nb_endpoint
#' @export
nb_tte <- function(time, event,
                   direction   = c("longer_better", "shorter_better"),
                   margin      = 0,
                   censor_rule = c("informative", "ignore")) {

  direction   <- match.arg(direction)
  censor_rule <- match.arg(censor_rule)

  if (!is.character(time)  || length(time)  != 1L || !nzchar(time)) {
    stop('"time" must be a single non-empty column name.', call. = FALSE)
  }
  if (!is.character(event) || length(event) != 1L || !nzchar(event)) {
    stop('"event" must be a single non-empty column name.', call. = FALSE)
  }
  if (!is.numeric(margin) || length(margin) != 1L || !is.finite(margin) ||
      margin < 0) {
    stop('"margin" must be a non-negative finite scalar.', call. = FALSE)
  }

  structure(
    list(
      type        = "tte",
      time        = time,
      event       = event,
      direction   = direction,
      margin      = margin,
      censor_rule = censor_rule
    ),
    class = c("nb_endpoint_tte", "nb_endpoint")
  )
}

#' @rdname nb_endpoint
#' @export
nb_continuous <- function(value,
                          direction = c("larger_better", "smaller_better"),
                          margin    = 0) {

  direction <- match.arg(direction)

  if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
    stop('"value" must be a single non-empty column name.', call. = FALSE)
  }
  if (!is.numeric(margin) || length(margin) != 1L || !is.finite(margin) ||
      margin < 0) {
    stop('"margin" must be a non-negative finite scalar.', call. = FALSE)
  }

  structure(
    list(
      type      = "continuous",
      value     = value,
      direction = direction,
      margin    = margin
    ),
    class = c("nb_endpoint_continuous", "nb_endpoint")
  )
}

#' @rdname nb_endpoint
#' @export
nb_binary <- function(value,
                      direction = c("larger_better", "smaller_better")) {

  direction <- match.arg(direction)

  if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
    stop('"value" must be a single non-empty column name.', call. = FALSE)
  }

  structure(
    list(
      type      = "binary",
      value     = value,
      direction = direction,
      margin    = 0
    ),
    class = c("nb_endpoint_binary", "nb_endpoint")
  )
}
