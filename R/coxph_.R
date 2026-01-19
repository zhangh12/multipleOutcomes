#'
#' @export
coxph_ <- function(formula, data_index = 1) {
  structure(
    list(
      engine = "coxph",
      formula = formula,
      data_index = data_index
    ),
    class = c("jc_spec_coxph", "jc_spec")
  )
}

