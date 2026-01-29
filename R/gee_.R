#'
#' @export
gee_ <- function(formula, id, family, corstr, R = NULL, b = NULL, Mv = 1, data_index = 1) {
  structure(
    list(
      engine = "gee",
      formula = formula,
      id = id, 
      family  = family,
      corstr = corstr, 
      R = R, 
      b = b,
      Mv = Mv,
      data_index = data_index
    ),
    class = c("jc_spec_gee", "jc_spec")
  )
}


