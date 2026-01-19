#'
#' @export
glm_ <- function(formula, family, data_index = 1) {
  structure(
    list(
      engine = "glm",
      formula = formula,
      family  = family,
      data_index = data_index
    ),
    class = c("jc_spec_glm", "jc_spec")
  )
}


