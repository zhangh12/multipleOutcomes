#'
#' @export
logrank_ <- function(formula, ties = c('efron', 'breslow', 'exact'), data_index = 1){
  
  ties <- match.arg(ties)
  
  structure(
    list(
      engine = "longrank",
      formula = formula,
      ties = ties,
      data_index = data_index
    ),
    class = c("jc_spec_logrank", "jc_spec")
  )
}

