
#' return estimate of log HR from coxph model
#' to be used when bootstrap is needed.
#' @export
coxphMO <- function(formula, data = NULL, ties = c('efron', 'breslow', 'exact')){
  
  if(is.null(data)){
    return(NULL)
  }
  
  ties <- match.arg(ties)
  fit <- coxph(formula, data, ties = ties)
  coef(fit)
  
}

