
#' return estimate of log HR from coxph model
#' to be used when bootstrap is needed.
#' @param formula a survival object using function \code{survival::Surv()}. 
#' @param data a data frame. User should always use its default value \code{NULL}. 
#' @param ties method to handle ties, must be \code{"efron"}, \code{"breslow"}, 
#' or \code{"exact"}. 
#' @export
coxphMO <- function(formula, data = NULL, ties = c('efron', 'breslow', 'exact')){
  
  if(is.null(data)){
    return(NULL)
  }
  
  ties <- match.arg(ties)
  fit <- coxph(formula, data, ties = ties)
  coef(fit)
  
}

