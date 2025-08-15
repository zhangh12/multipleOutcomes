
#' return estimates from GLM model
#' to be used when bootstrap is needed.
#' 
#' @param formula an object of class \code{formula}. 
#' @param family character family, could be \code{gaussian} or \code{bionomial} only. 
#' @param data  a data frame of data. User does not need to specify it. Always 
#' use its default value \code{NULL}. 
#' @export
glmMO <- function(formula, family, data = NULL){
  
  if(is.null(data)){
    return(NULL)
  }
  
  fit <- glm(formula, family, data)
  coef(fit)
  
}
