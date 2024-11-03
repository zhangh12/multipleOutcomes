
#' return estimates from GLM model
#' to be used when bootstrap is needed.
#' @export
glmMO <- function(formula, family, data = NULL){
  
  if(is.null(data)){
    return(NULL)
  }
  
  fit <- glm(formula, family, data)
  coef(fit)
  
}
