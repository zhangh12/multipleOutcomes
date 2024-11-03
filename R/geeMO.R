
#' return estimates from GEE model
#' to be used when bootstrap is needed.
#' @export
geeMO <- function(formula, id, data = NULL, family, corstr){
  
  if(is.null(data)){
    return(NULL)
  }
  
  fit <- gee(formula, id = id, data = data, family = family, corstr = corstr)
  coef(fit)
  
}
