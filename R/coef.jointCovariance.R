
#' Extract Model Coefficients
#' @description
#' `coef` is a generic function. 
#' 
#' @param object an object returned by `jointCovariance()`.
#' @param model_index `NULL` if displaying coefficients of all fitted models;
#' otherwise, an integer indicating the fitted model.
#' @param ... for debugging only
#'
#' @return a vector of coefficient estimates
#' @export
#'
coef.jointCovariance <- function(object, model_index = NULL, ...){
  
  ret <- object$coefficients
  
  if(!is.null(model_index)){
    id_map <- object$id_map
    if(model_index > length(id_map)){
      stop('Invalid model index.')
    }
    
    ret <- ret[id_map[[model_index]]]
    names(ret) <- names(id_map[[model_index]])
  }
  
  ret
  
}
