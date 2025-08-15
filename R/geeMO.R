
#' return estimates from GEE model
#' to be used when bootstrap is needed.
#'
#' @param formula an object of class \code{formula}. 
#' @param id a vector which identifies the clusters. The length of \code{id} 
#' should be the same as the number of observations. Data are assumed to be 
#' sorted so that observations on a cluster are contiguous rows for all 
#' entities in the \code{formula}.
#' @param data a data frame. User must always use its default value \code{NULL}. 
#' @param family description
#' @param corstr a character string specifying the correlation structure. 
#' The following are permitted: \code{"independence"}, \code{"fixed"}, 
#' \code{"stat_M_dep"}, \code{"non_stat_M_dep"}, \code{"exchangeable"}, 
#' \code{"AR-M"} and \code{"unstructured"}. 
#' @param family a family object: a list of functions and expressions for 
#' defining link and variance functions. Families supported in gee are 
#' \code{gaussian}, \code{binomial}, \code{poisson}, \code{Gamma}, and 
#' \code{quasi}; see the glm and family documentation. Some links are not 
#' currently available: \code{1/mu^2} and \code{sqrt} have not been 
#' hard-coded in the \code{cgee} engine at present. The inverse gaussian 
#' variance function is not available. All combinations of remaining 
#' functions can be obtained either by family selection or by the use of 
#' \code{quasi}.
#' 
#' @export
geeMO <- function(formula, id, data = NULL, family, corstr){
  
  if(is.null(data)){
    return(NULL)
  }
  
  fit <- gee(formula, id = id, data = data, family = family, corstr = corstr)
  coef(fit)
  
}
