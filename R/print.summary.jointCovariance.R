#' Title Summarize an Analysis of Multiple Outcomes.
#' @description
#' Summarize an analysis of multiple outcomes. 
#' 
#' @param x an object returned by `jointCovariance()`.
#' @param ... for debugging only.
#'
#' @return an invisible object.
#' @export
#'
#' @examples
#' ## no example
print.summary.jointCovariance <- function(x, ...){
  
  cat('Call:\n')
  print(x$call)
  cat('\n')
  
  cat('Coefficients:\n')
  printCoefmat(x$coefficients, P.values = TRUE, has.Pvalue = TRUE)
  
}
