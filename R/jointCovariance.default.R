#' 
#' @export
jointCovariance.default <- function(..., data, nboot = 0, compute_cov = TRUE, seed = NULL){
  
  if(nboot > 0){
    fit <- bootstrapJointCovariance(..., data = data, nboot = nboot, compute_cov = compute_cov, seed = seed)
  }else{
    fit <- asymptoticJointCovariance(..., data = data)
  }
  
  fit$call <- match.call()
  class(fit) <- 'jointCovariance'
  invisible(fit)
  
}



