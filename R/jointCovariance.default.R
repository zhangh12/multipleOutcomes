#' 
#' @export
jointCovariance.default <- function(..., data, nboot = 0, compute_cov = TRUE, seed = NULL){
  
  stopifnot(is.list(data))
  for(i in 1:length(data)){
    stopifnot(is.data.frame(data[[i]]))
    if(!('pid' %in% names(data[[i]]))){
      stop('data[[', i, ']] does not consist of a column pid for subject IDs. ')
    }
  }
  
  if(nboot > 0){
    fit <- bootstrapJointCovariance(..., data = data, nboot = nboot, compute_cov = compute_cov, seed = seed)
  }else{
    fit <- asymptoticJointCovariance(..., data = data)
  }
  
  fit$call <- match.call()
  class(fit) <- 'jointCovariance'
  invisible(fit)
  
}



