#' 
#' @export
jointCovariance.default <- function(..., data, nboot = 0, compute_cov = TRUE, seed = NULL){

  stopifnot(is.list(data))
  if(is.data.frame(data)){
    data <- list(data)
  }
  for(i in 1:length(data)){
    stopifnot(is.data.frame(data[[i]]))
    if(!('pid' %in% names(data[[i]]))){
      stop('data[[', i, ']] does not consist of a column pid for subject IDs. ')
    }
    data[[i]]$pid <- paste0('PID-', data[[i]]$pid)
  }

  specs <- list(...)
  for (i in seq_along(specs)) {
    di <- specs[[i]]$data_index
    if (!is.null(di) && di > length(data)) {
      stop(sprintf(
        "Model %d uses data_index = %d but only %d data frame(s) were supplied.",
        i, di, length(data)), call. = FALSE)
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



