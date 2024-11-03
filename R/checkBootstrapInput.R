
#' Process inputs of `multipleOutcomes` when bootstrap will be used to estimate 
#' variance-covariance matrix
checkBootstrapInput <- function(..., data, data_index){
  
  calls <- as.list(substitute(list(...)))[-1L]
  for(i in seq_along(calls)){
    if(!is.call(calls[[i]])){
      stop(str_glue('{calls[[i]]} is not a call. '))
    }
  }
  
  if(is.data.frame(data)){
    data <- list(data)
  }
  
  if(is.null(data_index)){
    if(length(data) == 1){
      data_index <- rep(1, length(calls))
    }
  }
  
  if(length(data_index) != length(calls)){
    stop('data_index should be a integer vector of length ', length(formula))
  }
  
  stopifnot(all(sapply(data, is.data.frame)))
  
  stopifnot(all(sapply(data_index, function(x){abs(x - round(x)) < .Machine$double.eps^0.5})))
  stopifnot(all(data_index <= length(data) & data_index >= 1))
  
  list(calls = calls, data = data, data_index = data_index)
  
}
