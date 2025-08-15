
#' Return difference in quantile between arms
#' formula can be endpoint ~ trt
#' @param formula an object of class \code{formula}. 
#' @param data a data frame. User should always use its default value \code{NULL}. 
#' @param probs numeric. A vector of probabilities with values in \code{[0, 1]}. 
#' By default, difference in first quartile, median, and third quartile are computed. 
#' 
#' @export
quantileMO <- function(formula, data = NULL, probs = c(.25, .5, .75)){
  
  if(is.null(data)){
    return(NULL)
  }
  
  mf <- model.frame(formula, data)
  
  outcome <- deparse(formula[[2]])
  group <- deparse(formula[[3]])
  
  if(length(unique(mf[[group]])) != 2){
    stop('Only two groups are allowed in quantileMO.')
  }
  
  if(length(group) != 1){
    stop('Only one variable defining group is allowed in RHS for quantileMO.')
  }
  
  if(length(outcome) != 1){
    stop('Only one variable defining outcome is allowed in LHS for quantileMO.')
  }
  
  split_data <- split(mf[[outcome]], mf[[group]])
  
  qs <- lapply(split_data, function(sdata){quantile(sdata, probs = probs)})
  qs <- do.call(cbind, qs)
  
  qdiff <- qs[, 1] - qs[, 2]
  names(qdiff) <- paste0(group, '_', rownames(qs))
  
  qdiff
  
}

