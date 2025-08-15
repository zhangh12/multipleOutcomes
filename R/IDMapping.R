
# df is of length that equals to the number of specified models
# df[i] is the number of parameters in model i
# This function returns index of parameters of model i in the vector of 
# parameters of all models
IDMapping <- function(df, i, var_name = NULL){
  if(i == 1){
    ids <- 1:df[1]
  }else{
    ids <- (sum(df[1:(i - 1)]) + 1):sum(df[1:i])
  }
  
  if(!is.null(var_name) && length(var_name) == length(ids)){
    names(ids) <- var_name
  }
  
  ids
}
