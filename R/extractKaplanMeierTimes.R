
# Figure out the time points with at least one event. 
# This is used to generate KM-like curves.
extractKaplanMeierTimes <- function(coef, sort = TRUE){
  
  labels <- names(coef)
  times <- labels %>% 
    sapply(function(x) {sub('.*time_\\(.*\\)_\\((.*)\\)', '\\1', x)}) %>% 
    as.numeric() %>% 
    unique()
  
  if(sort){
    times <- sort(times)
  }
  
  times
  
}

