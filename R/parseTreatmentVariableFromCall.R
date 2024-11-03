
parseTreatmentVariableFromCall <- function(...){
  
  calls <- as.list(substitute(list(...)))[-1L]
  for(i in seq_along(calls)){
    if(!is.call(calls[[i]])){
      stop(str_glue('{calls[[i]]} is not a call. '))
    }
  }
  
  func <- NULL
  outcome <- NULL
  arm <- NULL
  arg <- NULL
  n_terms <- NULL
  idx <- 0
  for(cstr in calls){
    func <- c(func, trimws(sub('\\s*\\(.*\\)', '', deparse1(cstr))))
    outcome <- c(outcome, trimws(sub('^[^(]*\\(([^~]*).*', '\\1', deparse1(cstr))))
    arm <- c(arm, trimws(sub('.*~\\s*([^,\\)]+).*', '\\1', deparse1(cstr))))
    arg0 <- NA
    n_terms0 <- 1
    
    idx <- idx + 1
    if(func[idx] %in% 'kmMO'){
      if(is.null(cstr$conf.type)){
        stop('conf.type is missing in kmMO')
      }
      arg0 <- cstr$conf.type
    }
    
    if(func[idx] %in% 'glmMO'){
      if(is.null(cstr$family)){
        stop('family is missing in glmMO')
      }
      arg0 <- cstr$family
    }
    
    if(func[idx] %in% 'quantileMO'){
      if(is.null(cstr$probs)){
        stop('probs is missing in quantileMO')
      }
      arg0 <- deparse(cstr$probs)
      n_terms0 <- length(eval(cstr$probs))
    }
    arg <- c(arg, arg0)
    n_terms <- c(n_terms, n_terms0)
    rm(arg0, n_terms0)
  }
  data.frame(func, outcome, arm, arg, n_terms)
}
