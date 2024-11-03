
#' Compute bootstrapped variance-covariance matrix of parameters in given models
#' It is used when at least one of the specified model needs bootstrap, 
#' for example, Kaplan-Merier estimate for probability of survival, or 
#' quantiles are used for prognostic variables.
bootstrapMultipleOutcomes <- function(..., data, data_index = NULL, nboot = 10, compute_cov = TRUE, seed = NULL){
  
  input <- checkBootstrapInput(..., data = data, data_index = data_index)
  calls <- input$calls
  data <- input$data
  data_index <- input$data_index
  
  n_models <- length(calls)
  df <- rep(NA, n_models)
  vars <- vector('list', n_models)
  id_map <- NULL
  estimate <- NULL
  km_times <- rep(list(NULL), n_models)
  set.seed(seed)
  for(b in 0:nboot){
    if(b == 0){
      bdata <- data
    }else{
      bdata <- sampleWithReplacement(data)
    }
    call_fit <- NULL
    for (cid in seq_along(calls)) {
      call <- calls[[cid]]
      call$data <- quote(bdata[[data_index[cid]]])
      if(b == 0){
        fit <- eval(call)
        df[cid] <- length(fit)
        id_map[[cid]] <- IDMapping(df, cid, names(fit))
        if(strsplit(deparse(call), '\\(')[[1]][1] =='kmMO' && is.null(call$times)){
          km_times[[cid]] <- extractKaplanMeierTimes(fit)
        }
      }else{
        if(strsplit(deparse(call), '\\(')[[1]][1] =='kmMO' && is.null(call$times)){
          call$times <- km_times[[cid]]
        }
        fit <- eval(call)
      }
      names(fit) <- paste0('model_', cid, '_', names(fit))
      call_fit <- c(call_fit, fit)
    }
    
    if(b == 0){
      bet <- call_fit
    }else{
      estimate <- rbind(estimate, call_fit)
    }
    
  }
  
  rownames(estimate) <- NULL
  
  if(compute_cov){
    mcov <- cov(estimate)
    bootstrap_estimate <- NULL
  }else{
    mcov <- NULL
    bootstrap_estimate <- estimate
  }
  
  ret <-
    list(
      coefficients = bet,
      mcov = mcov,
      bootstrap_estimate = bootstrap_estimate,
      id_map = id_map,
      n_shared_sample_sizes = NULL
    )
  
  invisible(ret)
  
}
