

bootstrapJointCovariance <- function(..., data, nboot, compute_cov, seed){
  
  specs <- list(...)
  if(length(specs) == 0){
    stop("No model specs provided in ...")
  }
  
  if(!is.list(data)){
    stop("data must be a list of data.frames.")
  }
  
  adapters <- lapply(specs, make_adapter, data_list = data)
  adapters <- lapply(adapters, function(ad){ ad$fit_model(); ad })
  
  for(ad in adapters){
    if(ad$get_engine() %in% 'km'){
      if(compute_cov){
        compute_cov <- FALSE
        message('compute_cov is set to FALSE as km_ is in use. ', 
                'You can compute the covariance matrix using $bootstrap_estimate. ')
      }
      break
    }
  }
  
  n_models <- length(adapters)
  bet <- NULL
  df <- NULL
  vars <- vector('list', n_models)
  id_map <- NULL
  estimate <- NULL
  
  set.seed(seed)
  for(b in 0:nboot){
    if(b == 0){
      bdata <- data
    }else{
      bdata <- sampleWithReplacement(data)
    }
    
    model_fit <- NULL
    for (i in 1:n_models){
      data_index <- adapters[[i]]$get_data_index()
      fit <- adapters[[i]]$refit(bdata[[data_index]])
      names(fit) <- paste0('model_', i, '_', names(fit))
      model_fit <- c(model_fit, fit)
      
      if(b == 0){
        df[i] <- length(fit)
        id_map[[i]] <- IDMapping(df, i, adapters[[i]]$get_coef_name())
      }
    }
    
    if(b == 0){
      bet <- model_fit
    }else{
      estimate <- rbind(estimate, model_fit)
    }
    
  }
  
  rownames(estimate) <- NULL
  
  if(compute_cov){
    mcov <- cov(estimate)
    rownames(mcov) <- NULL
    colnames(mcov) <- NULL
    bootstrap_estimate <- NULL
  }else{
    mcov <- NULL
    bootstrap_estimate <- estimate
  }
  
  n_shared_sample_sizes <- matrix(0, n_models, n_models)
  
  for(i in 1:n_models){
    idx1 <- adapters[[i]]$get_data_index()
    n_shared_sample_sizes[i, i] <- adapters[[i]]$get_n()
    for(j in i:n_models){
      idx2 <- adapters[[j]]$get_data_index()
      n_shared_sample_sizes[i, j] <- intersect(adapters[[i]]$get_sample_id(), adapters[[j]]$get_sample_id()) %>% length()
      n_shared_sample_sizes[j, i] <- n_shared_sample_sizes[i, j]
    }
  }
  
  bet <- unname(bet)
  
  ret <-
    list(
      coefficients = bet,
      mcov = mcov,
      bootstrap_estimate = bootstrap_estimate,
      id_map = id_map,
      n_shared_sample_sizes = n_shared_sample_sizes
    )
  
  invisible(ret)
}