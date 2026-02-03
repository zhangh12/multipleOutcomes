
asymptoticJointCovariance <- function(..., data){
  
  specs <- list(...)
  if(length(specs) == 0){
    stop("No model specs provided in ...")
  }
  
  if(!is.list(data)){
    stop("data must be a list of data.frames.")
  }
  
  adapters <- lapply(specs, make_adapter, data_list = data)
  adapters <- lapply(adapters, function(ad){ ad$fit_model(); ad })
  
  n_models <- length(adapters)
  bet <- NULL
  df <- NULL
  vars <- vector('list', n_models)
  for(i in 1:n_models){
    engine <- adapters[[i]]$get_engine()
    if(engine %in% 'km'){
      stop('Model ', i, ' cannot be "km_" with nboot = 0. ', 
           'You may also want to set compute_cov = FALSE to use km_ in ... ', 
           ' of jointCovariance. ')
    }
    fit <- adapters[[i]]$get_coef()
    names(fit) <- paste0('model_', i, '_', names(fit))
    bet <- c(bet, fit)
    df <- c(df, adapters[[i]]$get_dim())
    vars[[i]] <- adapters[[i]]$get_coef_name()
  }
  
  
  n_par <- length(bet)
  mcov <- matrix(0, n_par, n_par)
  n_shared_sample_sizes <- matrix(0, n_models, n_models)
  id_map <- NULL
  
  for(i in 1:n_models){
    idx1 <- adapters[[i]]$get_data_index()
    n_shared_sample_sizes[i, i] <- adapters[[i]]$get_n()
    score1 <- adapters[[i]]$get_score()
    bread1 <- adapters[[i]]$get_bread()
    
    id1 <- IDMapping(df, i, vars[[i]])
    id_map[[i]] <- id1
    
    for(j in i:n_models){
      idx2 <- adapters[[j]]$get_data_index()
      n_shared_sample_sizes[i, j] <- intersect(adapters[[i]]$get_sample_id(), adapters[[j]]$get_sample_id()) %>% length()
      n_shared_sample_sizes[j, i] <- n_shared_sample_sizes[i, j]
      
      score2 <- adapters[[j]]$get_score()
      bread2 <- adapters[[j]]$get_bread()
      
      id2 <- IDMapping(df, j, vars[[j]])
      info <- FisherInformation(score1, score2)
      mcov[id1, id2] <-
        bread1 %*%
        info %*%
        t(bread2) *
        (attr(info, 'n') / nrow(score1) / nrow(score2))
      mcov[id2, id1] <- t(mcov[id1, id2])
      
      rm(score2)
    }
    rm(score1)
    
  }
  
  bet <- unname(bet)
  # rownames(mcov) <- names(bet)
  # colnames(mcov) <- names(bet)
  
  fit <- 
    list(
      coefficients = bet,
      mcov = mcov,
      id_map = id_map,
      n_shared_sample_sizes = n_shared_sample_sizes
    )
  
  invisible(fit)
  
}