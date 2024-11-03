
#' Compute asymptotic variance-covariance matrix of parameters in given models
#' It is used for models where bootstrap is not needed
asymptoticMultipleOutcomes <- function(..., data, family, data_index = NULL, score_epsilon = 1e-6){
  
  input <- checkDefaultInput(..., family = family, data = data, data_index = data_index)
  formulas <- input$formulas
  family <- input$family
  data <- input$data
  data_index <- input$data_index
  
  n <- sapply(data, nrow)
  
  n_models <- length(formulas)
  models <- vector('list', n_models)
  inv_hess <- vector('list', n_models)
  
  df <- rep(NA, n_models)
  bet <- NULL
  vars <- vector('list', n_models)
  for(i in 1:n_models){
    
    if(family[i] %in% c('binomial', 'gaussian')){
      models[[i]] <- glm(formulas[[i]], family[i], data[[data_index[i]]])
    }else{
      if(family[i] %in% 'coxph'){
        models[[i]] <- coxph(formulas[[i]], data[[data_index[i]]], model = TRUE)
      }else{
        if(family[i] %in% 'logrank'){
          models[[i]] <- coxph(formulas[[i]], data[[data_index[i]]], model = TRUE, iter = 0)
          tmp_score <- as.matrix(resid(models[[i]], type = 'score'))
          tmp_imat <- imatCoxph(models[[i]], mean = FALSE)
          theta <- solve(matrixSquareRoot(tmp_imat), colSums(tmp_score))
          names(theta) <- names(coef(models[[i]]))
          attr(models[[i]], 'theta') <- theta
          rm(theta, tmp_imat, tmp_score)
        }else{
          if(family[i] %in% 'gmm'){
            models[[i]] <- gmm4(formulas[[i]], data[[data_index[i]]], theta0 = eval(formals(formulas[[i]])$start))
            attr(models[[i]], 'score') <- formulas[[i]](models[[i]]@theta, data[[data_index[i]]])
          }else{ ## gee+id+family+corstr
            config <- parseGeeConfig(family[i])
            if(!(config$id %in% names(data[[data_index[i]]]))){
              stop(str_glue('{config$id} is not in data[[{data_index[i]}]]'))
            }
            data[[data_index[i]]]$gee_id <- data[[data_index[i]]][, config$id]
            suppressMessages(
              capture.output(
                models[[i]] <- gee(formulas[[i]], id = gee_id, data = data[[data_index[i]]], family = config$family, corstr = config$corstr), 
                file = NULL
              )
            )
          }
        }
      }
    }
    
    if(family[i] %in% 'gmm'){
      cf <- models[[i]]@theta
    }else{
      if(family[i] %in% 'logrank'){
        cf <- attr(models[[i]], 'theta')
      }else{
        cf <- coef(models[[i]])
      }
    }
    
    df[i] <- length(cf)
    bet <- c(bet, unname(cf))
    vars[[i]] <- names(cf)
  }
  
  n_par <- length(bet)
  mcov <- matrix(0, n_par, n_par)
  n_shared_sample_sizes <- matrix(0, n_models, n_models)
  id_map <- NULL
  
  for(i in 1:n_models){
    n_shared_sample_sizes[i, i] <- nrow(data[[data_index[i]]])
    
    if(family[i] %in% c('binomial', 'gaussian')){
      dm1 <- model.matrix(formulas[[i]], model.frame(formulas[[i]], data[[data_index[i]]]))
      score1 <- resid(models[[i]], type = 'response') * dm1
      inv_hess[[i]] <- inverseHessianMatrixGlm(family[i], dm1, models[[i]])
      rm(dm1)
    }else{
      if(family[i] %in% c('coxph', 'logrank')){
        score1 <- -as.matrix(resid(models[[i]], type = 'score'))
        if(family[i] %in% 'coxph'){
          inv_hess[[i]] <- solve(imatCoxph(models[[i]]))
        }else{
          tmp_imat <- imatCoxph(models[[i]], mean = FALSE)
          inv_hess[[i]] <- solve(matrixSquareRoot(tmp_imat)) * nrow(score1)
        }
      }else{
        if(family[i] %in% 'gmm'){
          score1 <- attr(models[[i]], 'score')
          inv_hess[[i]] <- inverseJacobianMatrixGmm(formulas[[i]], models[[i]]@theta, data[[data_index[i]]])
        }else{ ## gee+id+family
          score1 <- models[[i]]$score
          rownames(score1) <- 1:nrow(score1)
          inv_hess[[i]] <- solve(-models[[i]]$hess / nrow(score1))
        }
      }
    }
    
    if(!(family[i] %in% 'logrank')){
      stopifnot((colSums(score1) / nrow(score1)) %>% abs %>% max < score_epsilon)
    }
    
    id1 <- IDMapping(df, i, vars[[i]])
    id_map[[i]] <- id1
    for(j in i:n_models){
      n_shared_sample_sizes[i, j] <- intersect(rownames(data[[data_index[[i]]]]), rownames(data[[data_index[[j]]]])) %>% length()
      n_shared_sample_sizes[j, i] <- n_shared_sample_sizes[i, j]
      
      if(family[j] %in% c('binomial', 'gaussian')){
        dm2 <- model.matrix(formulas[[j]], model.frame(formulas[[j]], data[[data_index[j]]]))
        score2 <- resid(models[[j]], type = 'response') * dm2
        if(is.null(inv_hess[[j]])){
          inv_hess[[j]] <- inverseHessianMatrixGlm(family[j], dm2, models[[j]])
        }
        rm(dm2)
      }else{
        if(family[j] %in% c('coxph', 'logrank')){
          score2 <- -as.matrix(resid(models[[j]], type = 'score'))
          if(family[j] %in% 'coxph'){
            inv_hess[[j]] <- solve(imatCoxph(models[[j]]))
          }else{
            tmp_imat <- imatCoxph(models[[j]], mean = FALSE)
            inv_hess[[j]] <- solve(matrixSquareRoot(tmp_imat)) * nrow(score2)
          }
        }else{
          if(family[j] %in% 'gmm'){
            score2 <- attr(models[[j]], 'score')
            inv_hess[[j]] <- inverseJacobianMatrixGmm(formulas[[j]], models[[j]]@theta, data[[data_index[j]]])
          }else{  ## gee+id+family+corstr
            score2 <- models[[j]]$score
            rownames(score2) <- 1:nrow(score2)
            inv_hess[[j]] <- solve(-models[[j]]$hess / nrow(score2))
          }
        }
      }
      
      if(!(family[j] %in% 'logrank')){
        stopifnot((colSums(score2) / nrow(score2)) %>% abs %>% max < score_epsilon)
      }
      
      id2 <- IDMapping(df, j)
      info <- FisherInformation(score1, score2)
      mcov[id1, id2] <-
        inv_hess[[i]] %*%
        info %*%
        t(inv_hess[[j]]) *
        (attr(info, 'n') / nrow(score1) / nrow(score2))
      mcov[id2, id1] <- t(mcov[id1, id2])
      
      rm(score2)
    }
    rm(score1)
    
    models[i] <- list(NULL)
    inv_hess[i] <- list(NULL)
    
  }
  
  fit <-
    list(
      coefficients = bet,
      mcov = mcov,
      id_map = id_map,
      family = family,
      n_shared_sample_sizes = n_shared_sample_sizes
    )
  
  invisible(fit)
  
}
