

fitKMCurve <- function(formula, data, conf_type, times = NULL){
  
  tr <- try(fit <- survival::survfit(formula, data = data, 
                                     type = 'kaplan-meier', se.fit = FALSE, 
                                     conf.type = 'none'))
  if('try-error' %in% class(tr)){
    stop('Unable to fit K-M curve. ')
  }
  
  if(is.null(times)){
    times <- sort(unique(fit$time))
  }
  
  pred <- summary(fit, times = times, extend = FALSE)
  if(is.null(pred$strata)){
    pred$strata <- 'trt=.'
  }
  pred$surv <- gFunction(conf_type)(pred$surv)
  
  pred1 <- summary(fit, times = times, extend = TRUE)
  if(is.null(pred1$strata)){
    pred1$strata <- 'trt=.'
  }
  pred1$surv <- gFunction(conf_type)(pred1$surv)
  
  if(length(pred$surv) != length(pred1$surv)){
    pred <- data.frame(strata = pred$strata, time = pred$time, surv = pred$surv)
    pred1 <- data.frame(strata = pred1$strata, time = pred1$time)
    pred <- merge(pred1, pred, by = c('strata', 'time'), all.x = TRUE) %>% 
      dplyr::arrange(strata, time)
    
  }
  
  trans_St <- pred$surv
  names(trans_St) <- paste0('time_(', pred$strata, ')_(', pred$time, ')')
  attr(trans_St, 'conf_type') <- conf_type
  
  trans_St
  
}

