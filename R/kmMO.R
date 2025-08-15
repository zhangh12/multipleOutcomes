#' xxx
#' @param formula a survival object created by \code{survival::Surv()}. 
#' @param conf.type type of confidence interval. It must be one of \code{"log"}, 
#' \code{"log-log"}, \code{"plain"}, \code{"logit"}, or \code{"arcsin"}. 
#' @param times numeric. A vector of times to compute the survival probabilities. 
#' By default, i.e. \code{NULL}, jump times in data is used. 
#' @param data a data frame. User should use its default value \code{NULL}. 
#' 
#' @export
kmMO <- function(formula, conf.type, times = NULL, data = NULL){
  
  if(!(conf.type %in% c('log','log-log','plain', 'logit', 'arcsin'))){
    stop("conf.type takes value from 'log','log-log','plain', 'logit', 'arcsin'")
  }
  if(is.null(data)){
    return(NULL)
  }
  
  fit <- survfit(formula, data = data, type = 'kaplan-meier', se.fit = FALSE, conf.type = 'none')
  if(is.null(times)){
    times <- sort(unique(fit$time))
  }
  
  pred <- summary(fit, times = times, extend = FALSE)
  if(is.null(pred$strata)){
    pred$strata <- 'trt=.'
  }
  pred$surv <- gFunction(conf.type)(pred$surv)
  
  pred1 <- summary(fit, times = times, extend = TRUE)
  if(is.null(pred1$strata)){
    pred1$strata <- 'trt=.'
  }
  pred1$surv <- gFunction(conf.type)(pred1$surv)
  
  if(length(pred$surv) != length(pred1$surv)){
    pred <- data.frame(strata = pred$strata, time = pred$time, surv = pred$surv)
    pred1 <- data.frame(strata = pred1$strata, time = pred1$time)
    pred <- merge(pred1, pred, by = c('strata', 'time'), all.x = TRUE) %>% 
      dplyr::arrange(strata, time)
    
  }
  
  trans_St <- pred$surv
  names(trans_St) <- paste0('time_(', pred$strata, ')_(', pred$time, ')')
  attr(trans_St, 'conf.type') <- conf.type
  
  trans_St
  
}

