

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

  # extend = TRUE carries the last observed step value forward when a requested
  # time is past the last event in a stratum; this is the conventional KM
  # extrapolation and avoids NAs leaking into the bootstrap covariance matrix.
  pred <- summary(fit, times = times, extend = TRUE)
  if(is.null(pred$strata)){
    pred$strata <- 'trt=.'
  }
  pred$surv <- gFunction(conf_type)(pred$surv)

  trans_St <- pred$surv
  names(trans_St) <- paste0('time_(', pred$strata, ')_(', pred$time, ')')
  attr(trans_St, 'conf_type') <- conf_type

  trans_St
}

