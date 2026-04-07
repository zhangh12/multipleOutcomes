#' Creating Objects of Kaplan-Meier Curve
#' 
#' @description
#' `km_` is a wrapper function creating an object of Kaplan-Meier curve to be 
#' passed into `jointCovariance`, the main function of this package through its 
#' argument `...`. The object defines how a Kaplan-Meier curve would be fitted. 
#' 
#' @param formula a formula created by `survival::Surv()`. 
#' @param times numeric vector of time. Survival probabilities at `times` are 
#' computed (with g-transformation defined by `conf_type`). 
#' @param conf_type character. Type of confidence interval. It must be one of 
#' `"log"`, `"log-log"`, `"plain"`, `"logit"`, or `"arcsin"`. 
#' @param data_index integer. Index of the data frame in the `data` argument of
#' `jointCovariance` to be used when fitting a generalized linear model. 
#' 
#' @details
#' 
#' Usually, g-transformation is applied to the survival probability `S(t)` 
#' to obtain pointwise confidence interval of a Kaplan-Meier curve. This 
#' can be achieved by specifying `conf_type`. For identity transformation, 
#' use `conf_type = "plain"`. 
#' 
#' This function can only be used with `jointCovariance` when the bootstrap 
#' method is used to estimate variance-covariance matrix of multiple outcome 
#' models.  
#' 
#' @export
km_ <- function(formula, times = NULL, conf_type, data_index = 1){
  
  if(!(conf_type %in% c('log','log-log','plain', 'logit', 'arcsin'))){
    stop("conf.type takes value from 'log','log-log','plain', 'logit', 'arcsin'")
  }
  
  structure(
    list(
      engine = "km",
      formula = formula, 
      times = times, 
      conf_type = conf_type,
      data_index = data_index,
      id_col = 'pid'
    ),
    class = c("jc_spec_km", "jc_spec")
  )
}


