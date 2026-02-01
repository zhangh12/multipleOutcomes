#' Creating Objects of Proportional Hazards Regression Model
#' 
#' @description
#' `coxph_` is a wrapper function of `survival::coxph` to create an object to be 
#' passed into `jointCovariance`, the main function of this package through its 
#' argument `...`. The object defines how a proportional hazard model would be 
#' fitted. 
#' 
#' @param formula see `formula` in `survival::coxph`. 
#' @param data_index integer. Index of the data frame in the `data` argument of
#' `jointCovariance` to be used when fitting a proportional hazards model. 
#' 
#' @details
#' 
#' Not all arguments of `survival::coxph` are supported in `coxph_` due to the 
#' complexity in handling environment and scope, which is particularly difficult 
#' for arguments like `weights`, `subset`, etc. 
#' 
#' @export
coxph_ <- function(formula, data_index) {
  structure(
    list(
      engine = "coxph",
      formula = formula,
      data_index = data_index,
      id_col = 'pid'
    ),
    class = c("jc_spec_coxph", "jc_spec")
  )
}

