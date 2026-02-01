#' Creating Objects of Mixed Models for Repeated Measures
#' 
#' @description
#' `mmrm_` is a wrapper function of `mmrm::mmrm` to create an object to be passed 
#' into `jointCovariance`, the main function of this package through its 
#' argument `...`. The object defines how a MMRM model would be fitted. 
#' 
#' 
#' @param formula see `formula` in `mmrm::mmrm`.
#' @param covariance see `covariance` in `mmrm::mmrm`. 
#' @param reml see `reml` in `mmrm::mmrm`. 
#' @param control see `control` in `mmrm::mmrm`. 
#' @param ... see `...` in `mmrm::mmrm`.
#' @param data_index integer. Index of the data frame in the `data` argument of
#' `jointCovariance` to be used when fitting a GEE model. 
#' 
#' @details
#' 
#' The argument `weights` of `mmrm::mmrm` is supported in `mmrm_` due to the 
#' complexity in handling environment and scope. 
#' 
#' Please always refer to help document of `mmrm::mmrm` before using `mmrm_`. 
#' For example, time variable and observation ID must be factor variables in 
#' some cases, otherwise error may be prompted. Users can call `mmrm::mmrm` 
#' using the same arguments being passed to `mmrm_` to check validity. 
#' 
#' @export
mmrm_ <- function(formula, covariance = NULL, reml = TRUE, 
                  control = mmrm::mmrm_control(...), ..., 
                  data_index){
  
  if(!missing(control) && length(list(...)) > 0){
    stop('Fine-grained control can be specified either via ... or via control, 
         not both. See the Details section in ?mmrm. ')
  }
  
  if(!inherits(control, 'mmrm_control')){
    stop('control must be an object of class mmrm_control 
         (created by mmrm::mmrm_control()). ', 
         call. = FALSE)
  }
  
  if(!is.list(control$optimizers) || length(control$optimizers) < 1L){
    stop("control$optimizes must be a non-empty list (as produced by mmrm::mmrm_control()).",
         call. = FALSE)
  }
  
  if(control$method %in% c("Kenward-Roger", "Kenward-Roger-Linear") && !isTRUE(reml)){
    stop("Kenward-Roger only works for REML.", call. = FALSE)
  }
  
  ## otherwise mmrm::component(object, 'score_per_subject') doesn't return scores
  if(!identical(control$method, 'Residual')){
    control$method <- 'Residual'
    message('method in mmrm_control is re-set to be "Residual"')
  }
  
  if(!identical(control$vcov, 'Empirical')){
    control$vcov <- 'Empirical'
    message('vcov in mmrm_control is re-set to be "Empirical"')
  }
  
  structure(
    list(
      engine = "mmrm",
      formula = formula,
      covariance = covariance, 
      reml = reml,
      control = control,
      data_index = data_index,
      id_col = 'pid'
    ),
    class = c("jc_spec_mmrm", "jc_spec")
  )
}


