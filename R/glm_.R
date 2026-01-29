#' Creating Objects of Generalized Linear Models
#' 
#' @description
#' `glm_` is a wrapper function of `stats::glm` to create an object to be passed 
#' into `jointCovariance`, the main function of this package through its 
#' argument `...`. The object defines how a GLM model would be fitted. 
#' 
#' @param formula see `formula` in `stats::glm`. 
#' @param family currently supports `"gaussian"` or `"binomial"`. Other families 
#' are under testing. 
#' @param data_index integer. Index of the data frame in the `data` argument of
#' `jointCovariance` to be used when fitting a generalized linear model. 
#' 
#' @details
#' 
#' Not all arguments of `stats::glm` are supported in `glm_` due to the 
#' complexity in handling environment and scope, which is particularly difficult 
#' for arguments like `weights`, `subset`, etc. 
#' 
#' @export
glm_ <- function(formula, family, data_index = 1) {
  structure(
    list(
      engine = "glm",
      formula = formula,
      family  = family,
      data_index = data_index
    ),
    class = c("jc_spec_glm", "jc_spec")
  )
}


