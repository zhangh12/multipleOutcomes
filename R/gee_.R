#' Creating Objects of Generalized Estimation Equation Model
#' 
#' @description
#' `gee_` is a wrapper function of `gee::gee` to create an object to be passed 
#' into `jointCovariance`, the main function of this package through its 
#' argument `...`. The object defines how a GEE model would be fitted. 
#' 
#' This package does not import the package `gee`. Instead, codes of `gee` are 
#' modified and integrated to compute score and information matrix. Thus, users 
#' does not need to install the package `gee` to use this package.  
#' 
#' @param formula see `formula` in `gee::gee`. 
#' @param family see `family` in `gee::gee`. 
#' @param corstr see `corstr` in `gee::gee`. 
#' @param R see `R` in `gee::gee`. 
#' @param b see `b` in `gee::gee`. 
#' @param Mv see `Mv` in `gee::gee`. 
#' @param data_index integer. Index of the data frame in the `data` argument of
#' `jointCovariance` to be used when fitting a GEE model. 
#' 
#' @details
#' 
#' Not all arguments of `stats::gee` are supported in `gee_` due to the 
#' complexity in handling environment and scope, which is particularly difficult 
#' for arguments like `subset`, etc. 
#' 
#' @export
gee_ <- function(formula, family, corstr, R = NULL, b = NULL, Mv = 1, data_index = 1) {
  structure(
    list(
      engine = "gee",
      formula = formula,
      family  = family,
      corstr = corstr, 
      R = R, 
      b = b,
      Mv = Mv,
      data_index = data_index,
      id_col = 'pid'
    ),
    class = c("jc_spec_gee", "jc_spec")
  )
}


