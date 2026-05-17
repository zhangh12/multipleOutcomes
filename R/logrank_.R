#' Creating Objects of Logrank Test
#' 
#' @description
#' `logrank_` is a wrapper function `survival::coxph` to create an object to be 
#' passed into `jointCovariance`, the main function of this package through its 
#' argument `...`. Logrank test is the score test under the proportional hazards 
#' regression model. The object defines how a logrank test would be computed. 
#' 
#' @param formula see `formula` in `survival::coxph`.
#' @param ties character string specifying the method for tie handling. One of
#' `"efron"` (default), `"breslow"`, or `"exact"`. Passed through to
#' `survival::coxph`.
#' @param data_index integer. Index of the data frame in the `data` argument of
#' `jointCovariance` to be used when computing testing statistic of logrank 
#' test.
#' 
#' @details
#' 
#' Not all arguments of `survival::coxph` are supported in `logrank_` due to the 
#' complexity in handling environment and scope, which is particularly difficult 
#' for arguments like `weights`, `subset`, etc. 
#' 
#' @export
logrank_ <- function(formula, ties = c('efron', 'breslow', 'exact'), data_index = 1){
  
  ties <- match.arg(ties)
  data_index <- validate_data_index(data_index)

  structure(
    list(
      engine = "longrank",
      formula = formula,
      ties = ties,
      data_index = data_index,
      id_col = 'pid'
    ),
    class = c("jc_spec_logrank", "jc_spec")
  )
}

