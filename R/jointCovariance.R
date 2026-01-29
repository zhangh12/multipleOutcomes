#' Fitting Regression Models for Multiple Outcomes and Returning the Matrix of
#' Covariance
#' @description
#' `jointCovariance` can fit different types of models for multiple outcomes 
#' simultaneously and return model parameters and variance-covariance matrix 
#' for further analysis. 
#'
#' @param ... objects returned by `glm_()`, `coxph_()`, `logrank_()`, `gee_()` 
#' and `gmm_()`. 
#' @param data a data frame if all models are fitted on the same dataset;
#' otherwise a list of data frames for fitting models in `...`. Note that a
#' dataset can be used to fit multiple models, thus, `length(data)` is unnecessary
#' to be equal to the number of models in `...`. The row names in a data frame
#' are treated as sample IDs. Consequently, for any two records in different
#' data frames that correspond to the same sample, their row names should be
#' consistent.
#' @param nboot non-zero integer if bootstrap is adopted. By default 0.
#' @param compute_cov logic. If \code{TRUE} and `nboot > 0`, 
#' empirical covariance matrix is computed using bootstrap estimate and 
#' returned. Bootstrap estimate will be abandoned. If 
#' \code{FALSE}, bootstrap estimate will be returned and no empirical covariance 
#' matrix is computed. 
#' @param seed random seed when generate bootstrap data. 
#'
#' @return It returns an object of class "jointCovariance", which is a list
#' containing the following components:
#' \tabular{ll}{
#' \code{coefficients} \tab an unnamed vector of coefficients of all fitted models.
#' Use `id_map` for variable mapping. \cr
#' \tab \cr
#' \code{mcov} \tab a unnamed matrix of covariance of `coefficients`. Use `id_map`
#' for variable mapping. \cr
#' \tab \cr
#' \code{id_map} \tab a list mapping the elements in `coefficients` and `mcov` to
#' variable names. \cr
#' \tab \cr
#' \code{n_shared_sample_sizes} \tab a matrix of shared sample sizes between datasets
#' being used to fit the models. \cr
#' \tab \cr
#' \code{call} \tab the matched call.\cr
#' }
#' @export
#'
#' @examples
#' ## More examples can be found in the vignettes.
#' library(survival)
#' library(mvtnorm)
#' library(tidyr)
#' genData <- function(seed = NULL){
#'   
#'   set.seed(seed)
#'   n <- 200
#'   sigma <- matrix(c(1, .6, .6, 1), 2)
#'   x <- rmvnorm(n, sigma = sigma)
#'   z1 <- rbinom(n, 1, .6)
#'   z2 <- rnorm(n)
#'   gam <- c(.1, -.2)
#'   trt <- rbinom(n, 1, .5)
#'   
#'   bet <- c(-.2,.2)
#'   y <- -.5+x %*% bet + z1 * .3 - z2 * .1 + .1 * trt-.1 * rnorm(n)
#'   death <- rbinom(n, 1, .8)
#'   id <- 1:n
#'   data.frame(
#'     y = y, trt = trt, 
#'     z1 = z1, z2 = z2, 
#'     x1 = x[, 1], x2 = x[, 2], 
#'     death, id)
#'   }
#'   
#'   dat1 <- genData(seed = 31415926)
#'   
#'   ## create a dataset with repeated measurements x
#'   dat2 <- dat1 %>% pivot_longer(c(x1, x2), names_to='tmp', values_to='x') %>% 
#'   dplyr::select(x, trt, id) %>% as.data.frame()
#'   
#'   fit <- 
#'     jointCovariance(
#'     logrank_(Surv(time=y, event=death) ~ trt, data_index = 1),
#'     glm_(z1 ~ trt, family = 'binomial', data_index = 1), 
#'     glm_(z2 ~ trt, family = 'gaussian', data_index = 1), 
#'     gee_(x ~ trt, id = 'id', 
#'          family = 'gaussian', corstr = 'independence', data_index = 2), 
#'     data = list(dat1, dat2))
#'  
#'  fit
#'  
#'  ## to use bootstrap, set (large) nboot. nboot = 100 in this example is 
#'  ## too small. In this example, nboot = 1000 would give pretty close mcov
#'  bfit <- 
#'     jointCovariance(
#'     logrank_(Surv(time=y, event=death) ~ trt, data_index = 1),
#'     glm_(z1 ~ trt, family = 'binomial', data_index = 1), 
#'     glm_(z2 ~ trt, family = 'gaussian', data_index = 1), 
#'     gee_(x ~ trt, id = 'id', 
#'          family = 'gaussian', corstr = 'independence', data_index = 2), 
#'     data = list(dat1, dat2), nboot = 100)
#'  
#'  bfit
#'  
jointCovariance <- function(..., data, nboot = 0, compute_cov = TRUE, seed = NULL){
    
    UseMethod('jointCovariance')
    
  }


