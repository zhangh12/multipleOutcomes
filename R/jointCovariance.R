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
#' are not treated as subject IDs. Instead, all data frame should consist of 
#' a column `pid` as subject IDs. For any two records in different
#' data frames that correspond to the same subject, their values in `pid` should 
#' be consistent. For data frames to be used by GEE, `pid` defines clusters. 
#' See `id` and the Details section of `gee:gee`. 
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
#'   n <- 300
#'   sigma <- matrix(.7, 4, 4)
#'   diag(sigma) <- 1
#'   v <- rmvnorm(n, sigma = sigma)
#'   x1 <- v[, 1]
#'   x2 <- v[, 2]
#'   z1 <- (v[, 3] > 0) + 0
#'   z2 <- v[, 4]
#'   
#'   trt <- rbinom(n, 1, .5)
#'   
#'   bet <- c(-.3,.3)
#'   y <- -log(runif(n))/
#'     exp(-.3 * x1 + .3 * x2 + z1 * .5 - z2 * .3 + .1 * trt + rnorm(n))
#'   
#'   z1[sample.int(n, 50)] <- NA
#'   z2[sample.int(n, 50)] <- NA
#'   x1[sample.int(n, 50)] <- NA
#'   x2[sample.int(n, 50)] <- NA
#'   death <- ifelse(y > 2, 0, 1)
#'   y[y > 2] <- 2
#'   
#'   pid <- paste0('pid-', 1:n)
#'   ret <- data.frame(
#'     y = y, trt = trt, 
#'     z1 = z1, z2 = z2, 
#'     x1 = x1, x2 = x2, 
#'     death, pid)
#'   ret
#' }
#' 
#' dat1 <- genData()
#' 
#' ## create a dataset with repeated measurements x
#' dat2 <- dat1 %>% pivot_longer(c(x1, x2), names_to='visit', values_to='x') %>% 
#'   dplyr::select(x, trt, visit, pid) %>% as.data.frame()
#' 
#' dat2$visit <- as.factor(dat2$visit)
#' dat2$pid <- as.factor(dat2$pid)
#' 
#' fit <- jointCovariance(
#'   coxph_(Surv(time = y, event = death) ~ trt, data_index = 1),
#'   logrank_(Surv(time = y, event=death) ~ trt, data_index = 1),
#'   glm_(z1 ~ trt, family = 'binomial', data_index = 1), 
#'   glm_(z2 ~ trt, family = 'gaussian', data_index = 1), 
#'   mmrm_(x ~ trt + us(visit | pid), reml = TRUE, data_index = 2), 
#'   gee_(x ~ trt, family = 'gaussian', corstr = 'independence', data_index = 2), 
#'   data = list(dat1, dat2))
#' 
#' fit
#' 
#' 
#' bfit <-jointCovariance(
#'   coxph_(Surv(time=y, event=death) ~ trt, data_index = 1),
#'   logrank_(Surv(time=y, event=death) ~ trt, data_index = 1),
#'   glm_(z1 ~ trt, family = 'binomial', data_index = 1),
#'   glm_(z2 ~ trt, family = 'gaussian', data_index = 1),
#'   mmrm_(x ~ trt + us(visit | pid), reml = TRUE, data_index = 2),
#'   gee_(x ~ trt, family = 'gaussian', corstr = 'independence', data_index = 2),
#'   data = list(dat1, dat2), nboot = 10)
#'
#' summary(bfit)
#'
#' ## km_() and quantile_() require nboot > 0 because they have no
#' ## closed-form score. compute_cov is forced to FALSE for km_().
#' ## When all models share one dataset, `data_index` and `list(...)`
#' ## can be omitted.
#' kfit <- jointCovariance(
#'   km_(Surv(time = y, event = death) ~ trt, conf_type = 'log',
#'       times = c(0.5, 1, 1.5)),
#'   glm_(z1 ~ trt, family = 'binomial'),
#'   data = dat1, nboot = 30, seed = 1)
#'
#' qfit <- jointCovariance(
#'   quantile_(y ~ trt, probs = c(0.25, 0.5, 0.75)),
#'   glm_(z2 ~ trt, family = 'gaussian'),
#'   data = dat1, nboot = 30, seed = 1)
#'
jointCovariance <- function(..., data, nboot = 0, compute_cov = TRUE, seed = NULL){
    
    UseMethod('jointCovariance')
    
  }


