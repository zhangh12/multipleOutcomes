#' Prognostic Variables Assisted Treatment Effect Detection
#' @description
#' `pated` is a wrapper function of `multipleOutcomes` for testing treatment effect 
#' in randomized clinical trials. It assumes that prognostic variables are fully 
#' randomized. This assumption can help enhancing statistical power of conventional 
#' approaches in detecting the treatment effect. Specifically, the sensitivity 
#' of the conventional models specified in `...` are improved by `pated`. 
#' 
#' @param ... formulas of models to be fitted, or moment functions for gmm. 
#' @param family a character vector of families to be used in the models.
#' All families supported by `multipleOutcomes` are also supported by `pated`. 
#' `family` can be of length 1 if all models are fitted in the same family; 
#' otherwise family should be specified for each of the models in `...`.
#' @param data a data frame if all models are fitted on the same dataset;
#' otherwise a list of data frames for fitting models in `...`. Note that a
#' dataset can be used to fit multiple models, thus, `length(data)` is unnecessary
#' to be equal to the number of models in `...`. The row names in a data frame
#' are treated as sample IDs. Consequently, for any two records in different
#' data frames that correspond to the same sample, their row names should be
#' consistent.
#' @param data_index `NULL` if `data` is a data frame; otherwise, a vector in
#' integer specifying mapping a model in `...` to a data frame in `data` (a list).
#'
#' @return a data frame of testing results.
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
#'
#' }
#'
#' dat1 <- genData(seed = 31415926)
#' 
#' ## create a dataset with repeated measurements x
#' dat2 <- dat1 %>% pivot_longer(c(x1, x2), names_to='tmp', values_to='x') %>% 
#' dplyr::select(x, trt, id) %>% as.data.frame()
#' 
#' fit <- 
#'   pated(
#'     Surv(time=y, event=death) ~ trt,
#'     z1 ~ trt, 
#'     z2 ~ trt, 
#'     x ~ trt, 
#'     family=c('logrank', 'binomial', 'gaussian', 'gee+id+gaussian'), 
#'     data=list(dat1, dat2), data_index = c(1, 1, 1, 2))
#' 
#' fit
#' 
pated <- function(..., family, data, data_index = NULL){
  fit <- multipleOutcomes(..., family = family, data = data, data_index = data_index)

  parseFormula <- function(...){
    formulas <- list(...)
    formulas[sapply(formulas, function(x){!inherits(x, 'formula')})] <- NULL
    outcome <- NULL
    arm <- NULL
    for(fstr in formulas){
      outcome <- c(outcome, trimws(sub("\\s*~.*", "", deparse1(fstr))))
      arm <- c(arm, trimws(sub("^.*?~", "", deparse1(fstr))))
    }
    data.frame(outcome, arm)
  }

  fml <- parseFormula(...)

  id <- sapply(1:length(fit$id_map), function(i) fit$id_map[[i]][fml$arm[i]])
  id1 <- id[1]
  id2 <- id[-1]
  Delta <- coef(fit)[id1]
  delta <- coef(fit)[id2]
  mcov <- vcov(fit)
  opt_c <- as.vector(- solve(mcov[id2, id2, drop = FALSE]) %*% mcov[id2, id1, drop = FALSE])

  estimate <- Delta + sum(delta * opt_c)
  stderr <- sqrt(
    mcov[id1, id1] +
      2 * as.vector(t(opt_c) %*% mcov[id2, id1, drop = FALSE]) +
      as.vector(t(opt_c) %*% mcov[id2, id2, drop = FALSE] %*% opt_c)
  )

  pvalue <- pchisq((estimate / stderr)^2, df = 1, lower.tail = FALSE)
  
  treatment <-
    data.frame(
      term = fml$outcome[1],
      family = 'PATED',
      estimate = estimate,
      stderr = stderr,
      pvalue = pvalue,
      method = 'PATED',
      corr = NA
    )

  nonconfounder <-
    data.frame(
      term = fml$outcome,
      family = family,
      estimate = coef(fit)[id],
      stderr = sqrt(diag(vcov(fit))[id]),
      pvalue = pchisq((coef(fit)[id] / sqrt(diag(vcov(fit)))[id])^2, df = 1, lower.tail = FALSE),
      method = c('Standard', rep('Prognostic', nrow(fml) - 1)),
      corr = cov2cor(mcov)[id, id1]
    )

  ret <- rbind(treatment, nonconfounder)
  ret$method <- paste0(ret$method, ifelse(ret$method == 'Prognostic' & ret$pvalue < .05, '*', ''))
  attr(ret, 'Rel. Eff.') <- mcov[id1, id1] / stderr^2
  ret

}
