#' Prognostic Variables Assisted Treatment Effect Detection
#' @description
#' `pated` is a wrapper function of `multipleOutcomes` for testing treatment effect 
#' in randomized clinical trials. It assumes that prognostic variables are fully 
#' randomized. This assumption can help enhancing statistical power of conventional 
#' approaches in detecting the treatment effect. Specifically, the sensitivity 
#' of the conventional models specified in `...` are improved by `pated`. 
#' 
#' @importFrom stringr str_extract
#' @importFrom ggpubr ggarrange
#' @importFrom stats rbinom rnorm runif sd time var
#' @importFrom dplyr row_number first case_when bind_rows filter arrange desc 
#' @importFrom ggplot2 geom_hline geom_vline scale_color_manual theme element_blank
#' @importFrom rlang .data
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
#' @param nboot non-zero integer if bootstrap is adopted. By default 0.
#' @param compute_cov logic. If \code{TRUE}, empirical covariance matrix is computed 
#' using bootstrap estimate and returned. Bootstrap estimate will be abandoned. If 
#' \code{FALSE}, bootstrap estimate will be returned and no empirical covariance 
#' matrix is computed. 
#' @param seed random seed when generate bootstrap data. 
#' @param transform character. Now only supports \code{"identity"}. 
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
pated <- 
  function(
    ..., 
    data, 
    family = NULL, 
    data_index = NULL, 
    nboot = 0, 
    compute_cov = TRUE, 
    seed = NULL, 
    transform = 'identity'){
    
  fit <- 
    multipleOutcomes(
      ..., 
      data = data, 
      family = family, 
      data_index = data_index,
      nboot = nboot, 
      compute_cov = compute_cov, 
      seed = seed)

  if(nboot > 0){
    fml <- parseTreatmentVariableFromCall(...)
  }else{
    fml <- parseTreatmentVariableFromFormula(...)
  }

  #id <- sapply(1:length(fit$id_map), function(i) fit$id_map[[i]][fml$arm[i]])
  id <- 
    lapply(
      seq_along(fit$id_map), 
      function(i){
        imap <- fit$id_map[[i]]
        arm <- fml$arm[i]
        if(arm %in% names(imap)){
          imap[arm]
        }else{
          if(any(grepl(str_glue('^{arm}_\\d+%$'), names(imap)))){
            imap[grepl(str_glue('^{arm}_\\d+%$'), names(imap))]
          }else{
            imap[startsWith(str_extract(names(imap), "(?<=time_\\().*?(?=\\)_\\()"), arm)]
          }
        }
      }
    )
  id1 <- id[[1]]
  id2 <- unlist(id[-1])
  Delta <- coef(fit)[id1]
  delta <- coef(fit)[id2]
  mcov <- vcov(fit)
  if(!is.null(mcov)){
    mcov22 <- mcov[id2, id2, drop = FALSE]
    mcov21 <- mcov[id2, id1, drop = FALSE]
    var11 <- diag(mcov[id1, id1, drop = FALSE])
  }else{
    mcov22 <- cov(fit$bootstrap_estimate[, id2, drop = FALSE])
    mcov21 <- cov(fit$bootstrap_estimate[, id2, drop = FALSE], fit$bootstrap_estimate[, id1, drop = FALSE])
    var11 <- apply(fit$bootstrap_estimate[, id1, drop = FALSE], 2, var)
    if(!('kmMO' %in% fml$func)){
      mcov <- cov(fit$bootstrap_estimate)
    }
  }
  opt_c <- - solve(mcov22) %*% mcov21

  estimate <- Delta + as.vector(t(opt_c) %*% delta)
  stderr <- sqrt(var11 + diag(t(opt_c) %*% mcov21) + diag(t(mcov21) %*% opt_c) + diag(t(opt_c) %*% mcov22 %*% opt_c))

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

  if(is.null(family)){
    family <- gsub('MO', '', fml$func)
    family <- c(rep(family[1], length(id1)), rep(family[-1], times = fml$n_terms[-1]))
  }
  
  nonconfounder <-
    data.frame(
      term = c(rep(fml$outcome[1], length(id1)), rep(fml$outcome[-1], times = fml$n_terms[-1])),
      family = family,
      estimate = coef(fit)[c(id1, id2)],
      stderr = sqrt(c(var11, diag(mcov22))),
      pvalue = pchisq((coef(fit)[c(id1, id2)] / sqrt(c(var11, diag(mcov22))))^2, df = 1, lower.tail = FALSE),
      method = rep(c('Standard', 'Prognostic'), times = c(length(id1), length(id2))),
      corr = NA
    )
  if(!is.null(mcov) && length(id1) == 1){
    nonconfounder$corr <- cov2cor(mcov)[c(id1, id2), id1]
  }

  ret <- rbind(treatment, nonconfounder)
  ret$method <- paste0(ret$method, ifelse(ret$method == 'Prognostic' & ret$pvalue < .05, '*', ''))
  if(!is.null(mcov) && length(id1) == 1){
    attr(ret, 'Rel. Eff.') <- mcov[id1, id1] / stderr^2
  }
  
  if(fml$func[1] %in% 'kmMO'){
    
    conf.type <- fml$arg[fml$func %in% 'kmMO'][1]
    pated_res <- 
      treatment %>% 
      dplyr::select(estimate, stderr) %>% 
      mutate(LCI = gInverseFunction(conf.type)(estimate - 1.96 * stderr)) %>%
      mutate(UCI = gInverseFunction(conf.type)(estimate + 1.96 * stderr)) %>% 
      mutate(estimate = gInverseFunction(conf.type)(estimate)) %>% 
      dplyr::select(dplyr::all_of(c("estimate", "LCI", "UCI")))
    
    km_res <- 
      nonconfounder %>% 
      dplyr::filter(.data$method %in% 'Standard') %>% 
      dplyr::select(estimate, stderr) %>% 
      mutate(LCI = gInverseFunction(conf.type)(estimate - 1.96 * stderr)) %>%
      mutate(UCI = gInverseFunction(conf.type)(estimate + 1.96 * stderr)) %>% 
      mutate(estimate = gInverseFunction(conf.type)(estimate)) %>% 
      dplyr::select(dplyr::all_of(c("estimate", "LCI", "UCI")))
    
    pated_curve <- createKaplanMeierCurve(pated_res, str_glue('PATED ({conf.type})'), transform)
    km_curve <- createKaplanMeierCurve(km_res, str_glue('KM ({conf.type})'), transform)
    comp_ci <- comparePointwiseConfidenceIntervalWidth(pated_res, km_res, transform)
    
    #plot(ggarrange(ggarrange(km_curve, pated_curve, ncol = 2), ggarrange(comp_ci[[1]], comp_ci[[2]], ncol = 2), nrow = 2))
    plot(ggarrange(km_curve, pated_curve, ncol = 2))
    
    attr(ret, 'pated curve') <- pated_curve
    attr(ret, 'km curve') <- km_curve
    attr(ret, 'comp ci') <- comp_ci
    attr(ret, 'conf.type') <- conf.type
  }
  class(ret) <- c('pated', class(ret))
  ret

}

#' Plot PATED Analysis Results
#'
#' @param x an object returned from \code{pated()}. 
#' @param ... currently not supported.
#' 
#' @returns \code{NULL}
#' @export
#'
plot.pated <- function(x, ...){
  
  x_plot <- x %>%
    mutate(term = ifelse(row_number() == 2 & .data$term == first(.data$term),
                         paste0(" ", .data$term),
                         .data$term)) %>%
    mutate(
      rowid = row_number(),
      z = .data$estimate / .data$stderr,
      group = case_when(
        rowid == 1 ~ "PATED",
        rowid == 2 ~ "Unadjusted",
        TRUE ~ "Prognostic"
      )
    )
  
  x_plot <- bind_rows(
    x_plot %>% filter(.data$rowid <= 2),
    x_plot %>% filter(.data$rowid > 2) %>% arrange(desc(abs(.data$corr)))
  )
  
  x_plot$variable <- factor(x_plot$term, levels = rev(x_plot$term))
  
  p <- 
    ggplot(x_plot, aes(x = .data$z, y = .data$variable, color = .data$group)) +
    geom_point(size = 3) +
    geom_hline(yintercept = length(x_plot$variable) - 2 + 0.5,
               linetype = "dashed", color = "gray80") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = c(-1.96, 1.96), linetype = "dashed", color = "gray60") +
    scale_color_manual(values = c("PATED" = "red",
                                  "Unadjusted" = "blue",
                                  "Prognostic" = "gray40"),
                       breaks = c("PATED", "Unadjusted", "Prognostic")) +
    labs(x = "Z value (variable ~ arm)", y = "Variable") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  plot(p)
  
  invisible(NULL)
  
}
