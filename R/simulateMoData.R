#' Generating Data for Simulation and Testing
#' @description
#' `simulateMoData` generates data for simulation and testing purposes.
#' 
#' @param n an integer for total sample size of a randomized control trial of 
#' two arms. 
#' @param hr hazard ratio of treatment.
#' @param seed random seed. By default `NULL` for no seed being specified.
#' 
#' @export
simulateMoData <- function(n = 500, hr = 0.8, seed = NULL){
  
  set.seed(seed)
  
  rep_sigma <- matrix(.6, 3, 3) # correlation of repeated measurements
  diag(rep_sigma) <- 1
  repm <- data.frame(rmvnorm(n, sigma = rep_sigma))
  colnames(repm) <- paste0('repm', 1:ncol(repm))
  pmax_repm <- apply(repm, 1, max)
  
  cont <- rnorm(n) # continuous outcome
  bin <- rbinom(n, 1, .5) # binary outcome
  err <- rnorm(n, sd = .01)
  trt <- rbinom(n, 1, .5) # fully randomized arm assignment
  tte <- -log(runif(n)) / .05 / exp(log(hr) * trt + 5 * (.8 * cont + 2.5 * bin + 0 * pmax_repm) + err)
  tte <- tte^.35
  event <- rbinom(n, 1, .9) # event or censored
  tte <- tte * ifelse(event == 1, 1, runif(n))
  
  follow_up_time <- 100
  event[tte > follow_up_time] <- 0
  tte <- pmin(tte, follow_up_time)
  id <- paste0('uid-', 1:n)
  dat1 <- data.frame(cont, bin, tte, event, trt, id)
  dat2 <- data.frame(repm, trt, id) %>% 
    pivot_longer(
      cols = all_of(paste0('repm', 1:3)), 
      names_to = 'time', 
      values_to = 'repm') %>% 
    dplyr::select(.data$repm, .data$time, .data$trt, .data$id) %>% 
    as.data.frame()
  
  invisible(list(dat1 = dat1, dat2 = dat2))
  
}

