
#' glm_fit is object returned from glm()
inverseHessianMatrixGlm <- function(glm_fit){
  
  V <- stats::vcov(glm_fit)
  disp <- summary(glm_fit)$dispersion
  n <- stats::nobs(glm_fit)
  ret <- -solve(disp * V * n)
  
  attr(ret, 'n') <- n
  
  invisible(ret)
}
