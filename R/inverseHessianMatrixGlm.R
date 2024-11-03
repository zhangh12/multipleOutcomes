

inverseHessianMatrixGlm <- function(family, design_matrix, model = NULL){
  n_sam <- nrow(design_matrix)
  n_par <- ncol(design_matrix)
  hess <- matrix(0, n_par, n_par)
  if(family == 'gaussian'){
    hess <- -t(design_matrix / n_sam) %*% design_matrix
  }else{
    hess <- t(design_matrix / n_sam) %*% (design_matrix * (-fitted(model) * (1 - fitted(model))))
  }
  
  ret <- solve(hess)
  attr(ret, 'n') <- n_sam
  
  invisible(ret)
}
