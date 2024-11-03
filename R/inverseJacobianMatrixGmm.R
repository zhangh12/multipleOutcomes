

inverseJacobianMatrixGmm <- function(func, u, data){
  func2 <- function(eval_at, data){
    apply(func(eval_at, data), 2, sum)
  }
  
  n_sam <- nrow(data)
  jac <- numDeriv::jacobian(func2, u, data = data) / n_sam
  ret <- solve(jac)
  attr(ret, 'n') <- n_sam
  
  invisible(ret)
}

