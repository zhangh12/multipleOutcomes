
#' @export
logrankMO <- function(formula, data = NULL, ties = c('efron', 'breslow', 'exact')){
  
  if(is.null(data)){
    return(NULL)
  }
  
  ties <- match.arg(ties)
  fit <- coxph(formula, data, ties = ties, model = TRUE, iter = 0)
  score <- as.matrix(resid(fit, type = 'score'))
  imat <- imatCoxph(fit, mean = FALSE)
  theta <- solve(matrixSquareRoot(imat), colSums(score))
  names(theta) <- names(coef(fit))
  theta
  
}
