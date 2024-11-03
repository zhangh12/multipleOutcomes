#' @export
multipleOutcomes.default <- 
  function(
    ..., 
    data, 
    family = NULL, 
    data_index = NULL, 
    nboot = 0, 
    compute_cov = TRUE, 
    seed = NULL, 
    score_epsilon = 1e-6){
  
  if(nboot > 0){
    fit <- bootstrapMultipleOutcomes(..., data = data, data_index = data_index, nboot = nboot, compute_cov = compute_cov, seed = seed)
  }else{
    fit <- asymptoticMultipleOutcomes(..., data = data, family = family, data_index = data_index, score_epsilon = score_epsilon)
  }

  fit$call <- match.call()
  class(fit) <- 'multipleOutcomes'
  invisible(fit)

}
