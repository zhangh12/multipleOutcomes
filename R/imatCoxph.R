

# for HR in coxph, use mean = TRUE
# for logrank, use mean = FALSE
imatCoxph <- function(model, mean = TRUE){
  imat <- coxph.detail(model)$imat
  n <- nrow(matrix(resid(model, type = 'score')))
  if('numeric' %in% class(imat)){
    imat <- sum(imat) / n
  }else{
    n <- nrow(resid(model, type = 'score'))
    imat <- rowSums(imat, dims = 2) / n
  }
  if(!mean){
    imat <- imat * n
  }
  
  imat
}
