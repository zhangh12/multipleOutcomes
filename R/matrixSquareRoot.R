
# Compute matrix A so that AA = mat
matrixSquareRoot <- function(mat){
  ei <- eigen(mat)
  v <- ei$vectors
  d <- diag(sqrt(ei$values), nrow(v), ncol(v))
  v %*% d %*% t(v)
}
