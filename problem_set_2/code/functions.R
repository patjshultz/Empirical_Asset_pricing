generate_trans_mat <- function(rho){
  diagonal <- (1 + rho) / 2
  off_diag <- (1 - rho) / 2
  P <- matrix(data = c(diagonal, off_diag, diagonal, off_diag), nrow = 2)
  return(P)
}

pch <- function(x){
  (x[2:nobs]/x[1:(nobs-1)])
}

