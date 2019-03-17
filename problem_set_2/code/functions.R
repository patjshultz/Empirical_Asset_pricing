pch <- function(x){
  (x[2:nobs]/x[1:(nobs-1)])
}

get_bond_prices <- function(Pi, mu, sigma, beta, gamma){
  # calculate lambdas
  lambda_1 <- mu + sigma 
  lambda_2 <- mu - sigma
  
  # calculate bond prices
  prices <- beta * (Pi[, 1] * lambda_1^(-gamma) + Pi[, 2] * lambda_2^(-gamma))
  
  # return a vector of returns. Each element corresponds to the price in that state
  return(prices)
}