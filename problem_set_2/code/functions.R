pch <- function(x, nobs){
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


get_beta <- function(rho, mu, sigma, alpha, target_R = 1.05){
  # parameters set up
  phi <- (1+rho)/2
  lambda1 <- mu + sigma
  lambda2 <- mu - sigma 
  
  # calculate bond prices
  b1 <- phi * lambda1^alpha + (1 - phi) * lambda2^alpha
  b2 <- (1 - phi) * lambda1^alpha + phi * lambda2^alpha
  
  # calculate beta 
  beta <- (1/(2*target_R)) * ((1/b1) + (1/b2))
  return(beta)
}