# functions for factor model analysis
return_ols_stats <- function(y, model){
 
   if(model == "CAPM"){
    x <- RX
    fit <- summary(lm(y~x))  
    
    # estimates of alpha and beta (and t-state)
    alpha <- fit$coefficients["(Intercept)", "Estimate"]
    t_alpha <- fit$coefficients["(Intercept)", "t value"]
    beta <- fit$coefficients["x", "Estimate"]
    t_beta <- fit$coefficients["x", "t value"]
    
    # return residuals and estimates
    estimates <- c(alpha, t_alpha, beta, t_beta)
    return_list <- list(estimates, fit$residuals)
    
    names(return_list) <- c("estimates", "residuals")
    return(return_list$estimates)
  } else if (model == "FF3") {
    fit <- summary(lm(y~ RX + SMB + HML))
    
    # estimates of alpha and b, s, h
    alpha <- fit$coefficients["(Intercept)", "Estimate"]
    t_alpha <- fit$coefficients["(Intercept)", "t value"]
    b <- fit$coefficients["RX", "Estimate"]
    t_b <- fit$coefficients["RX", "t value"]
    s <- fit$coefficients["SMB", "Estimate"]
    t_s <- fit$coefficients["SMB", "t value"]
    h <- fit$coefficients["HML", "Estimate"]
    t_h <- fit$coefficients["HML", "t value"]
    
    
    
    # return residuals and estimates
    estimates <- c(alpha, t_alpha, b, t_b, s, t_s, h, t_h)
    return_list <- list(estimates, fit$residuals)
    return(estimates)
  }


}
return_ols_residuals <- function(y, model){
  if(model == "CAPM"){
    x <- RX
    fit <- summary(lm(y~x))  
    return(fit$residuals)
  } else if (model == "FF3") {
    fit <- summary(lm(y~ RX + SMB + HML))
    return(fit$residuals)
  }
}

GRS <- function(alphas, residuals, factors){
  # get number of observations and number of factors
  factors <- as.matrix(factors)
  nobs <- nrow(factors)
  nfactors <- ncol(factors)
  
  # calculate the residual vcov matrix
  Sigma <- (t(residuals) %*% residuals) / (nobs - nfactors - 1)
 
  # generate mean matrix of factors returns
  mu_f <- colMeans(factors) # mean of factors
  Fmat <- factors # matrix of factors
  mu_F <- matrix(data = mu_f, nrow = nobs, ncol = nfactors, byrow = T)
  
  # calculate vcov of factors
  Omega <- (t(Fmat - mu_F) %*% (Fmat - mu_F)) / (nobs - 1)
  
  # finally, calculate GRS test statistic
  bias_adjustment <- (nobs/N) %*% ((nobs - N - L)/(nobs - L - 1))
  sum_square_alphas <- t(alphas) %*% solve(Sigma) %*% alphas
  GRS <- bias_adjustment %*% (sum_square_alphas/(1 + t(mu_f) %*% solve(Omega) %*% mu_f))
  return(GRS)
}
ts_regressions <- function(y){
  # leave out intercept  
  normal_mat <- solve(t(factors)%*%factors)
  beta_hat <- normal_mat %*% t(factors) %*% y
  
}