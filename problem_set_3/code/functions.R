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
return_ols_residuals <- function(y){
  # CAPM regression
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
  return(return_list$residuals)
}

GRS <- function(alphas, residuals){
  Sigma <- (t(residuals) %*% residuals) / (nobs - nfactors - 1)
  mu_f <- mean(RX) # mean of factors
  Fmat <- RX # matrix of factors
  Omega <- (t(Fmat - mu_f) %*% (Fmat - mu_f)) / (nobs - 1)
  
  bias_adjustment <- (nobs/N) %*% ((nobs - N - L)/(nobs - L - 1))
  sum_square_alphas <- t(alphas) %*% solve(Sigma) %*% alphas
  GRS <- bias_adjustment %*% (sum_square_alphas/(1 + mu_f %*% (1/Omega) %*% mu_f))
  return(GRS)
}