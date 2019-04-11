#===============================================================================
# Finance 921: Assignment 3
# Question 1 part b
#===============================================================================
rm(list = ls())
library(ggplot2)
library(reshape2)
source("functions.R")

# load data
returns <- read.csv("../data/returns_bivariate_sort_monthly.csv")[, -1]
size <- read.csv("../data/avg_mkt_cap_bivariate_sort_monthly.csv")[, -1]
beme <- read.csv("../data/vw_beme_bivariate_sort_monthly.csv")[, -1]
FF3 <- read.csv("../data/FF_factors.CSV")[, -1]
Rf <- FF3$RF
xreturns <- returns - Rf
nobs <- nrow(returns)
nportfolios <- ncol(xreturns)

# take logs of data 
average_beme <- log(beme)
average_size <- log(size)

#===============================================================================
# Step 1: Run TS regressions on just FF3
#===============================================================================

ts_regressions <- function(y){return(solve(t(cbind(1, x))%*%cbind(1, x)) %*% t(cbind(1, x)) %*% y)}

# fama french factors plus size and book to market ratio
x <- cbind(FF3$RE, FF3$SMB, FF3$HML)

# time series regression for book-to-market sorted portfolios
estimates <- apply(xreturns, 2, ts_regressions)
est_tab <- rbind(colMeans(xreturns), estimates)
rownames(est_tab) <- c("E(R)", "alpha", "b", "s", "h")

# replicate figure 20.9 of Asset Pricing
plot(est_tab[3, ], est_tab[1, ], xlab = " beta on market", 
     ylab = "excess return", xlim = c(0, 1.5), ylim = c(0, 1.25))

#===============================================================================
# Step 2: Run CS regression for each observation including size i and beme i 
#===============================================================================

# matrix of estimated betas from time series regression
betas <- t(est_tab[3:5, ])

# set up matrices to store estimates in
lambda_mat <- matrix(data = NA, nrow = nobs, ncol = 3)
lambda_mat_SML_HMB <- matrix(data = NA, nrow = nobs, ncol = 5)

alpha_mat <- matrix(data = NA, nrow = nobs, ncol = nportfolios)
alpha_mat_SML_HMB <- matrix(data = NA, nrow = nobs, ncol = nportfolios)

# loop through each observation and calculate the cross sectional regression
for (t in 1:nobs){
  return_t  <- t(as.matrix(xreturns[t, ]))
  size_t <- t(average_size[t, ])
  beme_t <- t(average_beme[t, ])
  
  # run the regression without SMB and HML
  x <- cbind(betas[, 1], size_t, beme_t)
  lambda_mat[t, ] <- solve(t(x)%*%x) %*% t(x) %*% return_t # No intercept in cross sectional regressions!
  alpha_mat[t, ] <- return_t  - betas %*% lambda_mat[t, ]
  
  # run the regressions with SMB and HML
  x <- cbind(betas, size_t, beme_t)  
  lambda_mat_SML_HMB[t, ] <- solve(t(x)%*%x) %*% t(x) %*% return_t # No intercept in cross sectional regressions!
  alpha_mat_SML_HMB[t, ] <- return_t  - x %*% lambda_mat_SML_HMB[t, ]
}

#===============================================================================
# Step 3: Calculate sample estimates of lambda and alpha
#===============================================================================

lambda_hat <- colMeans(lambda_mat)
lambda_hat_SML_HMB <- colMeans(lambda_mat_SML_HMB)

alpha_hat <- colMeans(alpha_mat)
alpha_hat_SML_HMB <- colMeans(alpha_mat_SML_HMB)

#===============================================================================
# Step 4: Calculate standard errors and significance of lambda estimates
#===============================================================================

# compute sample standard deviations of lambdas without additional characteristics
lambda_dev <- matrix(data = NA, nrow = nobs, ncol = 3)
for(t in 1:nobs){
  lambda_dev[t, ] <- (lambda_mat[t, ] - lambda_hat)^2
}
lambda_var <- colSums(lambda_dev)/nobs^2

# compute sample standard deviations of lambdas with additional characteristics
lambda_dev_SML_HMB <- matrix(data = NA, nrow = nobs, ncol = 5)
for(t in 1:nobs){
  lambda_dev_SML_HMB[t, ] <- (lambda_mat_SML_HMB[t, ] - lambda_hat_SML_HMB)^2
}
lambda_var_SML_HMB <- colSums(lambda_dev_SML_HMB)/nobs^2

# calculate significance of price of risk parameters
slope_coef <- t(rbind(lambda_hat, lambda_hat / lambda_var))
colnames(slope_coef) <- c("lambda hat", "t-stat")
slope_coef_SML_HMB <- t(rbind(lambda_hat_SML_HMB, lambda_hat_SML_HMB/lambda_var_SML_HMB))
colnames(slope_coef_SML_HMB) <- c("lambda hat", "t-stat")
