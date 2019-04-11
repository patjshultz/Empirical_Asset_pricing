#===============================================================================
# Finance 921: Assignment 3
# Question 1 part c (test of consumption-based CAPM)
#===============================================================================

rm(list = ls())
library(quantmod)
library(knitr)

#===============================================================================
# Load data
#===============================================================================

# first get consumption data
getSymbols(c("DNDGRA3Q086SBEA", "DSERRA3Q086SBEA", "DNDGRA3M086SBEA", "DSERRA3M086SBEA"), src='FRED') # quarterly

qrtly_ND <- DNDGRA3Q086SBEA
qrtly_serv <- DSERRA3Q086SBEA 

mthly_ND <- DNDGRA3M086SBEA
mthly_serv <- DSERRA3M086SBEA 

# calculate change in consumption
consumption <- mthly_ND #+ qrtly_serv
dates <- index(consumption)
nobs <- length(dates)
dates_cons_chg <- index(consumption)
consumption <- as.numeric(consumption)
cons_chg <- as.matrix(100*(log(consumption[2:nobs]/consumption[1:(nobs-1)])))

plot(cons_chg, type = "l")
abline(h = mean(cons_chg), v = NA)

# load return data 
returns <- read.csv("../data/returns_bivariate_sort_monthly.csv")
FF3 <- read.csv("../data/FF_factors.CSV")
Rf <- FF3$RF

# convert returns into excess returns
returns[, -1] <- returns[, -1] - Rf

min_returns_date <- returns[1, 1]
max_returns_date <- returns[nrow(returns), 1]

max_cons_date <- max(dates)
min_cons_date <- dates[2] # the first date we have cons growth is the second date we have consumption

returns <- returns[which(returns[, 1] > 195901), ]
returns <- returns[-nrow(returns), ]

# check that consumption and returns have the same number of observations 
nrow(returns) - length(cons_chg)
xreturns <- returns[, -1]

# find the rows that are an end of quarter month 
#qrt_ind <- which(BEME_portfolios$month %in% c(3, 6, 9, 12))

# set up monthly and quarterly returns
#size_qrtly <- matrix(data = NA, nrow = length(qrt_ind), ncol = 10)
#BEME_qrtly <- matrix(data = NA, nrow = length(qrt_ind), ncol = 10)


# aggregate to quarterly returns
#for(i in 1:length(qrt_ind)){
  # index of returns within a quarter
#  qind <- (qrt_ind[i]-2):qrt_ind[i]
  
  # returns for each month within a quarter. note that returns are monthly in the datasets from FF
#  size_returns <- (1 + size_mthly[qind, ])#^(1/3)
#  BEME_returns <- (1 + BEME_mthly[qind, ])#^(1/3) 
  
  # aggregate return for each quarter
#  size_qrtly[i, ] <- 100*(apply(X = size_returns, MARGIN = 2, FUN = prod)-1)
#  BEME_qrtly[i, ] <- 100*(apply(X = BEME_returns, MARGIN = 2, FUN = prod)-1)
#}



#===============================================================================
# Run the time series and cross sectional regressions using FM procedure
#===============================================================================

# time series regression
x <- cbind(1, cons_chg)
ts_regressions <- function(y){return(solve(t(x)%*%x)%*%t(x)%*%y)}

# time series regression for size portfolios
estimates <- apply(X = xreturns, MARGIN = 2, FUN = ts_regressions) 
betas <- estimates[2, ]

# unconditional mean of returns for size and be/me portfolios
ER <- colMeans(xreturns)

# make summary table
ts_mat <- rbind(ER, estimates) 
colnames(ts_mat) <- paste("p", 1:25, sep = '')
rownames(ts_mat) <- c("E(R)", "alpha", "beta")

# cross sectional regression (don't impose an intercept here)
CS_regression <- summary(lm(ER ~ betas + 0))
lambda <- CS_regression$coefficients["betas", "Estimate"]


# plot betas vs expected returns with market price of risk line
par(mar = c(5, 5.1, 4, 1))
par(ps = 20)
plot(betas, ER,  
     ylim = c(0, max(ER)), xlim = c(0, max(betas)), 
     xlab = expression(beta), ylab = "Mean quarterly return % (Size Portfolios)", 
     main = paste("lambda=", round(lambda, 3)))
abline(lm(ER~betas + 0))


# plot the actual mean excess return vs the predicted 
Ec <- mean(cons_chg)
predicted_ER <- betas * Ec

plot(predicted_ER, ER, 
     ylab = "Actual mean excess return E(Ri - Rf)",
     xlab = expression(paste("Predicted:", beta, "E(",Delta, "c)")))


#===============================================================================
# Using GMM 
#===============================================================================

X <- xreturns
theta <- rep(1, 51)

CCAPM_GMM_criterion <- function(theta, X){
  
  W <- diag(770)
  
  # parameters which we want to estimate
  a <- theta[1:10]
  beta <- theta[11:20]
  lambda <- theta[21]
  nassets <- ncol(X)
  
  # the data to be used in calculate the g function
  returns <- X
  cgrowth <- cons_chg
  nobs <- length(cgrowth)
  
  # calculate the error at each observation for the "g" function 
  error1 <- matrix(data = NA, nrow = nobs, ncol = ncol(returns))
  error2 <- error1
  error3 <- matrix(data = NA, nrow = nobs, ncol = 1)
  
  # calculate the relevant errors for each time period t
  for(i in 1:nobs){
    error1[i, ] <- as.numeric(returns[i, ] - a - beta * cgrowth[i])
    error2[i, ] <- as.numeric((returns[i, ] - a - beta * cgrowth[i]) * cgrowth[i])
    error3[i, ] <- as.numeric((1/nassets)*(sum(returns[i, ] - beta * lambda))) 
  }
  
  # estimate of moment conditions for given parameters
  gT1 <- colSums(error1)/nobs
  gT2 <- colSums(error2)/nobs
  gT3 <- error3
  
  gT <- c(gT1, gT2, gT3)
  
  criterion <- t(gT) %*% W %*% gT 
  #return(errors)
  return(criterion)
}

CCAPM_GMM_criterion(theta = rep(1, 51), X = xreturns)

GMM_estimates <- optim(par = c(rep(0, 25), rep(1, 26)), fn = CCAPM_GMM_criterion,
                       X = xreturns, method = "SANN")
