#===============================================================================
# Finance 921: Assignment 3
# Question 1 part c (test of consumption-based CAPM)
#===============================================================================
rm(list = ls())
library(quantmod)

#===============================================================================
# Load data
#===============================================================================

# first complete analysis with quarterly data
getSymbols(c("DNDGRA3Q086SBEA", "DSERRA3Q086SBEA"), src='FRED') # quarterly

qrtly_ND <- DNDGRA3Q086SBEA
qrtly_serv <- DSERRA3Q086SBEA # note that services are only provided in percent change

# calculate change in consumption
consumption <- qrtly_ND + qrtly_serv
dates <- index(consumption)
nobs <- length(dates)
consumption <- as.numeric(consumption)
cons_chg <- as.matrix(log(consumption[2:nobs]/consumption[1:(nobs-1)]))

plot(dates[-1], cons_chg*100, type = "l", xlab = "", ylab = "Delta C", lwd = 2)
abline(h = 0, v = NA)

# load return data 
size_portfolios <- read.csv("../data/ME_returns_monthly.CSV", 
                            stringsAsFactors = F)
size_portfolios$month <- size_portfolios$date %% 100
BEME_portfolios <- read.csv("../data/BEME_returns_monthly.CSV", 
                            stringsAsFactors = F)
BEME_portfolios$month <- BEME_portfolios$date %% 100

# subset to same dates as consumption series
drop_ind <- which(size_portfolios$date < 194700)
BEME_portfolios <- BEME_portfolios[-drop_ind, ]
size_portfolios <- size_portfolios[-drop_ind, ]

# find the rows that are an end of quarter month 
qrt_ind <- which(BEME_portfolios$month %in% c(3, 6, 9, 12))

# set up monthly and quarterly returns
size_qrtly <- matrix(data = NA, nrow = length(qrt_ind), ncol = 10)
BEME_qrtly <- matrix(data = NA, nrow = length(qrt_ind), ncol = 10)
size_mthly <- size_portfolios[, c(-1, -12)]/100
BEME_mthly <- BEME_portfolios[, c(-1, -12)]/100

# aggregate to quarterly returns
for(i in 1:length(qrt_ind)){
  # index of returns within a quarter
  qind <- (qrt_ind[i]-2):qrt_ind[i]
  
  # returns for each month within a quarter
  size_returns <- (1 + size_mthly[qind, ])#^(1/3)
  BEME_returns <- (1 + BEME_mthly[qind, ])#^(1/3) 
  
  # aggregate return for each quarter
  size_qrtly[i, ] <- apply(X = size_returns, MARGIN = 2, FUN = prod)-1
  BEME_qrtly[i, ] <- apply(X = BEME_returns, MARGIN = 2, FUN = prod)-1
}

# check that consumption and returns have the same number of observations 
length(size_portfolios$date[qrt_ind]) - length(dates)

#===============================================================================
# Run the time series and cross sectional regressions using FM procedure
#===============================================================================

# time series regression
ts_regressions <- function(x, y=cons_chg){return(solve(t(x)%*%x)%*%t(x)%*%y)}
size_betas <- apply(X = cbind(1, size_qrtly[-1, ]), MARGIN = 2, FUN = ts_regressions)[-1] # drop the intercept coefficient, we don't care about it
BEME_betas <- apply(X = cbind(1, BEME_qrtly[-1, ]), MARGIN = 2, FUN = ts_regressions)[-1]# drop the intercept coefficient, we don't care about it
size_ER <- colMeans(size_qrtly[-1, ]); BEME_ER <- colMeans(BEME_qrtly[-1, ])

ts_mat <- cbind(size_betas, size_ER, BEME_betas, BEME_ER) 
rownames(ts_mat) <- paste("d", 1:10, sep = '')

# cross sectional regression (don't impose an intercept here)
lambda_size <- solve(t(size_betas)%*%size_betas) %*% size_betas %*% size_ER
lambda_BEME <- solve(t(BEME_betas)%*%BEME_betas) %*% BEME_betas %*% BEME_ER

#===============================================================================
# Using GMM 
# we estimate the market price of risk as [c'Wc]^(-1)c'WET[Re]
#===============================================================================

# first we use W = identity
W <- diag(10)

# calculate cov(Re, Delta c_{t+1})
c <- cov(size_qrtly[-1, ], cons_chg)
