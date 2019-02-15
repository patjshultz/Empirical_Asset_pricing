#################################################################
# Question 4: VAR of log(D/P), log(D_{t+1}/D_t), risk free rate #
#################################################################
rm(list = ls())
library(vars)
library(sandwich)
library(knitr)
################################################
# Load data / calculate ratios and check plots #
################################################

stock_data <- read.csv("../data/data_annual.csv", stringsAsFactors = F)
stock_data$date <- as.Date(stock_data$date, date = "%Y/%m/%d")
n <- nrow(stock_data)

# calculate log pd ratios
stock_data$log_div_price_ratio <- log(stock_data$dividends/stock_data$prices)
stock_data$div_growth <- c(NA, log(stock_data$dividends[2:n]) - log(stock_data$dividends[1:(n-1)]))

###########
# Run VAR #
###########

# specify VAR variables
X <- data.frame(dp = stock_data$log_div_price_ratio, 
                dgr = stock_data$div_growth, 
                rf = stock_data$t90ret)
X <- na.omit(X)

# estimate VAR(1) with constant term
return_model <- VAR(X, p = 1, type = 'const')
coef <- Bcoef(return_model)
A0 <- coef[, 4]
A1 <- coef[, 1:3]

# compute standard errors
HAC_vcov <- NeweyWest(return_model, lag = 4, prewhite = F)
HAC_se <- matrix(sqrt(as.vector(diag(HAC_vcov))), 3, 4)

# impulse response functions
impulse_responses <- irf(return_model)
plot(irf(return_model, nsteps = 20), col = "blue" )
