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
                rf = stock_data$real_rf)
X <- na.omit(X)

# estimate VAR(1) with constant term
return_model <- VAR(X, p = 1, type = 'const')
coef <- Bcoef(return_model)
A0 <- coef[, 4]
A1 <- coef[, 1:3]

# compute standard errors
HAC_vcov <- NeweyWest(return_model, lag = 4, prewhite = F)
HAC_se <- matrix(sqrt(as.vector(diag(HAC_vcov))), 3, 4)

#####################################################################
# Q5. Given VAR, infer return predictability using CS approximation #
#####################################################################

pd <- log(stock_data$prices) - log(stock_data$dividends)
pd_bar <- mean(pd)
pd_lag <- c(NA, pd[1:(length(pd)-1)])

dgr <- stock_data$div_growth
dgr_bar <- mean(na.omit(stock_data$div_growth))
kappa <- exp(pd_bar)/(1 + exp(pd_bar))
kappa_0 <- log(1 + exp(pd_bar)) - exp(pd_bar)/(1+exp(pd_bar))*pd_bar

CS_approx <- kappa_0 + kappa * pd - pd_lag + dgr
df <- data.frame(date = stock_data$date, 
                 returns = log(1 + stock_data$vwretd),
                 CS_approx = CS_approx)
df_melt <- melt(df, id.vars = "date")

ggplot(data=df_melt[-1, ],
       aes(x=date, y = value, colour = variable)) +
  geom_line()

RMSE <- sqrt(mean((na.omit(df$CS_approx - df$returns))^2))

################################################################
# use campbell shiller approximation to infer VAR coefficients #
# for predicting one period ahead return by div yield.         #
################################################################

# first check if the dividend yield predicts one period ahead return
beta_dp <- -kappa * A1[1,1] + A1[2,1] +1

# standard errors implied by delta method which provides a "sandwich" estimator of the SE
jacobian <- c(0,-kappa,0,0,0,1,0,0,0,0,0,0)
implied_SE_dp <-  (jacobian %*% HAC_vcov %*% jacobian)^(1/2)
implied_Rsquared <- beta_dp^2 * var(stock_data$log_div_price_ratio)/var(log(1 + stock_data$vwretd))
implied_t_stat <- beta_dp/implied_SE_dp

# Now check if all three variables predict one period ahead return
beta_dgr <- -kappa * A1[2,3] + A1[2,2]
jacobian <- c(0,0,-kappa,0,0,0,1,0,0,0,0,0)
implied_SE_dgr <-  (jacobian %*% HAC_vcov %*% jacobian)^(1/2)


beta_r <- -kappa * A1[1,3] + A1[2,3]
jacobian <- c(0,0,0,-kappa,0,0,0,1,0,0,0,0)
implied_SE_r <-  (jacobian %*% HAC_vcov %*% jacobian)^(1/2)
