##################################################
# Question 6: VAR implied regression coefficient #
##################################################

rm(list = ls())
library(ggplot2) 
library(sandwich) 
theme_set(theme_bw(base_size = 18))

######################
# load / format data #
######################
stock_data <- read.csv("../data/data_annual.csv")
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")

###########################################################
# calculate regression data (log returns, dividend yield) #
###########################################################
cum_div_log_return <- log(stock_data$vwretd + 1)
ex_div_log_return <- log(stock_data$vwretx + 1)
log_real_rf <- log(stock_data$real_rf + 1)
div_yield <- stock_data$div_yield

# specify return horizons in quarters
horizons <- c(1, 3, 5)

# set up matrices to store regression summary statistics
stats <- c("b", "t(b)", "r2")
summary_mat_returns <- matrix(data = NA, nrow = 3, ncol = 3) 
summary_mat_dividends <- matrix(data = NA, nrow = 3, ncol = 3)
rownames(summary_mat_returns) <- horizons; colnames(summary_mat_returns) <- stats
rownames(summary_mat_dividends) <- horizons; colnames(summary_mat_dividends) <- stats

########################################
# run equation by equation regressions #
########################################

row<-1
for(h in horizons){
  
  # calculate future returns
  returns <- matrix(data = NA, nrow = length(cum_div_log_return), ncol = 1)
  for(i in 1 :(length(cum_div_log_return)-h)){
    returns[i, 1] <- sum(cum_div_log_return[(i+1):(i+h)])
  }
  
  # calculate dividend growth
  div_growth <- matrix(data = NA, nrow = length(cum_div_log_return), ncol = 1)
  for(i in 1 :(length(cum_div_log_return)-h)){
    div_growth[i, 1] <- log(stock_data$dividends[i+h]) - log(stock_data$dividends[i])
  }
  
  # run regressions for forecasting returns
  fit <- lm(returns ~ div_yield)
  coefficients <- lmtest::coeftest(fit, vcov. = sandwich)
  summary_mat_returns[row, 1] <- coefficients["div_yield", "Estimate"]
  summary_mat_returns[row, 2] <- coefficients["div_yield", "t value"]
  summary_mat_returns[row, 3] <- summary(fit)$r.squared
  
  # run regressions for forecasting dividend growth
  fit <- lm(div_growth ~ div_yield)
  coefficients <- lmtest::coeftest(fit, vcov. = sandwich)
  summary_mat_dividends[row, 1] <- coefficients["div_yield", "Estimate"]
  summary_mat_dividends[row, 2] <- coefficients["div_yield", "t value"]
  summary_mat_dividends[row, 3] <- summary(fit)$r.squared
  
  row <- row + 1
}

# Generate tables of direct regressions
stargazer::stargazer(summary_mat_returns, colnames = T, 
                     rownames = T, title = "Return Regressions")
stargazer::stargazer(summary_mat_dividends, colnames = T, 
                     rownames = T, title = "Dividend Growth Regressions")

#########################################################
# Run VAR  an calculate implied regression coefficients #
#########################################################
X <- data.frame(dp = stock_data$div_yield,
               dgr = stock_data$div_growth,
               rft = stock_data$t90ret)
X <- na.omit(X)

# parameters from CS approx. and derivation of implied coefficients
kappa <- 1 / (1 + exp(mean(stock_data$div_yield)))
A <- Bcoef(VAR(X, type = "none"))
C <- c(-kappa, 1, 0) %*% A + c(1, 0, 0)
e1 <- c(1, 0, 0); e2 <- c(0, 1, 0) 

# calculate necessary maniplations to A matrix from VAR
sum_1 <- A
sum_3 <- matrix(0, 3, 3); for (i in 1:3){sum_3 <- sum_3 + A^(i)}
sum_5 <- matrix(0, 3, 3); for (i in 1:5){sum_5 <- sum_5 + A^(i)}

var_X <- var(X)
beta1 <- C %*% sum_1 %*% var_X %*% t(t(e1)) / e1 %*% var_X %*% t(t(e1))
beta2 <- C %*% sum_3 %*% var_X %*% t(t(e1)) / e1 %*% var_X %*% t(t(e1))
beta3 <- C %*% sum_5 %*% var_X %*% t(t(e1))/ e1 %*% var_X %*% t(t(e1))

