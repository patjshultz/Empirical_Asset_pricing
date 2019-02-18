# ##################################################################################
# Question 3: Calculate one return that would bring D/P back to historical average #
####################################################################################
rm(list = ls())

stock_data <- read.csv("../data/data_annual.csv", stringsAsFactors = F)
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")

dp_ratio <- stock_data$dividends / stock_data$prices
average_dp_ratio <- mean(dp_ratio)
current_dp_ratio <- dp_ratio[length(dp_ratio)]

# calculate price change to bring back to average
div <- stock_data$dividends[nrow(stock_data)]
price <- div * (1/average_dp_ratio)

crash <- (price - stock_data$prices[nrow(stock_data)]) / stock_data$prices[nrow(stock_data)]

# rerun regressions include crash data 
###########################################################
# calculate regression data (log returns, dividend yield) #
###########################################################
cum_div_log_return <- c(log(stock_data$vwretd + 1), log(1 + crash))
div_yield <- c(log(stock_data$dividends/stock_data$prices), log(div/price))

# specify return horizons in year
horizons <- c(1:5, 10)

# set up matrices to store regression summary statistics
stats <- c("b", "t(b)", "r2")
summary_mat_returns <- matrix(data = NA, nrow = length(horizons), ncol = length(stats)) 
rownames(summary_mat_returns) <- horizons; colnames(summary_mat_returns) <- stats

# run regressions
row<-1
for(h in horizons){
  
  # calculate future returns
  returns <- matrix(data = NA, nrow = length(cum_div_log_return), ncol = 1)
  for(i in 1 :(length(cum_div_log_return)-h)){
    returns[i, 1] <- sum(cum_div_log_return[(i+1):(i+h)])
  }
  
  # run regressions for forecasting returns
  fit <- lm(returns ~ div_yield)
  coefficients <- lmtest::coeftest(fit)
  summary_mat_returns[row, 1] <- coefficients["div_yield", "Estimate"]
  summary_mat_returns[row, 2] <- coefficients["div_yield", "t value"]
  summary_mat_returns[row, 3] <- summary(fit)$r.squared
  
  row <- row + 1
}

stargazer::stargazer(summary_mat_returns, title = "Full Sample with crash")
