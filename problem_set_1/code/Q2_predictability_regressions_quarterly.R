####################################
# Question 2: Return predictability#
####################################
rm(list = ls())
library(ggplot2) # for plotting
library(sandwich) # for standard errors
library(zoo)
theme_set(theme_bw(base_size = 18))

######################
# load / format data #
######################
stock_data <- read.csv("../data/data_quarterly.csv")
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")


###########################################################
# calculate regression data (log returns, dividend yield) #
###########################################################
cum_div_log_return <- log(stock_data$vwretd + 1)
ex_div_log_return <- log(stock_data$vwretx + 1)
log_real_rf <- log(stock_data$real_rf + 1)

div_yield <- log(stock_data$dividends/stock_data$prices)

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
  coefficients <- lmtest::coeftest(fit, vcov. = sandwich)
  summary_mat_returns[row, 1] <- coefficients["div_yield", "Estimate"]
  summary_mat_returns[row, 2] <- coefficients["div_yield", "t value"]
  summary_mat_returns[row, 3] <- summary(fit)$r.squared
  
  row <- row + 1
}

stargazer::stargazer(summary_mat_returns, title = "Full Sample")

#################################
# now run for different samples #
#################################

# sample 1926 - 1990 ==============================================================
sample <- seq(as.Date("1926/01/01"), as.Date("1990/12/31"), by = "day")
sub_sample_1926_1990 <- stock_data[which(stock_data$date %in% sample ), ]

cum_div_log_return <- log(sub_sample_1926_1990$vwretd + 1)
ex_div_log_return <- log(sub_sample_1926_1990$vwretx + 1)
log_real_rf <- log(sub_sample_1926_1990$real_rf + 1)

div_yield <- log(sub_sample_1926_1990$dividends/sub_sample_1926_1990$prices)

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
  coefficients <- lmtest::coeftest(fit, vcov. = sandwich)
  summary_mat_returns[row, 1] <- coefficients["div_yield", "Estimate"]
  summary_mat_returns[row, 2] <- coefficients["div_yield", "t value"]
  summary_mat_returns[row, 3] <- summary(fit)$r.squared
  
  row <- row + 1
}
stargazer::stargazer(summary_mat_returns, title = "Sample 1926-1990")

# 1947 - 1990 =========================================================
sample <- seq(as.Date("1947/01/01"), as.Date("1990/12/31"), by = "day")
sub_sample_1947_1990 <- stock_data[which(stock_data$date %in% sample ), ]

cum_div_log_return <- log(sub_sample_1947_1990$vwretd + 1)
ex_div_log_return <- log(sub_sample_1947_1990$vwretx + 1)
log_real_rf <- log(sub_sample_1947_1990$real_rf + 1)
div_yield <- log(sub_sample_1947_1990$dividends/sub_sample_1947_1990$prices)

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
  coefficients <- lmtest::coeftest(fit, vcov. = sandwich)
  summary_mat_returns[row, 1] <- coefficients["div_yield", "Estimate"]
  summary_mat_returns[row, 2] <- coefficients["div_yield", "t value"]
  summary_mat_returns[row, 3] <- summary(fit)$r.squared
  
  row <- row + 1
}
stargazer::stargazer(summary_mat_returns, title = "1947 - 1990 Sample")

# 1947 to 2018=================================================================
sample <- seq(as.Date("1947/01/01"), as.Date("2018/12/31"), by = "day")
sub_sample_1947_today <- stock_data[which(stock_data$date %in% sample ), ]

cum_div_log_return <- log(sub_sample_1947_today$vwretd + 1)
ex_div_log_return <- log(sub_sample_1947_today$vwretx + 1)
log_real_rf <- log(sub_sample_1947_today$real_rf + 1)

div_yield <- log(sub_sample_1947_today$dividends/sub_sample_1947_today$prices)

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
  coefficients <- lmtest::coeftest(fit, vcov. = sandwich)
  summary_mat_returns[row, 1] <- coefficients["div_yield", "Estimate"]
  summary_mat_returns[row, 2] <- coefficients["div_yield", "t value"]
  summary_mat_returns[row, 3] <- summary(fit)$r.squared
  
  row <- row + 1
}
stargazer::stargazer(summary_mat_returns, title = "Sample 1947-now")

# 1973 - today ================================================================
sample <- seq(as.Date("1973/01/01"), as.Date("2018/12/31"), by = "day")
sub_sample_1973_today <- stock_data[which(stock_data$date %in% sample ), ]

cum_div_log_return <- log(sub_sample_1973_today$vwretd + 1)
ex_div_log_return <- log(sub_sample_1973_today$vwretx + 1)
log_real_rf <- log(sub_sample_1973_today$real_rf + 1)
div_yield <- log(sub_sample_1973_today$dividends/sub_sample_1973_today$prices)

# specify return horizons in year
horizons <- c(4, 8, 12, 16, 20, 40)

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
  coefficients <- lmtest::coeftest(fit, vcov. = sandwich)
  summary_mat_returns[row, 1] <- coefficients["div_yield", "Estimate"]
  summary_mat_returns[row, 2] <- coefficients["div_yield", "t value"]
  summary_mat_returns[row, 3] <- summary(fit)$r.squared
  
  row <- row + 1
}
stargazer::stargazer(summary_mat_returns, title = "1973 - today sample")
