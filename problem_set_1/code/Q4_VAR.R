#################################################################
# Question 4: VAR of log(D/P), log(D_{t+1}/D_t), risk free rate #
#################################################################
rm(list = ls())
library(astsa)
library(vars)

################################################
# Load data / calculate ratios and check plots #
################################################

stock_data <- read.csv("../data/data_annual.csv", stringsAsFactors = F)
stock_data$date <- as.Date(stock_data$date, date = "%Y/%m/%d")
n <- nrow(stock_data)

# calculate log pd ratios
stock_data$log_div_price_ratio <- log(stock_data$dividends/stock_data$prices)
stock_data$div_growth <- c(NA, log(stock_data$dividends[2:n]) - log(stock_data$dividends[1:(n-1)]))

ggplot(data = stock_data, aes(x = date, y = log_div_price_ratio))+
     geom_line(color = "blue", size = 2) +
     geom_hline(yintercept = -3.6)

ggplot(data = stock_data, aes(x = date, y = dividends))+
  geom_line(color = "blue", size = 2) +
  geom_hline(yintercept = 0)

ggplot(data = stock_data, aes(x = date, y = real_rf))+
  geom_line(color = "blue", size = 2) +
  geom_hline(yintercept = 0)

###########
# Run VAR #
###########

# specify VAR variables
X <- data.frame(dp = stock_data$log_div_price_ratio, 
                dgr = stock_data$div_growth, 
                rf = stock_data$real_rf)
X <- X[-1, ] # drop observations with NAs (this is sloppy, but just the first observation in this case)

# estimate VAR(1) with constant term
out <- VAR(X, p = 1, type = 'const')
summary(out)


