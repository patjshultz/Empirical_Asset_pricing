# housekeeping
rm(list = ls())
library(astsa)
library(vars)

# Load data
stock_data <- read.csv("../data/data_quarterly.csv", stringsAsFactors = F)
stock_data$date <- as.Date(stock_data$date, date = "%Y/%m/%d")
n <- nrow(stock_data)

# calculate log pd ratios
stock_data$log_div_price_ratio <- log(stock_data$smoothed_dividends/stock_data$prices)
stock_data$div_growth <- c(rep(NA, 4), log(stock_data$dividends_quarterly[5:n]) - log(stock_data$dividends_quarterly[1:(n-4)]))

ggplot(data = stock_data, aes(x = date, y = log_div_price_ratio))+
     geom_line(color = "blue", size = 2) +
     geom_hline(yintercept = -3.6)

ggplot(data = stock_data, aes(x = date, y = div_growth))+
  geom_line(color = "blue", size = 2) 

# specify VAR variables
X <- data.frame(dp = stock_data$log_div_price_ratio, 
                dgr = stock_data$div_growth, 
                rf = stock_data$t30ret)
X <- X[-c(1:5), ] # drop observations with NAs

head(X)
# create data and run VAR(1)
out <- VAR(X, p = 1, type = 'both')
stargazer::stargazer(round(coeftest(out, vcov. = sandwich), 3))
