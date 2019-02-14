# ##################################################################################
# Question 3: Calculate one return that would bring D/P back to historical average #
####################################################################################
rm(list = ls())

stock_data <- read.csv("../data/data_annual.csv", stringsAsFactors = F)
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")

dp_ratio <- stock_data$dividends_annual / stock_data$prices
average_dp_ratio <- mean(dp_ratio)
current_dp_ratio <- dp_ratio[length(dp_ratio)]

# check price dividend ratio
stock_data_dp_ratio <- dp_ratio
ggplot(data = stock_data, aes(x = date, y = dp_ratio))+
  geom_line(color = "blue", size = 2) + 
  labs(x = "", y = "D/P") + geom_hline(yintercept = average_dp_ratio)

# calculate price change to bring back to average
div <- stock_data$dividends_annual[nrow(stock_data)]
price <- div * (1/average_dp_ratio)

(price - stock_data$prices[nrow(stock_data)]) / stock_data$prices[nrow(stock_data)]
