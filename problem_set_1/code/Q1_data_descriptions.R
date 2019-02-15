#####################################################
# 1. Describe the data/ test for a structural break #
#####################################################
rm(list = ls())
library(ggplot2)
theme_set(theme_bw(base_size = 18))

# load data 
stock_data <- read.csv("../data/data_annual.csv", stringsAsFactors = F)
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")
nobs <- nrow(stock_data)

#####################################################################################
# calculate and plot excess returns, dividends growth, risk free rate and P/D ratio #
#####################################################################################
# calculate excess returns
stock_data$excess_return <- stock_data$vwretd - stock_data$t90ret
ggplot(data = stock_data, aes(x = date, y = excess_return))+
  geom_line(color = "blue", size = 2) 

# calculate dividend growth
ggplot(data = stock_data, aes(x = date, y = div_growth))+
  geom_line(color = "blue", size = 2) +
  geom_vline(xintercept = as.Date("1973-01-01"))
stock_data$logpd <- log(stock_data$dividends/stock_data$prices)

# calculate summary statistics 
summary_mat <- matrix(data = NA, nrow = 4, ncol =2)
summary_mat[1, 1:2] <- c(mean(stock_data$excess_return), sd(stock_data$excess_return))
summary_mat[2, 1:2] <- c(mean(stock_data$div_growth, na.rm = T), sd(stock_data$div_growth, na.rm = T))
summary_mat[3, 1:2] <- c(mean(stock_data$real_rf), sd(stock_data$real_rf))
summary_mat[4, 1:2] <- c(mean(stock_data$logpd), sd(stock_data$logpd))


########################################
# Check for a structural break in 1973 #
########################################

##########################################################
# plot log of D/P ratio to make sure it does not explode #
##########################################################

ggplot(data = stock_data, aes(x = date, y = log(stock_data$dividends/stock_data$prices)))+
  geom_line(color = "blue", size = 2) +
  geom_hline(yintercept = mean(log(stock_data$dividends/stock_data$prices))) +
  ylab("Log(D/P)")





