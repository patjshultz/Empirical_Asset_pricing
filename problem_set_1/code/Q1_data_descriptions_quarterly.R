#####################################################
# 1. Describe the data/ test for a structural break #
#####################################################
rm(list = ls())
library(ggplot2)
theme_set(theme_bw(base_size = 18))

# load data 
stock_data <- read.csv("../data/data_quarterly.csv")
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")


#####################################################################################
# calculate and plot excess returns, dividends growth, risk free rate and P/D ratio #
#####################################################################################
# calculate excess returns
stock_data$excess_return <- stock_data$vwretd_quarterly - stock_data$t90ret
ggplot(data = stock_data, aes(x = date, y = excess_return))+
  geom_line(color = "blue", size = 2) 

# calculate dividend growth
stock_data$div_growth <- c(NA, diff(log(stock_data$dividends_quarterly)))
ggplot(data = stock_data, aes(x = date, y = div_growth))+
  geom_line(color = "blue", size = 1) +
  xlab("") + ylab("Quarterly Dividend Growth")

# plot dividends
ggplot(data = stock_data, aes(x = date, y = dividends_quarterly))+
  geom_line(color = "blue", size = 2) 

stock_data$div_yield <- log(stock_data$dividends/stock_data$prices)



# calculate summary statistics 
summary_mat <- matrix(data = NA, nrow = 4, ncol =2)
summary_mat[1, 1:2] <- c(mean(stock_data$excess_return, na.rm = T), sd(stock_data$excess_return, na.rm = T))
summary_mat[2, 1:2] <- c(mean(stock_data$div_growth, na.rm = T), sd(stock_data$div_growth, na.rm = T))
summary_mat[3, 1:2] <- c(mean(stock_data$real_rf, na.rm = T), sd(stock_data$real_rf, na.rm = T))
summary_mat[4, 1:2] <- c(mean(stock_data$div_yield, na.rm = T), sd(stock_data$div_yield, na.rm = T))

########################################
# Check for a structural break in 1973 #
########################################

# demeaned dividend growth  and dividend yield
dgr_demean <- stock_data$div_growth - mean(stock_data$div_growth, na.rm = T)
dp_demean <- stock_data$div_yield - mean(stock_data$div_yield, na.rm = T)

# create dummy for after 1973 
date_dummy <- matrix(data = 0, nrow = nrow(stock_data), 1)
date_dummy[which(stock_data$date > as.Date("1973-06-30")), ] <- 1

# run regression test
fit <- lm(dgr_demean ~ dp_demean + date_dummy + date_dummy*dp_demean)
stargazer::stargazer(fit)

# run chow test
strucchange::sctest(stock_data$div_growth ~ 1,
                    type = "Chow", 
                    point = which(stock_data$date == as.Date("1973-06-29")))



