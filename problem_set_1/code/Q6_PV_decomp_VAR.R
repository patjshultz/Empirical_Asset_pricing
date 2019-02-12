# Question 6: VAR test of price dividend ratio decompositions
# housekeeping##
################
rm(list = ls())
library(ggplot2) # for plotting
library(sandwich) # for standard errors
library(zoo)
library(gridExtra)
theme_set(theme_bw(base_size = 18))

######################
# load / format data #
######################
stock_data <- read.csv("../data/Q6_data_quarterly.csv")
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")

# use observations with complete data
stock_data <- stock_data[-(1:3), ]

#subset to match the Dog that did not bark 
#stock_data <- stock_data[which(stock_data$date<as.Date("2007-01-01")), ]

###########################################################
# calculate regression data (log returns, dividend yield) #
###########################################################
cum_div_log_return <- log(stock_data$vwretd_quarterly + 1)
ex_div_log_return <- log(stock_data$vwretd_quarterly + 1)
log_real_rf <- log(stock_data$real_rf + 1)

div_yield <- log(stock_data$smoothed_dividends/stock_data$prices)

# check price dividend ratio
ggplot(data = stock_data, aes(x = date, y = log(1/(prices/smoothed_dividends))))+
  geom_line(color = "blue", size = 2) + 
  labs(x = "", y = "P/D")


# specify return horizons in quarters
horizons <- c(4, 12, 20)

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
    div_growth[i, 1] <- stock_data$smoothed_dividends[i+h] - stock_data$smoothed_dividends[i]
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


# VAR of  










######################
# Figures and Tables #
######################


stargazer::stargazer(summary_mat_returns, colnames = T, 
                     rownames = T, title = "Return Regressions")
stargazer::stargazer(summary_mat_dividends, colnames = T, 
                     rownames = T, title = "Dividend Growth Regressions")

stock_data$dividend_yield <- div_yield
dyield_plot <- ggplot(data = stock_data, aes(x = date, y = dividend_yield))+
       geom_line(color = "blue", size = 2) + 
       labs(x = "", y = "Log(D/P)")

return_plot <- ggplot(data = stock_data, aes(x = date, y = ex_div_log_return))+
  geom_line(color = "blue", size = 2) + 
  labs(x = "", y = "Ex div return (quarterly)")

div_plot <- ggplot(data = stock_data, aes(x = date, y = smoothed_dividends))+
  geom_line(color = "blue", size = 2) + 
  labs(x = "", y = "Dividends (quarterly)")

grid.arrange(
  dyield_plot, return_plot, div_plot,
  layout_matrix = rbind(c(1, 1) ,
                        c(2, 3))
)
 
