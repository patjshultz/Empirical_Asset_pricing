rm(list = ls())
library(ggplot2) # for plotting
library(sandwich) # for standard errors
library(zoo)
library(gridExtra)

theme_set(theme_bw(base_size = 18))

# Question 6: VAR test of price dividend ratio decompositions
stock_data <- read.csv("../data/Q6_data_quarterly.csv")
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")
# use observations with complete data
stock_data <- stock_data[-(1:3), ]

# specify log returns
cum_div_log_return <- log(stock_data$vwretd_quarterly + 1)
ex_div_log_return <- log(stock_data$vwretd_quarterly + 1)
div_yield <- stock_data$smoothed_dividends/stock_data$prices


# specify return horizons in quarters
horizons <- c(4, 12, 20)

# set up matrices to store regression summary statistics
stats <- c("b", "se", "r2")
summary_mat_returns <- matrix(data = NA, nrow = 3, ncol = 3) 
summary_mat_dividends <- matrix(data = NA, nrow = 3, ncol = 3)
rownames(summary_mat_returns) <- horizons; colnames(summary_mat_returns) <- stats
rownames(summary_mat_dividends) <- horizons; colnames(summary_mat_dividends) <- stats


row<-1
for(h in horizons){
  
  returns <- c(rollapply(cum_div_log_return, h, sum), rep(NA, (h-1)))
  div_growth <- c(diff(stock_data$dividends_quarterly, h), rep(NA, h))

  # run regressions for forecasting returns
  fit <- lm(returns ~ div_yield)
  coefficients <- lmtest::coeftest(fit, vcov. = sandwich)
  summary_mat_returns[row, 1] <- coefficients["div_yield", "Estimate"]
  summary_mat_returns[row, 2] <- coefficients["div_yield", "Std. Error"]
  summary_mat_returns[row, 3] <- summary(fit)$r.squared
  
  # run regressions for forecasting dividend growth
  fit <- lm(div_growth ~ div_yield)
  coefficients <- lmtest::coeftest(fit, vcov. = sandwich)
  summary_mat_dividends[row, 1] <- coefficients["div_yield", "Estimate"]
  summary_mat_dividends[row, 2] <- coefficients["div_yield", "Std. Error"]
  summary_mat_dividends[row, 3] <- summary(fit)$r.squared
  
  row <- row + 1
}












######################
# Figures and Tables #
######################


stargazer::stargazer(summary_mat_returns, colnames = T, 
                     rownames = T, title = "Return Regressions")
stargazer::stargazer(summary_mat_dividends, colnames = T, 
                     rownames = T, title = "Dividend Growth Regressions")

stock_data$dividend_yield <- div_yield
dyield_plot <- ggplot(data = stock_data, aes(x = date, y = log(dividend_yield)))+
       geom_line(color = "blue", size = 2) + 
       labs(x = "", y = "Log(D/P)")

return_plot <- ggplot(data = stock_data, aes(x = date, y = ex_div_log_return))+
  geom_line(color = "blue", size = 2) + 
  labs(x = "", y = "Ex div return (quarterly)")

div_plot <- ggplot(data = stock_data, aes(x = date, y = dividends_quarterly))+
  geom_line(color = "blue", size = 2) + 
  labs(x = "", y = "Dividends (quarterly)")

grid.arrange(
  dyield_plot, return_plot, div_plot,
  layout_matrix = rbind(c(1, 1) ,
                        c(2, 3))
)
 
