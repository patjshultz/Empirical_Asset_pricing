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
stock_data <- read.csv("../data/data_annual.csv")
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")

#subset to match the Dog that did not bark 
#stock_data <- stock_data[which(stock_data$date<as.Date("2007-01-01")), ]

###########################################################
# calculate regression data (log returns, dividend yield) #
###########################################################
cum_div_log_return <- log(stock_data$vwretd + 1)
ex_div_log_return <- log(stock_data$vwretx + 1)
log_real_rf <- log(stock_data$real_rf + 1)

div_yield <- log(stock_data$dividends/stock_data$prices)

# check price dividend ratio
ggplot(data = stock_data, aes(x = date, y = log(dividends/prices)))+
  geom_line(color = "blue", size = 2) + 
  labs(x = "", y = "log(D/P)")


# specify return horizons in year
horizons <- c(1:5, 10)

# set up matrices to store regression summary statistics
stats <- c("b", "t(b)", "r2")
summary_mat_returns <- matrix(data = NA, nrow = length(horizons), ncol = length(stats)) 
rownames(summary_mat_returns) <- horizons; colnames(summary_mat_returns) <- stats

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
  
   # run regressions for forecasting returns
  fit <- lm(returns ~ div_yield)
  coefficients <- lmtest::coeftest(fit, vcov. = sandwich)
  summary_mat_returns[row, 1] <- coefficients["div_yield", "Estimate"]
  summary_mat_returns[row, 2] <- coefficients["div_yield", "t value"]
  summary_mat_returns[row, 3] <- summary(fit)$r.squared
  
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

