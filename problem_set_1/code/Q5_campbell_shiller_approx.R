########################################################
# 5. Campbell Shiller Approx and VAR implied estimates #
########################################################
library(ggplot2)
stock_data <- read.csv("../data/data_annual.csv", stringsAsFactors = F)
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")

campbell_shiller_approx <- function(k, rho, prices, dividends){
  # k0: constant determined by mean of dividend yield
  # k1: constant determined by mean of dividend yield
  # prices: vector of prices from CRSP
  # dividends: vecotr of dividends from CRSP
  # return: the approximation for cum dividend return from t to t + 1
  n <- length(prices)
  pt1 <- log(prices[2:n]); pt <- log(prices[1:(n-1)])
  dt1 <- log(dividends[2:n]); dt <- log(dividends[1:(n-1)])
  return(k + rho * pt1 + (1-rho)* dt1 - pt)
}


ggplot(data = stock_data, aes(x = date, y = log(1+stock_data$vwretd)))+
  geom_line(color = "blue", size = 2) + 
  labs(x = "", y = "Cum. Div. Return from CRSP")


rho <- 1 / (1 + exp(mean(stock_data$div_yield)))
k <- -log(rho) - (1-rho)*log(1/rho - 1)                        
stock_data$approx_returns <- c(NA,campbell_shiller_approx(k = k, rho = rho, prices = stock_data$prices, dividends = stock_data$dividends))

ggplot(data = stock_data, aes(x = date, y = approx_returns))+
  geom_line(color = "blue", size = 2) 
ggplot(data = stock_data, aes(x = date, y = vwretd))+
  geom_line(color = "blue", size = 2) 

ggplot(data = stock_data, aes(x = date, y = vwretd - approx_returns))+
  geom_line(color = "blue", size = 2) 

cor(x = stock_data$vwretd[-1], y = stock_data$approx_returns[-1])
mean((stock_data$vwretd[-1] - stock_data$approx_returns[-1])^2)
