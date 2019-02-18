########################################################
# 5. Campbell Shiller Approx and VAR implied estimates #
########################################################
library(ggplot2)
library(reshape2)
theme_set(theme_bw(base_size = 18))

# load data
stock_data <- read.csv("../data/data_annual.csv", stringsAsFactors = F)
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")

# compute parameters of approximation
pd <- log(stock_data$prices) - log(stock_data$dividends)
pd_bar <- mean(pd)
pd_lag <- c(NA, pd[1:(length(pd)-1)])

dgr <- stock_data$div_growth
dgr_bar <- mean(na.omit(stock_data$div_growth))
kappa <- exp(pd_bar)/(1 + exp(pd_bar))

# compute approximation
CS_approx <- log(1 + exp(pd_bar)) - pd_bar + kappa * (pd - pd_bar) - (pd_lag - pd_bar) + dgr

# plot the approximation vs actual returns
df <- data.frame(date = stock_data$date, 
                 returns = log(1 + stock_data$vwretd),
                 CS_approx = CS_approx)
df$error <- df$returns - df$CS_approx
df_melt_plot1 <- melt(df[, -4], id.vars = "date")
df_melt_errors <- melt(df[, c(1, 4)], id.vars = "date")

ggplot(data=df_melt_plot1[-1, ],
       aes(x=date, y=value, colour=variable)) +
  geom_line()

ggplot(data=df_melt_errors[-1, ],
       aes(x=date, y=value, colour=variable)) +
  geom_line(colour = "blue") + ylab("Approximation error")

# calculate RMSE
error <- df$CS_approx - df$returns
mean_square_error <- mean(error^2, na.rm = T)
RMSE <- sqrt(mean_square_error)

################################################################
# use campbell shiller approximation to infer VAR coefficients #
# for predicting one period ahead return by div yield.         #
################################################################
beta <- -kappa * A1[1,1] +A1[2,1] +1

