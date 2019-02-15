##############################################
# COnstruct data using only annual CRSP data #
##############################################

# housekeeping
rm(list = ls())
library(gridExtra)
library(ggplot2)
theme_set(theme_bw(base_size = 18))

#######################
# load data from CRSP #
#######################

return_data <- read.csv("../data/returns_annual.csv", stringsAsFactors = F)
return_data$date <- as.Date(as.character(return_data$date), format =  "%Y%m%d")
return_data$month <- as.numeric(as.factor(months(return_data$date)))

inflation_tbills_data <- read.csv("../data/inflation_tbills.csv", stringsAsFactors = F)
inflation_tbills_data$date <- as.Date(as.character(inflation_tbills_data$date), format =  "%Y%m%d")

return_data$month <- as.numeric(format(return_data$date,"%m"))
inflation_tbills_data$month <- as.numeric(format(inflation_tbills_data$date,"%m"))

inflation_tbills_data_annual <- read.csv("../data/inflation_tbill_annual.csv", stringsAsFactors = F)
inflation_tbills_data_annual$date <- as.Date(as.character(inflation_tbills_data_annual$date), format =  "%Y%m%d")

#############################################
# recursively calculate prices and dividends#
#############################################

return_data$prices <- NA
return_data$dividends <- NA

# Assume P(0) = 1 to get t=1 prices and dividends
return_data$prices[1] <- (1 + return_data$vwretx[1])
return_data$dividends[1] <-  return_data$vwretd[1] - return_data$vwretx[1]

# use recursive identity to get price and dividend data
for(i in 2:nrow(return_data)){
  return_data$prices[i] <- return_data$prices[i-1]*(1 + return_data$vwretx[i])
  return_data$dividends[i] <- return_data$prices[i-1] * (return_data$vwretd[i] - return_data$vwretx[i])
}

prices_all_plot <- ggplot(data = na.omit(return_data), aes(x = date, y = prices))+
  geom_line(color = "blue", size = 2)
dividend_all_plot <- ggplot(data = na.omit(return_data), aes(x = date, y = dividends))+
  geom_line(color = "blue", size = 2)

# calculate dividend growth 
return_data$div_growth <- c(NA, log(return_data$dividends[2:length(return_data$dividends)]) - log(return_data$dividends[1:(length(return_data$dividends)-1)]))
# claculate dividend yield
return_data$div_yield <- log(return_data$dividends / return_data$prices)

###################################################
# estimate AR1 process to get expeceted inflation #
###################################################

# annual expected inflation
pi_t <- inflation_tbills_data_annual$cpiret[2:nrow(inflation_tbills_data_annual)]
pi_lag <- inflation_tbills_data_annual$cpiret[1:(nrow(inflation_tbills_data_annual)-1)] 

ar1 <- summary(lm(pi_t~ pi_lag))
intercept <- ar1$coefficients["(Intercept)", "Estimate"]
ar_coef <- ar1$coefficients["pi_lag", "Estimate"]

inflation_tbills_data_annual$exp_inflation <- NA
for(i in 2:nrow(inflation_tbills_data_annual)){
  prev_pi <- inflation_tbills_data_annual$cpiret[(i-1)]
  inflation_tbills_data_annual$exp_inflation[i] <- intercept + ar_coef * prev_pi 
}
inflation_tbills_data_annual$real_rf <- inflation_tbills_data_annual$t90ret - inflation_tbills_data_annual$exp_inflation


#########################
# Plots and export data #
#########################

merged_data <- merge(return_data, inflation_tbills_data, by = "date")

annual_data <- merge(return_data, inflation_tbills_data_annual, by = "date")
save_vars <- c("date", "prices", "vwretd", "vwretx", "dividends","div_growth","b1ret",
               "t90ret", "cpiret", "real_rf", "div_yield")
annual_data <- annual_data[, which(colnames(annual_data) %in% save_vars)]


prices_plot <- ggplot(data = na.omit(annual_data), aes(x = date, y = prices))+
  geom_line(color = "blue", size = 2)
dividends_plot <- ggplot(data = na.omit(annual_data), aes(x = date, y = dividends))+
  geom_line(color = "blue", size = 2)
annual_pd_ratio <- ggplot(data = annual_data, aes(x = date, y = prices/dividends))+
  geom_line(color = "blue", size = 2)
annual_inflation_plot <- ggplot(data = annual_data, aes(x = date, y = cpiret))+
  geom_line(color = "blue", size = 2)
annual_real_rf <- ggplot(data = annual_data, aes(x = date, y = real_rf))+
  geom_line(color = "blue", size = 1.5)

ggplot(data = annual_data, aes(x = date, y = t90ret))+
  geom_line(color = "blue", size = 1.5)

ggplot(data = inflation_tbills_data_annual, aes(x = date, y = exp_inflation))+
  geom_line(color = "blue", size = 1.5)

ggplot(data = inflation_tbills_data_annual, aes(x = date, y = t90ret - exp_inflation))+
  geom_line(color = "blue", size = 1.5)


write.csv(annual_data, "../data/data_annual.csv", row.names = F)
