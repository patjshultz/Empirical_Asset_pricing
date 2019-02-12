# housekeeping
rm(list = ls())
library(gridExtra)
library(ggplot2)
theme_set(theme_bw())

#######################
# load data from CRSP #
#######################

return_data <- read.csv("../data/returns.csv", stringsAsFactors = F)
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

for(i in 2:nrow(return_data)){
  return_data$prices[i] <- return_data$prices[i-1]*(1 + return_data$vwretx[i])
  return_data$dividends[i] <- return_data$prices[i-1] * (return_data$vwretd[i] - return_data$vwretx[i])
}

prices_all_plot <- ggplot(data = na.omit(return_data), aes(x = date, y = prices))+
  geom_line(color = "blue", size = 2)
dividend_all_plot <- ggplot(data = na.omit(return_data), aes(x = date, y = dividends))+
  geom_line(color = "blue", size = 2)

###################################################
# calculate annual and quaterly returns/dividends #
###################################################

return_data$vwretd_quarterly <- NA
return_data$vwretx_quarterly <- NA
return_data$dividends_quarterly <- NA

return_data$vwretd_annual <- NA
return_data$vwretx_annual <- NA
return_data$dividends_annual <- NA

return_data$smoothed_dividends <- NA


for (i in 4:nrow(return_data)) {
  # calculate quarterly dividends and returns
  if (return_data$month[i] %in% c(3, 6, 9, 12)) {
    return_data$dividends_quarterly[i] <- sum(return_data$dividends[(i-2):i])
    return_data$vwretd_quarterly[i] <- prod(1 + return_data$vwretd[(i-2):i])-1
    return_data$vwretx_quarterly[i] <- prod(1 + return_data$vwretx[(i-2):i])-1
    
  } 

  # calculate annual dividends and returns
  if (return_data$month[i] == 12) {
    return_data$dividends_annual[i] <- sum(return_data$dividends[(i - 11):i])
    return_data$vwretd_annual[i] <- prod(1 + return_data$vwretd[(i - 11):i])-1
    return_data$vwretx_annual[i] <- prod(1 + return_data$vwretx[(i - 11):i])-1
  } 
  
  # calculate smoothed dividends (i.e. sum of past years dividends)
  if (i > 11){
    return_data$smoothed_dividends[i] <- sum(return_data$dividends[(i - 11):i])
  }
}



################################################
# calculate annual inflation/risk free returns #
################################################

inflation_tbills_data$tbill_annual <- NA
inflation_tbills_data$inflation_annual <- NA

for (i in 12:nrow(inflation_tbills_data)) {
  # calculate annual dividends and returns
  if (inflation_tbills_data$month[i] == 12) {
    inflation_tbills_data$tbill_annual[i] <- prod(1 + inflation_tbills_data$t90ret[(i - 11):i])-1
    inflation_tbills_data$inflation_annual[i] <- prod(1 + inflation_tbills_data$cpiret[(i - 11):i])-1
  } 
}


###################################################
# estimate AR1 process to get expeceted inflation #
###################################################
pi_t <- inflation_tbills_data$cpiret[2:nrow(inflation_tbills_data)]
pi_lag <- inflation_tbills_data$cpiret[1:(nrow(inflation_tbills_data)-1)] 

ar1 <- summary(lm(pi_t~ pi_lag))
intercept <- ar1$coefficients["(Intercept)", "Estimate"]
ar_coef <- ar1$coefficients["pi_lag", "Estimate"]

inflation_tbills_data$exp_inflation_quarterly <- NA
for(i in 2:nrow(inflation_tbills_data)){
  prev_pi <- inflation_tbills_data$cpiret[(i-1)]
  inflation_tbills_data$exp_inflation_quarterly[i] <- intercept + ar_coef * prev_pi 
}

inflation_tbills_data$real_rf <- inflation_tbills_data$t90ret - inflation_tbills_data$exp_inflation_quarterly

#########################
# Plots and export data #
#########################

merged_data <- merge(return_data, inflation_tbills_data, by = "date")


# subset to a quarterly dataset 
quarterly_data <- merged_data[which(merged_data$month.x %in% c(3, 6, 9, 12)), ]
save_vars <- c("date", "prices", "vwretd_quarterly", "vwretx_quarterly", 
               "dividends_quarterly", "smoothed_dividends", "t90ret", "cpiret",
               "exp_inflation_quarterly", "real_rf")
quarterly_data <- quarterly_data[, which(colnames(quarterly_data) %in% save_vars)]

dividend_quarterly_plot <- ggplot(data = quarterly_data, aes(x = date, y = dividends_quarterly))+
  geom_line(color = "blue", size = 2)
dividends_smoothed_plot <- ggplot(data = na.omit(quarterly_data), aes(x = date, y = smoothed_dividends))+
  geom_line(color = "blue", size = 2)
inflation_quarterly_plot <- ggplot(data = na.omit(quarterly_data), aes(x = date, y = cpiret))+
  geom_line(color = "blue", size = 2)
tbill_quarterly_plot <- ggplot(data = na.omit(quarterly_data), aes(x = date, y = t90ret))+
  geom_line(color = "blue", size = 2)
exp_inf_quarterly_plot <- ggplot(data = na.omit(quarterly_data), aes(x = date, y = exp_inflation_quarterly))+
  geom_line(color = "blue", size = 2)
real_rf_plot <- ggplot(data = na.omit(quarterly_data), aes(x = date, y = real_rf))+
  geom_line(color = "blue", size = 1)

# subset to an annual dataset 
annual_data <- merged_data[which(merged_data$month.x == 12), ]
save_vars <- c("date", "prices", "vwretd_annual", "vwretx_annual", "dividends_annual",
               "tbill_annual", "inflation_annual")
annual_data <- annual_data[, which(colnames(annual_data) %in% save_vars)]

annual_pd_ratio <- ggplot(data = na.omit(annual_data), aes(x = date, y = prices/dividends_annual))+
  geom_line(color = "blue", size = 2)
annual_inflation_plot <- ggplot(data = na.omit(annual_data), aes(x = date, y = inflation_annual))+
  geom_line(color = "blue", size = 2)
annual_real_rf <- ggplot(data = na.omit(annual_data), aes(x = date, y = inflation_annual))+
  geom_line(color = "blue", size = 2)

write.csv(annual_data, "../data/data_annual.csv", row.names = F)
write.csv(quarterly_data, "../data/data_quarterly.csv", row.names = F)
