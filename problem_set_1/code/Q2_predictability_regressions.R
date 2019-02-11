rm(list = ls())
library(sandwich)
# load data
data <- read.csv("../data/CRSP_dataset_merged.csv", stringsAsFactors = F)
data$date <- as.Date(data$date, date = "%Y/%m/%d")

# subset to annual data
data_annual <- data[which(month(as.POSIXlt(data$date,
                                           format="%d/%m/%Y")) == 12), ]
data_annual <- data_annual[which(data_annual$date < as.Date("2006-01-01")), ]
prices <- matrix(data_annual$prices)
dividends <- matrix(data_annual$dividends_annual)
dp_ratio <- 1/data_annual$pd_ratio_annual
tbill <- matrix(data_annual$tbill_annualized)

# calculate returns
calc_returns <- function(prices = prices, dividends = dividends, reference_rate = tbill, horizon){
  
  # first specify relevant variables
  prices_k <- prices[(horizon+1):length(prices), ]  
  dividends_k <- dividends[(horizon+1):length(dividends), ] 
  prices_t <- prices[1:(length(prices)-horizon), ]
  tbill <- reference_rate[(horizon+1):length(reference_rate), ]  
  
  # calculate returns
  R_tk <- ((prices_k + dividends_k) / prices_t) - 1 
  return(R_tk - tbill)
}

summary_mat <- matrix(data = NA, nrow = 6, ncol = 3)
return_horizons <- c(1:5, 10)
row <- 1
for(h in return_horizons){
  # select data
  R_tk <- calc_returns(prices, dividends, tbill ,h)
  dp <- dp_ratio[1:(length(dp_ratio)-h)]
  
  # run regressions
  coefficients <- coeftest(lm(R_tk ~ dp), vcov. = sandwich)
  summary_mat[row, 1] <- coefficients["dp", "Estimate"]
  summary_mat[row, 2] <- coefficients["dp", "t value"]
  summary_mat[row, 3] <- summary(lm(R_tk ~ dp))$r.squared

  row <- row + 1
}

summary_mat  