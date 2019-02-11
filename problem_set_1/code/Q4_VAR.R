# housekeeping
rm(list = ls())
library(astsa)
library(vars)

# Load data
data <- read.csv("../data/CRSP_dataset_merged.csv", stringsAsFactors = F)
data$date <- as.Date(data$date, date = "%Y/%m/%d")

# subset to annual data
data_annual <- data[which(month(as.POSIXlt(data$date,
                                           format="%d/%m/%Y")) == 12), ]

# construct dividend growth 
d <- data_annual$dividends_annual[1:(nrow(data_annual)-1)]
dp <- data_annual$dividends_annual[2:nrow(data_annual)]
data_annual$dividend_growth_annual <- c(NA, log(dp/d))

# replicate plot from lecture slides
ggplot(data = data_annual, aes(x = date, y = log(1/data_annual$pd_ratio_annual)))+
  geom_line(color = "#00AFBB", size = 2) +
  geom_hline(yintercept = -3.6)


X <- data.frame(dp = log(1/data_annual$pd_ratio_annual), 
                dgr = data_annual$dividend_growth_annual, 
                rf = data_annual$tbill_annualized)
X <- X[-c(1, 2), ] # drop observations with NAs


# create data and run VAR(1)
x = cbind(cmort, tempr, part)
out <- VAR(X, p = 1, type = 'both')
coeftest(out, vcov. = sandwich)
