##################################################
# Question 6: VAR implied regression coefficient #
##################################################

rm(list = ls())
library(ggplot2) 
library(sandwich) 
library(vars)
library(reshape2)
library("VAR.etp")

theme_set(theme_bw(base_size = 18))

######################
# load / format data #
######################
stock_data <- read.csv("../data/data_annual.csv")
stock_data$date <- as.Date(stock_data$date, format = "%Y-%m-%d")
nobs <- nrow(stock_data)

###########################################################
# calculate regression data (log returns, dividend yield) #
###########################################################
cum_div_log_return <- log(stock_data$vwretd + 1)
ex_div_log_return <- log(stock_data$vwretx + 1)
log_real_rf <- log(stock_data$real_rf + 1)
div_yield <- stock_data$div_yield

# specify return horizons in quarters
horizons <- c(1, 3, 5)

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
returns_tau <- matrix(data = NA, ncol = 3, nrow = nrow(stock_data))
colnames(returns_tau) <- c("one-year", "three-year", "five-year")
dgr_tau <- matrix(data = NA, ncol = 3, nrow = nrow(stock_data))
colnames(dgr_tau) <- c("one-year", "three-year", "five-year")
                           
for(h in horizons){
  
  # calculate future returns
  returns <- matrix(data = NA, nrow = length(cum_div_log_return), ncol = 1)
  for(i in 1 :(length(cum_div_log_return)-h)){
    returns[i, 1] <- sum(cum_div_log_return[(i+1):(i+h)])
  }
  returns_tau[, which(horizons %in% h)] <- returns
  
  # calculate dividend growth
  div_growth <- matrix(data = NA, nrow = length(cum_div_log_return), ncol = 1)
  for(i in 1 :(length(cum_div_log_return)-h)){
    div_growth[i, 1] <- log(stock_data$dividends[i+h]) - log(stock_data$dividends[i])
  }
  dgr_tau[, which(horizons %in% h)] <- div_growth
  
  
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

# Generate tables of direct regressions
stargazer::stargazer(summary_mat_returns, colnames = T, 
                     rownames = T, title = "Return Regressions")
stargazer::stargazer(summary_mat_dividends, colnames = T, 
                     rownames = T, title = "Dividend Growth Regressions")

##########################################################
# Run VAR  and calculate implied regression coefficients #
##########################################################
X <- data.frame(dp = stock_data$div_yield,
                dgr = stock_data$div_growth,
                rft = log(1 + stock_data$t90ret))

X <- na.omit(X)


# parameters from CS approx. and derivation of implied coefficients
avg_dp <- mean(stock_data$div_yield)
kappa <- 1 / (1 + exp(avg_dp))
VAR_model <- VAR(X, type = "none")
A <- Bcoef(VAR_model)
A <- A[, 1:3] # drop constant terms
C <- c(-kappa, 1, 0) %*% A + c(1, 0, 0)
e1 <- matrix(data = c(1, 0, 0), nrow = 3, ncol = 1)
e2 <- matrix(data = c(0, 1, 0), nrow = 3, ncol = 1)

# calculate return regression coefficients
A_1 <- diag(3)
A_3 <- diag(3) + A + A^2
A_5 <- diag(3) + A + A^2 + A^3 + A^4

var_X <- var(X)
return_beta1 <- C %*% A_1 %*% var_X %*% e1 / t(e1) %*% var_X %*% e1
return_beta3 <- C %*% A_3 %*% var_X %*% e1 / t(e1) %*% var_X %*% e1
return_beta5 <- C %*% A_5 %*% var_X %*% e1 / t(e1) %*% var_X %*% e1

# the corresponding R squared values are
return_r2_1 <- return_beta1^2 %*% t(e1) %*% var_X %*% e1 / var(returns_tau[, 1], na.rm = T)
return_r2_3 <- return_beta3^2 %*% t(e1) %*% var_X %*% e1 / var(returns_tau[, 2], na.rm = T)
return_r2_5 <- return_beta5^2 %*% t(e1) %*% var_X %*% e1 / var(returns_tau[, 3], na.rm = T)


# calcualte implied div growth coefficients
A_3 <- A + A^2 + A^3
A_5 <- A + A^2 + A^3 + A^4 + A^5

div_beta1 <- t(e2) %*% A %*% var_X %*% e1 / t(e1) %*% var_X %*% e1
div_beta3 <- t(e2) %*% A_3 %*% var_X %*% e1 / t(e1) %*% var_X %*% e1
div_beta5 <- t(e2) %*% A_5 %*% var_X %*% e1 / t(e1) %*% var_X %*% e1

# the corresponding R squared values are
div_r2_1 <- div_beta1^2 %*% t(e1) %*% var_X %*% e1 / var(dgr_tau[, 1], na.rm = T)
div_r2_3 <- div_beta3^2 %*% t(e1) %*% var_X %*% e1 / var(dgr_tau[, 2], na.rm = T)
div_r2_5 <- div_beta5^2 %*% t(e1) %*% var_X %*% e1 / var(dgr_tau[, 3], na.rm = T)

# compare the direct and implied coefficients
return_matrix <- cbind(summary_mat_returns, 
                       c(return_beta1, return_beta3, return_beta5), 
                       c(return_r2_1, return_r2_3, return_r2_5))
dividend_matrix <- cbind(summary_mat_dividends, 
                         c(div_beta1, div_beta3, div_beta5), 
                         c(div_r2_1, div_r2_3, div_r2_5))
stargazer::stargazer(return_matrix)
stargazer::stargazer(dividend_matrix)

# calculate the variation due to returns vs dividend growth
cf_var  <- (kappa * summary_mat_dividends[1,1]) + (kappa^3 * summary_mat_dividends[2,1]) + (kappa^5 * summary_mat_dividends[3,1])
ret_var <- (kappa * summary_mat_returns[1,1]) + (kappa^3 * summary_mat_returns[2,1]) + (kappa^5 * summary_mat_returns[3,1])


################################################################################
# Plot the response of this system to dividend growth an dividend yield shocks #
################################################################################

source("irfs.R")

#############################
# Approximate return shocks #
#############################

eps_1 <- VAR_model$varresult$dp$residuals
eps_2 <- VAR_model$varresult$dgr$residuals
eps_r <- kappa * eps_1 - eps_2

eps_mat <- matrix(data = cbind(eps_1, eps_2, eps_r), nrow = length(eps_1), ncol = 3)
stargazer::stargazer(cor(eps_mat))

##################
# Restricted VAR #
##################
restricted_VAR_data <- data.frame(return = log(1 + stock_data$vwretd), 
                                 div_yield = stock_data$div_yield) 

restriced_VAR <- VAR(restricted_VAR_data, type = "const")  
impulse_responses <- irf(restriced_VAR, ortho = T)

# construct plotting data
return_shock_to_return <- data.frame(mean = impulse_responses$irf$return[, 1], 
                              lower = impulse_responses$Lower$return[, 1], 
                              upper = impulse_responses$Upper$return[, 1]) 

return_shock_to_dp <- data.frame(mean = impulse_responses$irf$return[, 2], 
                               lower = impulse_responses$Lower$return[, 2], 
                               upper = impulse_responses$Upper$return[, 2]) 

dp_shock_to_return <- data.frame(mean = impulse_responses$irf$div_yield[, 1], 
                              lower = impulse_responses$Lower$div_yield[, 1], 
                              upper = impulse_responses$Upper$div_yield[, 1]) 

dp_shock_to_dp <- data.frame(mean = impulse_responses$irf$div_yield[, 2], 
                                 lower = impulse_responses$Lower$div_yield[, 2], 
                                 upper = impulse_responses$Upper$div_yield[, 2]) 
# plot responses =================================================================
r_shock_r_plot <-ggplot(data = return_shock_to_return, aes(x = lags,y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=lags, ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("Return shocks") + ggtitle("Return") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black')

r_shock_dp_plot <-ggplot(data = return_shock_to_dp, aes(x = lags,y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=lags, ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("") + ggtitle("Dividend Yield") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black')

dp_shock_r_plot <-ggplot(data = dp_shock_to_return, aes(x = lags,y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=lags, ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("Div. Yield Shocks") + ggtitle("Return ") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black')

dp_shock_dp_plot <-ggplot(data = dp_shock_to_dp, aes(x = lags,y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=lags, ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("Div Yield") + ggtitle("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black')
gridExtra::grid.arrange(r_shock_r_plot, r_shock_dp_plot,
                        dp_shock_r_plot, dp_shock_dp_plot,
                        nrow = 2)




###################################################
# Bootstrap first equation of this restricted VAR #
###################################################

n_iter <- 100     # Sample Size
n_sample <- 100      # Number of samples
boot_VAR_coef <- vector(mode = "double", length = n_sample)

# Loop through iterations (warning: takes about 4 mins)  49 mins if 1000 iter, 100 samples
for (i in 1:n_sample){
  estimated_boot_VAR <- VAR.Boot(restricted_VAR_data, p = 1, nb = n_iter, type = "const")
  boot_VAR_coef[i]   <- estimated_boot_VAR$coef[1,2]
}

# Plot a histogram of bootstrapped coefficients
hist(boot_VAR_coef, breaks = 30)


################################################################
# News Decomposition using approximate price-dividend identity #
################################################################
pd <- log(stock_data$prices / stock_data$dividends)
dg <- stock_data$div_growth
r <- log(1 + stock_data$vwretd)

# Demean all the variables
z <- pd - mean(pd, na.rm = T)
g <- dg - mean(dg, na.rm = T)
r <- r - mean(r, na.rm = T)

# Compute terms ahead of time
e3 <- c(0,0,1); e2 <- c(0,1,0)

# Fit VAR
X <- na.omit(cbind(z, g, r))
VAR_model <- VAR(X, type = 'none')
A <- Bcoef(VAR_model)
x  <- nrow(X)

# Preallocate
cf_news <- rep(0, x-1)
r_news  <- rep(0, x-1)
total   <- rep(0, x-1)

# Use the VAR coefficients
avg_pd <- mean(pd, na.rm = T)
kappa1 <- exp(avg_pd) / (1 + exp(avg_pd))
inv <- solve(diag(3) - kappa1 * A)
for (i in 2:x) {
  u  <- X[i,] - A %*% t(t(X[i-1,]))
  total[i-1] <- u[3]
  cf_news[i-1] <- t(e2) %*% inv %*% u
  r_news[i-1] <- kappa1 * (t(e3) %*% A %*% inv %*% u)
}

# if quarterly news_df <- data.frame(date = stock_data$date[c(-1, -2, -3)], total = total, cf_news = cf_news, r_news = r_news)
news_df <- data.frame(date = stock_data$date[c(-1, -2)], total = total, cf_news = cf_news, r_news = r_news)

# Create plot
df_melt <- melt(news_df, id.vars = "date")
ggplot(data=df_melt,
       aes(x=date, y=value, colour=variable)) +
  geom_line(size = 1) + ggtitle("News Decomposition") + ylab("") + xlab("") + geom_point()


# Average absolute error of decomposition:
mean(abs(total - cf_news + r_news))

################################
# Question 10 CS decomposition #
################################

A1 <- Bcoef(VAR_model)

## Question 10: Creating the invidual elements of the decomposition
nobs <- nrow(X)
kappa <- 1 / (1 + exp(avg_pd))
inv <- solve(diag(3)-kappa*A1)
K <- c(kappa,1,0)

E_tlag_r = K%*%A1%*%t(X)-c(1,0,0)%*%t(X)
E_tlag_r = E_tlag_r[1:(nobs-1)]


#dividends
E_t_g <- t(X[2:nobs,2])
E_tlag_g <- c(0,1,0)%*%A1%*%t(X[1:(nobs-1),])

E_t_sumg <- c(0,1,0)%*%A1%*%inv%*%t(X[2:(nobs),])
E_tlag_sumg <- c(0,1,0)%*%A1%*%inv%*%t(X[1:(nobs-1),])

# returns
E_t_sumr <- (t(K) %*% inv %*% A1-c(1,0,0)%*%inv) %*% t(X[2:(nobs),])
E_tlag_sumr <- (K %*% inv %*% A1-c(1,0,0) %*% inv) %*% t(X[1:(nobs-1),])

epsilon <- E_t_g - E_tlag_g + E_t_sumg - E_tlag_sumg - (E_t_sumr - E_tlag_sumr)
dr_news <- (E_t_sumr - E_tlag_sumr)
cf_news <- E_t_g - E_tlag_g + E_t_sumg - E_tlag_sumg

decomp_df <- data.frame(date = stock_data$date[c(-1, -2)], 
                        total = as.numeric(epsilon),
                        cf_news = as.numeric(cf_news), 
                        r_news = as.numeric(dr_news))
df_melt <- melt(decomp_df, id.vars = "date")

ggplot(data=df_melt,
       aes(x=date, y=value, colour=variable)) +
  geom_line(size = 1) + ggtitle("Return Decomposition") + ylab("") + xlab("") + geom_point()


###################################
# Question 11 Rolling Regressions #
###################################
library(zoo)
library(seasonal)
library(vars)
library(stargazer)

### Question 11

## Question 11: Set-up

qrtly_data <- read.csv("../data/OUTPUT.csv",stringsAsFactors = F)
colnames(qrtly_data)[colnames(qrtly_data)=="my"] <- "date"

qrtly_data$date <- gsub("m3", "-1", qrtly_data$date)
qrtly_data$date <- gsub("m6", "-2", qrtly_data$date)
qrtly_data$date <- gsub("m9", "-3", qrtly_data$date)
qrtly_data$date <- gsub("m12", "-4", qrtly_data$date)
qrtly_data$date <- as.yearqtr(qrtly_data$date)

lagcumfun<-function(x,lag){
  # produces vector of cumsums of the most recent [lag] values of x where [lag] incl. today
  # note that time series needs to be sorted s.t. newest value at the end
  y = matrix(rep(NA,(length(x)*lag)),length(x),lag) #pre-allocate memory
  for (i in lag:1){
    y[,i] <- c(x[c(i:(length(x)))],rep(NA,(i-1)))
  }
  z <- rowSums(y[c(1:(length(x)-(lag-1))),])
  return(c(rep(NA,(lag-1)),z))
}

seasongrowfun<-function(x,lag){
  # produces vector of current x divided by [lag] lagged x
  # note that time series needs to be sorted s.t. newest value at the end
  y = matrix(rep(NA,(length(x)*2)),length(x),2) #pre-allocate memory
  y[,1] <- x
  y[,2] <- c(x[c((lag+1):(length(x)))],rep(NA,(lag)))
  z <- y[,2]/y[,1]
  return(c(rep(NA,lag),z[(1:(length(x)-lag))]))
}

qrtly_data$rt <- log(1+qrtly_data$vwretd)
qrtly_data$ltmDt <- lagcumfun(qrtly_data$Dt,4) # last twelve month dividends, for D/P ratios
acf(log(seasongrowfun(qrtly_data$Dt,1)),4,na.action = na.pass) # see annual div cycle: 4th phi up
qrtly_data$yoyG <- log(seasongrowfun(qrtly_data$ltmDt,1)) # seasonally adj dividend growth: sum (ltm d_t) / sum (ltm d_(t-1))
acf(qrtly_data$yoyG,4,na.action = na.pass) # check for autocorrelation (=seasonality) in Div growth

### Note that for out of sample predictions ex ante inflation smoothing is non-sensical
### because a trader would also only have the previous expectations
qrtly_data$rf3m <- 0.25*log(1+qrtly_data$tb3ms/100) #-qrtly_data$exp_inflation
plot(qrtly_data$date,qrtly_data$rf3m,"l")

## Question 11: Estimating the VAR(1) on a rolling basis

VAR_data <- cbind(log(qrtly_data$ltmDt/qrtly_data$Pt), #-lagcumfun(qrtly_data$inflation_season,4),
                  qrtly_data$yoyG,
                  qrtly_data$rf3m)
na_count <- sapply(data.frame(VAR_data), function(y) sum((is.na(y))))
VAR_data <- VAR_data[(max(na_count)+1):nrow(VAR_data),]
VAR_data <- data.frame(VAR_data)
colnames(VAR_data) <- c('dp','g','rf')

var1c_coeff <- list()
roll_mean <- list()
r_fcast <- matrix(rep(0,(nrow(VAR_data)-20+1)),(nrow(VAR_data)-20+1),1)
# -20 because that is the rolling VAR period, +1 because we produce one forecast beyond our data

for (i in 20:nrow(VAR_data)){
  var1c <- VAR(VAR_data[((i-19):i),],p = 1,type = "const")
  var1c_coeff[[i]] <- Bcoef(var1c)
  a <- matrix(var1c_coeff[[i]],3,4)
  b <- t(as.matrix(cbind(VAR_data[i,],1),1,4))
  if(sum(is.na(a)) >0){ # accounting for WWII constant 3m treasury rates
    var1c <- VAR(VAR_data[((i-19):i),(1:2)],p = 1,type = "const")
    var1c_coeff[[i]] <- Bcoef(var1c)
    a <- matrix(var1c_coeff[[i]],2,3)
    b <- t(as.matrix(cbind(VAR_data[i,(1:2)],1),1))
  }
  
  ## Question 11: Forecasting out of sample returns via CS decomposition
  
  CSparameters <- a%*%b
  roll_mean[[i]] <- colMeans(VAR_data[((i-19):i),])
  
  z_bar <- (-1)*roll_mean[[i]][1] # note that in CS z = p-d vs. our VAR: d-p
  kappa1 <- exp(z_bar) / (1+exp(z_bar)) 
  kappa0 <- log(1+exp(z_bar)) - z_bar * kappa1
  r_fcast[i-19,1] <- kappa0 + kappa1 * (-1)*CSparameters[1] + CSparameters[2] - (-1)*VAR_data[i,1]
}


plot_df <- data.frame(date = qrtly_data$date[-(1:39)], r = qrtly_data$rt[-(1:39)], Forecast = r_fcast)
df_melt <- melt(plot_df, id.vars = "date")

ggplot(data=df_melt,
       aes(x=date, y=value, colour=variable)) +
  geom_line(size = 1) + ggtitle("Rolling forecast") + ylab("") + xlab("") + geom_point()

RMSE <- (1/(nrow(r_fcast)-1)*
           sum((
             r_fcast[1:(nrow(r_fcast)-1)] -
               qrtly_data$rt[(nrow(qrtly_data)-(nrow(r_fcast)-1)+1):nrow(qrtly_data)]
           )^2))^0.5
VarR <- var(r_fcast[1:(nrow(r_fcast)-1)]) /
  var(qrtly_data$rt[(nrow(qrtly_data)-(nrow(r_fcast)-1)+1):nrow(qrtly_data)])
linreg <- lm(qrtly_data$rt[(nrow(qrtly_data)-(nrow(r_fcast)-1)+1):nrow(qrtly_data)] ~
               r_fcast[1:(nrow(r_fcast)-1)])
R2 <- summary(linreg)$r.squared
output <- data.frame(round(RMSE,3),round(VarR,3),round(R2,3))
colnames(output) <- c("RMSE","Var Ratio", "Implied R2")
