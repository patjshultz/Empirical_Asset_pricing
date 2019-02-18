#################################
# Script to Generate IRF graphs #
#################################

impulse_responses <- irf(x = VAR(X, type = "none"), ortho = T)
dgr_shock_to_dp <- data.frame(mean = impulse_responses$irf$dgr[, 1], 
                              lower = impulse_responses$Lower$dgr[, 1], 
                              upper = impulse_responses$Upper$dgr[, 1]) 

dgr_shock_to_dgr <- data.frame(mean = impulse_responses$irf$dgr[, 2], 
                               lower = impulse_responses$Lower$dgr[, 2], 
                               upper = impulse_responses$Upper$dgr[, 2]) 

dgr_shock_to_rf <- data.frame(mean = impulse_responses$irf$dgr[, 3], 
                              lower = impulse_responses$Lower$dgr[, 3], 
                              upper = impulse_responses$Upper$dgr[, 3]) 






######################################
# now plot shocks from dividend yield #
######################################

number_ticks <- function(n) {function(limits) pretty(limits, n)}
lags <- c(1:11)

dgr_shock_dp <-ggplot(data = dgr_shock_to_dp,aes(x = lags,y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=lags, ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("Dividend Growth Shocks") + ggtitle("Dividend Yield") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black')

dgr_shock_dgr <-ggplot(data = dgr_shock_to_dgr, aes(x = lags,y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=lags, ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("") + ggtitle("Dividend Growth") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black')

dgr_shock_rf <-ggplot(data = dgr_shock_to_rf, aes(x = lags,y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=lags, ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("") + ggtitle("Risk free rate") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black')


dp_shock_to_dp <- data.frame(mean = impulse_responses$irf$dp[, 1], 
                              lower = impulse_responses$Lower$dp[, 1], 
                              upper = impulse_responses$Upper$dp[, 1]) 

dp_shock_to_dgr <- data.frame(mean = impulse_responses$irf$dp[, 2], 
                               lower = impulse_responses$Lower$dp[, 2], 
                               upper = impulse_responses$Upper$dp[, 2]) 

dp_shock_to_rf <- data.frame(mean = impulse_responses$irf$dp[, 3], 
                              lower = impulse_responses$Lower$dp[, 3], 
                              upper = impulse_responses$Upper$dp[, 3]) 
number_ticks <- function(n) {function(limits) pretty(limits, n)}
lags <- c(1:11)

dp_shock_dp <-ggplot(data = dp_shock_to_dp,aes(x = lags,y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=lags, ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("Dividend Yield Shocks") + ggtitle("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black')

dp_shock_dgr <-ggplot(data = dp_shock_to_dgr, aes(x = lags,y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=lags, ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("") + ggtitle("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black')

dp_shock_rf <-ggplot(data = dp_shock_to_rf, aes(x = lags,y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=lags, ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("") + ggtitle("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(10)) +
  geom_line(colour = 'black')

gridExtra::grid.arrange(dgr_shock_dp, dgr_shock_dgr,dgr_shock_rf, 
                        dp_shock_dp, dp_shock_dgr,dp_shock_rf, 
                        nrow=2)

##############################################################################
# calculate responses of price leels and returns from dividend growth shocks #
##############################################################################
dgr_exp <- exp(dgr_shock_to_dgr)
dp_exp <- exp(dgr_shock_to_dp) 

# first date outside of loop since we assume P1=1
previous_price <- c(1, 1, 1)
div_previous <- dp_exp[1, ] * previous_price

price_response_to_dgr <- matrix(data = NA, ncol = 3, nrow = 10)
return_response_to_dgr <- matrix(data = NA, ncol = 3, nrow = 10)
colnames(price_response_to_dgr) <- c("mean", "lower", "upper")
colnames(return_response_to_dgr) <- c("mean", "lower", "upper")

for(i in 2:nrow(dgr_exp)){
  dividends_t <- dgr_exp[i, ]* div_previous 
  pd_ratio_t <- 1 / dp_exp[i, ]
  
  prices_t <- pd_ratio_t * dividends_t
  return_t <- ((prices_t + dividends_t) / previous_price ) - 1 
  
  price_response_to_dgr[(i-1), ] <- as.numeric(prices_t)
  return_response_to_dgr[(i-1), ] <- as.numeric(return_t)
  
  div_previous <- dividends_t
  previous_price <- prices_t
}
price_response_to_dgr <- as.data.frame(price_response_to_dgr) 
return_response_to_dgr <- as.data.frame(return_response_to_dgr)

dgr_to_return_response <- ggplot(data = price_response_to_dgr[-1, ], aes(x = c(1:9),y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=c(1:9), ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("Dividend growth shocks") + ggtitle("Prices") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(9)) +
  geom_line(colour = 'black')

dgr_to_price_response <- ggplot(data = return_response_to_dgr, aes(x = c(1:10),y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=c(1:10), ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("") + ggtitle("Returns") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(9)) +
  geom_line(colour = 'black')

###############################################################################
# calculate responses of returns leels and returns from dividend yield shocks #
###############################################################################
dgr_exp <- exp(dp_shock_to_dgr)
dp_exp <- exp(dp_shock_to_dp) 

# first date outside of loop since we assume P1=1
previous_price <- c(1, 1, 1)
div_previous <- dp_exp[1, ] * previous_price

price_response_to_dp <- matrix(data = NA, ncol = 3, nrow = 10)
return_response_to_dp <- matrix(data = NA, ncol = 3, nrow = 10)
colnames(price_response_to_dp) <- c("mean", "lower", "upper")
colnames(return_response_to_dp) <- c("mean", "lower", "upper")

for(i in 2:nrow(dp_exp)){
  dividends_t <- dgr_exp[i, ]* div_previous 
  pd_ratio_t <- 1 / dp_exp[i, ]
  
  prices_t <- pd_ratio_t * dividends_t
  return_t <- ((prices_t + dividends_t) / previous_price ) - 1 
  
  price_response_to_dgr[(i-1), ] <- as.numeric(prices_t)
  return_response_to_dgr[(i-1), ] <- as.numeric(return_t)
  
  div_previous <- dividends_t
  previous_price <- prices_t
}
price_response_to_dp <- as.data.frame(price_response_to_dgr) 
return_response_to_dp <- as.data.frame(return_response_to_dgr)

dp_to_return_response <- ggplot(data = price_response_to_dp, aes(x = c(1:10),y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=c(1:10), ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("Dividend yield shocks") + ggtitle("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(9)) +
  geom_line(colour = 'black')

dp_to_price_response <- ggplot(data = return_response_to_dp, aes(x = c(1:10),y =  mean)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = mean), size = 2)+
  geom_ribbon(aes(x=c(1:10), ymax=upper, ymin=lower), fill="lightblue", alpha=.5) +
  xlab("") + ylab("") + ggtitle("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  scale_x_continuous(breaks=number_ticks(9)) +
  geom_line(colour = 'black')


gridExtra::grid.arrange(dgr_to_return_response, dgr_to_price_response,
                        dp_to_return_response, dp_to_price_response,   
                        nrow=2)
