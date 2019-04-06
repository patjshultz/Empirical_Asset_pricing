rm(list = ls())
library(reshape2)
library(ggplot2)
library(quantmod)
source("functions.R")
theme_set(theme_bw(base_size = 20))

#===============================================================================
# Load and calculate summary stats of BEA data
#===============================================================================

getSymbols(c("DDURRA3A086NBEA", "DSERRA3A086NBEA"), src='FRED')
dates <- index(DDURRA3A086NBEA)
consumption <- as.numeric(DDURRA3A086NBEA + DSERRA3A086NBEA)
nobs <- length(consumption)

# Data from Alejandro
cdata <- read.csv("../data/consumption_data.csv", stringsAsFactors = F)
dates <- as.Date(cdata$observation_date, format = "%m/%d/%Y")
bea_cg <- as.numeric(na.omit(cdata$AG_EOY)) # year over year growth, at the end of year
nobs <- length(bea_cg)

# calculate consumption growth
#bea_cg <- log(consumption[2:nobs] / consumption[1:(nobs-1)])
plot(bea_cg, type = "l")
abline(h = mean(bea_cg))

# Full sample moments
mu_F <- mean(bea_cg)
sigma_F <- sd(bea_cg)
rho_F <- cor(bea_cg[2:(nobs-1)], bea_cg[1:(nobs-2)])

# post war sample 
PW_ind <- which(dates > as.Date("1950-01-01"))
PW_cg <- as.numeric(na.omit(cdata$AG_EOY[PW_ind]))
nobs_PW <- length(PW_cg)

# Post war moments
mu_PW <- mean(PW_cg)
sigma_PW <- sd(PW_cg)
rho_PW <- cor(bea_cg[2:nobs_PW], bea_cg[1:(nobs_PW-1)])

#===============================================================================
# original Mehra Prescott data
#===============================================================================

MP_data <- read.table("../data/MP_original_data.txt", header = T)
nobs_MP <- nrow(MP_data)
MP_data$C_growth <- c(NA, diff(log(MP_data$C)))
MP_data$C_growth <- c(NA, MP_data$C[2:nobs_MP]/MP_data$C[1:(nobs_MP-1)])

# calculate summary statistics
mu_MP <- mean(MP_data$C_growth, na.rm = T)
sigma_MP <- sd(MP_data$C_growth, na.rm = T)
rho_MP <- -0.14
phi <- (1+rho_MP)/2

#===============================================================================
# Choose numerical values for the model's parameters such that the equilibrium 
# growth process matches that of US annual per-capita growth
#===============================================================================


get_beta(rho = rho_MP, mu = mu_MP, sigma = sigma_MP, alpha = -2)
get_beta(rho = rho_F, mu = 1 + mu_F, sigma = sigma_F, alpha = -2)
get_beta(rho = rho_PW, mu = 1 + mu_PW, sigma = sigma_PW, alpha = -2)


#===============================================================================
# a) Markov chains
#===============================================================================

# specify parameters and transition matrix
mu <- mean_MP + 1; sigma <- sd_MP; phi <- 0.43
Pi <- matrix(data = c(phi, 1- phi, 1 - phi,  phi), nrow = 2)


# iii. Solve for the unconditional means 
iota <- matrix(data = 1, nrow = 2, ncol = 1)
A <- rbind(diag(2) - Pi, t(iota))
e3 <- matrix(data = c(0, 0, 1), nrow = 3, ncol = 1)

Pi_star <- solve((t(A) %*% A)) %*% t(A) %*% e3

#===============================================================================
# b) Term Structure of Interest Rates
#===============================================================================

beta_MP <- get_beta(rho = rho_MP, mu = mu_MP, sigma = sigma_MP, alpha = -2, target_R = 1.05 )
beta_F <- get_beta(rho = rho_F, mu = 1+mu_F, sigma = sigma_F, alpha = -2, target_R = 1.05 )
beta_PW <- get_beta(rho = rho_PW, mu = 1+mu_PW, sigma = sigma_PW, alpha = -2, target_R = 1.05 )

MP_stats <- round(c(mu_MP, sigma_MP, rho_MP, beta_MP), 3)
F_stats  <- round(c(mu_F, sigma_F, rho_F, beta_F), 3)
PW_stats <- round(c(mu_PW, sigma_PW, rho_PW, beta_PW), 3)
stats <- rbind(MP_stats, F_stats, PW_stats)
colnames(stats) <- c("mean", "sigma", "rho", "implied beta")
stargazer::stargazer(stats)
