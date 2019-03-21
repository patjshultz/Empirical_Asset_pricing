rm(list = ls())
library(reshape2)
library(ggplot2)
library(Quandl)
source("functions.R")
theme_set(theme_bw(base_size = 20))

#===============================================================================
# Load and plot data BEA data
#===============================================================================

cons_data <- Quandl(c("FRED/PCNDA", "FRED/PCESVA", "FRED/PCECA"), 
               start_date="1929-01-01",
               end_date="2008-12-31",
               type="raw")
colnames(cons_data) <- c("date", "non_durables", "services", "PCE_level")
cons_data$total <- cons_data$non_durables + cons_data$services
nobs <- nrow(cons_data)

# calculate consumption growth
growth <- log(cons_data[2:nobs, -1] / cons_data[1:(nobs-1), -1])
cons_growth_data <- data.frame(date = cons_data$date[2:nobs],
                               growth  = as.data.frame(growth))
colnames(cons_growth_data) <- colnames(cons_data)
mean(cons_growth_data$total)



plot(cons_growth_data$level, type = "l")
abline(h=0.06, v = NA)

# plot consumption
cons_long <- melt(cons_data, id.vars = "date")
p1 <- ggplot(data = cons_long, aes(x = date, y = log(value), colour = variable))+
  geom_line(size = 1.25) +  xlab("") +   ylab("Log Consumption") + 
  geom_vline(xintercept = as.Date("1950-01-01", format = "%Y-%m-%d")) + 
  xlab("")+ 
  theme(legend.title = element_blank())

  
# calculate and plot consumption growth 
growth_long <- melt(cons_growth_data, id.vars = "date")
p2 <- ggplot(data = growth_long, aes(x = date, y = value, colour = variable))+
  geom_line(size = 1.25) +  xlab("") +   ylab("Consumption growth") + 
  geom_vline(xintercept = as.Date("1950-01-01", format = "%Y-%m-%d")) + 
  xlab("")+ 
  theme(legend.title = element_blank())


g <- gridExtra::arrangeGrob(p1, p2)
ggsave("../Write_up/connsumption_growth.png",g,  width = 10, height = 8)

# post war sample 
PW_ind <- which(cons_growth_data$date > as.Date("1950-01-01"))
PW <- cons_growth_data[PW_ind, ]
mean(PW$non_durables)

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
cor(MP_data$C_growth[3:(nobs_MP)], MP_data$C_growth[2:(nobs_MP-1)])
rho_MP <- -0.14
phi <- (1+rho_MP)/2

#===============================================================================
# Choose numerical values for the model's parameters such that the equilibrium 
# growth process matches that of US annual per-capita growth
#===============================================================================


get_beta(rho = rho_MP, mu = mu_MP, sigma = sigma_MP, alpha = -2)
get_beta(rho = 0.405, mu = 1.019, sigma = 0.032, alpha = -2)
get_beta(rho = 0.320, mu = 1.019, sigma = 0.020, alpha = -2)

# second, find mu, sigma, and rho for the following samples 
sample_a <- cons_growth_data$total

keep_ind <- which(cons_growth_data$date > as.Date("1950-01-01"))
sample_b <- cons_growth_data$total[keep_ind]

mu_F <- 1.019
sigma_F <- 0.032
rho_F <- 0.405
  

mu_PW <- 1.019
sigma_PW <- 0.020
rho_PW <- 0.320

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
beta_F <- get_beta(rho = rho_F, mu = mu_F, sigma = sigma_F, alpha = -2, target_R = 1.05 )
beta_PW <- get_beta(rho = rho_PW, mu = mu_PW, sigma = sigma_PW, alpha = -2, target_R = 1.05 )

MP_stats <- round(c(mu_MP, sigma_MP, rho_MP, beta_MP), 3)
F_stats  <- round(c(mu_F, sigma_F, rho_F, beta_F), 3)
PW_stats <- round(c(mu_PW, sigma_PW, rho_PW, beta_PW), 3)
stats <- rbind(MP_stats, F_stats, PW_stats)
colnames(stats) <- c("mean", "sigma", "rho", "implied beta")
stargazer::stargazer(stats)
