rm(list = ls())
library(reshape2)
library(ggplot2)
library(Quandl)
source("functions.R")
theme_set(theme_bw(base_size = 20))

#===============================================================================
# Load and plot data 
#===============================================================================

cons_data <- Quandl(c("FRED/PCNDA", "FRED/PCESVA"), 
               start_date="1929-01-01",
               end_date="2008-12-31",
               type="raw")
colnames(cons_data) <- c("date", "non_durables", "services")
cons_data$total <- cons_data$non_durables + cons_data$services
nobs <- nrow(cons_data)

# calculate consumption growth

growth <- lapply(cons_data[, -1], FUN = pch)
cons_growth_data <- data.frame(date = cons_data$date[2:nobs],
                               growth  = as.data.frame(growth))
colnames(cons_growth_data) <- colnames(cons_data)

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

#===============================================================================
# Choose numerical values for the model's parameters such that the equilibrium 
# growth process matches that of US annual per-capita growth
#===============================================================================

# first use the original Mehra and Prescott numbers 
mean_MP <- 0.018; sd_MP <- 0.036; ac_MD <- -0.14

lambda <- 

# second, find mu, sigma, and rho for the following samples 
sample_a <- cons_growth_data$total

keep_ind <- which(cons_growth_data$date > as.Date("1950-01-01"))
sample_b <- cons_growth_data$total[keep_ind]

mu_a <- mean(sample_a)
sigma_a <- sd(sample_a)
rho_a <- corr()

#===============================================================================
# a) Markov chains
#===============================================================================
