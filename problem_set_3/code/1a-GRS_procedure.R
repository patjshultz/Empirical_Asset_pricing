#===============================================================================
# Finance 921: Assignment 3
# Question 1 part a
#===============================================================================
rm(list = ls())
library(ggplot2)
library(reshape2)
source("functions.R")

# load data
book_market <- read.csv("../data/BEME_returns_monthly.CSV",
                 stringsAsFactors = F, 
                 na.strings = 'NA')[, -1]
size <- read.csv("../data/ME_returns_monthly.CSV", 
               stringsAsFactors = F, 
               na.strings = 'NA')[, -1]
FF_factors <- read.csv("../data/FF_factors.CSV", stringsAsFactors = F)[, -1]
summary_tab <- matrix(data = NA, nrow = 6, ncol = 3)

# take logs of data? 

#book_market <- log(1 + book_market/100)
#size <- log(1 + size/100)
#FF_factors <- log(1 + FF_factors/100)

#===============================================================================
# a) Use GRS statistics to test the CAPM and FF 3 factor model
#===============================================================================

# We first conduct the analysis using the full sample
nobs <- nrow(FF_factors)

# define factors globally to be used in functions
RX <- FF_factors$RE # excess returns of the market
SMB <- FF_factors$SMB; HML <- FF_factors$HML # additional FF factors

#-------------------------- test of CAPM ---------------------------------------

factors <- as.matrix(RX) # only factor is excess return on market
L <- ncol(factors) # only one factor in capm 
N <- 10 # number of test portfolios (i.e. the number of regressions)
bm_estimates <- apply(book_market, 2, return_ols_stats, model= "CAPM")
rownames(bm_estimates) <- c("alpha", "t(alpha)", "beta", "t(beta)")

size_estimates <- apply(size, 2, return_ols_stats, model = "CAPM")

all_estimates <- cbind(t(size_estimates), colMeans(size),
                       t(bm_estimates), colMeans(book_market))
all_estimates <- all_estimates[, c(-2, -4, -7, -9)]
colnames(all_estimates) <- c("alpha", "beta", "E(R)", "alpha", "beta", "E(R)")
stargazer::stargazer(all_estimates, title = "CAPM Test for Size and Book-to-Market Portfolios")

# calculate GRS test statistic and critical value for F-distribution
alphas_bm <- bm_estimates[1, ] # t because it needs to be a column vector
residuals_bm <- apply(book_market, 2, return_ols_residuals, model = "CAPM")
CAPM_BEME_GRS <- GRS(alphas_bm, residuals_bm, factors = RX)

alphas_size <- size_estimates[1, ] # t because it needs to be a column vector
residuals_size <- apply(size, 2, return_ols_residuals, model = "CAPM")
CAPM_size_GRS <- GRS(alphas_size, residuals_size, factors = RX)

critical_value <- qf(.95, df1=N, df2=(nobs-N-L)) 

# allocate to summary stat table
summary_tab[1, 1:3] <- c(CAPM_BEME_GRS, CAPM_size_GRS, critical_value)

#-------------------------- test of FF3 ----------------------------------------

factors <- cbind(FF_factors$RE, FF_factors$SMB, FF_factors$HML)
L <- ncol(factors) # we now have a three factor model rather than just one
N <- 10 # still have the same number of test portfolios

# estimation of FFS model for size and B/M portoflios
bm_estimates <- apply(book_market, 2, return_ols_stats, model = "FF3")
rownames(bm_estimates) <- c("a", "t(a)", "b", "t(b)", "s", "t(s)", "h", "t(h)")
bm_estimates <- rbind(bm_estimates[c(-2, -(4:8)), ], colMeans(book_market))
rownames(bm_estimates) <- c("a", "b", "E(R)")

size_estimates <- apply(size, 2, return_ols_stats, model = "FF3")
rownames(size_estimates) <- c("a", "t(a)", "b", "t(b)", "s", "t(s)", "h", "t(h)")
size_estimates <- rbind(size_estimates[c(-2, -(4:8)), ], colMeans(size))
rownames(size_estimates) <- c("a", "b", "E(R)")

stargazer::stargazer(cbind(t(bm_estimates), t(size_estimates)))


# calculate GRS test statistic and critical value for F-distribution
alphas_bm <- bm_estimates[1, ] # t because it needs to be a column vector
residuals_bm <- apply(book_market, 2, return_ols_residuals, model = "FF3")
FF_BEME_GRS <- GRS(alphas = alphas_bm, residuals = residuals_bm, factors = factors)

alphas_size <- size_estimates[1, ] # t because it needs to be a column vector
residuals_size <- apply(size, 2, return_ols_residuals, model = "FF3")
FF_size_GRS <- GRS(alphas_size, residuals_size, factors)

critical_value <- qf(.95, df1=N, df2=(nobs-N-L)) 
summary_tab[2, 1:3] <- c(FF_BEME_GRS, FF_size_GRS, critical_value)

#===============================================================================
# Repeat analysis for the past 120 months
#===============================================================================

# index of observations to be used
index <- nobs:(nobs-119)

# subset factors 
RX <- FF_factors$RE[index]# excess returns of the market
SMB <- FF_factors$SMB[index]; HML <- FF_factors$HML[index] # additional FF factors

#---------------------------- test of CAPM -------------------------------------
factors <- as.matrix(RX)
L <- ncol(factors) #only factor in capm is systematic risk
N <- 10 # number of test portfolios (i.e. the number of regressions)

# time series regression estimates
bm_estimates <- apply(book_market[index, ], 2, return_ols_stats, model= "CAPM")
rownames(bm_estimates) <- c("alpha", "t(alpha)", "beta", "t(beta)")

size_estimates <- apply(size[index, ], 2, return_ols_stats, model = "CAPM")
rownames(size_estimates) <- c("alpha", "t(alpha)", "beta", "t(beta)")

# calculate GRS test statistic and critical value for F-distribution
alphas_bm <- bm_estimates[1, ] # t because it needs to be a column vector
residuals_bm <- apply(book_market[index, ], 2, return_ols_residuals, model = "CAPM")
CAPM_BEME_GRS <- GRS(alphas_bm, residuals_bm, factors = factors)

alphas_size <- size_estimates[1, ] # t because it needs to be a column vector
residuals_size <- apply(size[index, ], 2, return_ols_residuals, model = "CAPM")
CAPM_size_GRS <- GRS(alphas_size, residuals_size, factors = factors)

critical_value <- qf(.95, df1=N, df2=(nobs-N-L)) 

summary_tab[3, 1:3] <- c(CAPM_BEME_GRS, CAPM_size_GRS, critical_value)

#---------------------------- test of FF3 --------------------------------------
factors <-  cbind(FF_factors$RE, FF_factors$SMB, FF_factors$HML)
L <- ncol(factors) # we now have a three factor model rather than just one
N <- 10 # still have the same number of test portfolios

# estimation of FFS model for size and B/M portoflios
bm_estimates <- apply(book_market[index, ], 2, return_ols_stats, model = "FF3")
rownames(bm_estimates) <- c("a", "t(a)", "b", "t(b)", "s", "t(s)", "h", "t(h)")

size_estimates <- apply(size[index, ], 2, return_ols_stats, model = "FF3")
rownames(size_estimates) <- c("a", "t(a)", "b", "t(b)", "s", "t(s)", "h", "t(h)")

stargazer::stargazer(bm_estimates, title = "Fama-French Three Factor Model Test for B/M portfolios")
stargazer::stargazer(size_estimates,  title = "Fama-French Three Factor Model Test for size portfolios")

# calculate GRS test statistic and critical value for F-distribution
alphas_bm <- bm_estimates[1, ] # t because it needs to be a column vector
residuals_bm <- apply(book_market[index, ], 2, return_ols_residuals, model= "FF3")
FF_BEME_GRS <- GRS(alphas_bm, residuals_bm, factors = factors)

alphas_size <- size_estimates[1, ] # t because it needs to be a column vector
residuals_size <- apply(size[index, ], 2, return_ols_residuals, model = "FF3")
FF_size_GRS <- GRS(alphas_size, residuals_size, factors = factors)

critical_value <- qf(.95, df1=N, df2=(nobs-N-L)) 

summary_tab[4, 1:3] <- c(FF_BEME_GRS, FF_size_GRS, critical_value)

#===============================================================================
# Repeat analysis for the past 25 years
#===============================================================================

index <- nobs:(nobs-25*12)
# subset factors 
RX <- FF_factors$RE[index]# excess returns of the market
SMB <- FF_factors$SMB[index]; HML <- FF_factors$HML[index] # additional FF factors

#---------------------------- test of CAPM -------------------------------------
factors <- as.matrix(RX)
L <- ncol(factors) #only factor in capm is systematic risk
N <- 10 # number of test portfolios (i.e. the number of regressions)

# time series regression estimates
bm_estimates <- apply(book_market[index, ], 2, return_ols_stats, model= "CAPM")
rownames(bm_estimates) <- c("alpha", "t(alpha)", "beta", "t(beta)")

size_estimates <- apply(size[index, ], 2, return_ols_stats, model = "CAPM")
rownames(size_estimates) <- c("alpha", "t(alpha)", "beta", "t(beta)")

# calculate GRS test statistic and critical value for F-distribution
alphas_bm <- bm_estimates[1, ] # t because it needs to be a column vector
residuals_bm <- apply(book_market[index, ], 2, return_ols_residuals, model = "CAPM")
CAPM_BEME_GRS <- GRS(alphas_bm, residuals_bm, factors = factors)

alphas_size <- size_estimates[1, ] # t because it needs to be a column vector
residuals_size <- apply(size[index, ], 2, return_ols_residuals)
CAPM_size_GRS <- GRS(alphas_size, residuals_size, factors = factors)

critical_value <- qf(.95, df1=N, df2=(nobs-N-L)) 

summary_tab[5, 1:3] <- c(CAPM_BEME_GRS, CAPM_size_GRS, critical_value)

#---------------------------- test of FF3 --------------------------------------
factors <-  cbind(FF_factors$RE, FF_factors$SMB, FF_factors$HML)
L <- ncol(factors) # we now have a three factor model rather than just one
N <- 10 # still have the same number of test portfolios

# estimation of FFS model for size and B/M portoflios
bm_estimates <- apply(book_market[index, ], 2, return_ols_stats, model = "FF3")
rownames(bm_estimates) <- c("a", "t(a)", "b", "t(b)", "s", "t(s)", "h", "t(h)")

size_estimates <- apply(size[index, ], 2, return_ols_stats, model = "FF3")
rownames(size_estimates) <- c("a", "t(a)", "b", "t(b)", "s", "t(s)", "h", "t(h)")

stargazer::stargazer(bm_estimates, title = "Fama-French Three Factor Model Test for B/M portfolios")
stargazer::stargazer(size_estimates,  title = "Fama-French Three Factor Model Test for size portfolios")

# calculate GRS test statistic and critical value for F-distribution
alphas_bm <- bm_estimates[1, ] # t because it needs to be a column vector
residuals_bm <- apply(book_market[index, ], 2, return_ols_residuals, model= "FF3")
FF_BEME_GRS <- GRS(alphas_bm, residuals_bm, factors = factors)

alphas_size <- size_estimates[1, ] # t because it needs to be a column vector
residuals_size <- apply(size[index, ], 2, return_ols_residuals, model = "FF3")
FF_size_GRS <- GRS(alphas_size, residuals_size, factors = factors)

critical_value <- qf(.95, df1=N, df2=(nobs-N-L)) 

summary_tab[6, 1:3] <- c(FF_BEME_GRS, FF_size_GRS, critical_value)
colnames(summary_tab) <- c("BE/ME", "Size", "Critical Value")
rownames(summary_tab) <- c("CAPM (Full Sample)", "FF3 (Full Sample)", 
                           "CAPM (Last 10 Years)", "FF3 (Last 10 Years)",
                           "CAPM (Last 25 Years)", "FF3 (Last 25 Years)")
stargazer::stargazer(summary_tab, title = "GRS Statistics")



