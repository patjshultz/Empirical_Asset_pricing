#===============================================================================
# Finance 921: Assignemnt 3
# Question 1
# Author: Patrick Shultz
#===============================================================================
library(ggplot2)
library(reshape2)
source("functions.R")
theme_set(theme_bw(base_size = 20))

# load data
book_market <- read.csv("../data/BEME_value_weighted_deciles.CSV",
                 stringsAsFactors = F, 
                 na.strings = 'NA')[, -1]
size <- read.csv("../data/ME_value_weighted_deciles.CSV", 
               stringsAsFactors = F, 
               na.strings = 'NA')[, -1]
FF_factors <- read.csv("../data/FF_factors.CSV", stringsAsFactors = F)[, -1]

# plot data
matplot(book_market, type = "l")
matplot(size, type = "l")
matplot(FF_factors, type = "l")

#===============================================================================
# a) Use GRS statistics to test the CAPM and FF 3 factor model
#===============================================================================

# define factors globally to be used in functions
RX <- FF_factors$RE # excess returns of the market
SMB <- FF_factors$SMB; HML <- FF_factors$HML # additional FF factors

# test of CAPM -----------------------------------------------------------------

L <- 1 #only factor in capm is systematic risk
N <- 10 # number of test portfolios (i.e. the number of regressions)
bm_estimates <-apply(book_market, 2, return_ols_stats, model= "CAPM")
rownames(bm_estimates) <- c("alpha", "t(alpha)", "beta", "t(beta)")

size_estimates <- apply(size, 2, return_ols_stats)
rownames(size_estimates) <- c("alpha", "t(alpha)", "beta", "t(beta)")

all_estimates <- cbind(t(size_estimates), t(bm_estimates))
stargazer::stargazer(all_estimates, title = "CAPM Test for Size and Book-to-Market Portfolios")

# calculate GRS test statistic and critical value for F-distribution
alphas_bm <- bm_estimates[1, ] # t because it needs to be a column vector
residuals_bm <- apply(book_market, 2, return_ols_residuals)
GRS(alphas_bm, residuals_bm)

alphas_size <- size_estimates[1, ] # t because it needs to be a column vector
residuals_size <- apply(size, 2, return_ols_residuals)
GRS(alphas_size, residuals_size)

critical_value <- qf(.95, df1=N, df2=(nobs-N-L)) 
curve(df(x, df1=N, df2=(nobs-N-L)), from = 0, to = 50)
abline(v = critical_value, lty = 2)
abline(v = as.numeric(GRS), lty = 2)

# test of FF3 ------------------------------------------------------------------
L <- 3 # we now have a three factor model rather than just one
N <- 10 # still have the same number of test portfolios

# estimation of FFS model for size and B/M portoflios
bm_estimates <- apply(book_market, 2, return_ols_stats, model = "FF3")
rownames(bm_estimates) <- c("a", "t(a)", "b", "t(b)", "s", "t(s)", "h", "t(h)")

size_estimates <- apply(size, 2, return_ols_stats, model = "FF3")
rownames(size_estimates) <- c("a", "t(a)", "b", "t(b)", "s", "t(s)", "h", "t(h)")

stargazer::stargazer(bm_estimates)
stargazer::stargazer(size_estimates)

# calculate GRS statistic

