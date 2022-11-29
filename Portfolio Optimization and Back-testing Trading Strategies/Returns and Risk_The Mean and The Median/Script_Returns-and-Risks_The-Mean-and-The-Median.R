## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.align="center", message = FALSE, warning = FALSE)
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages------------------------------------------------------------
library(doParallel) # For parallel computation in foreach loops
library(PortfolioAnalytics) # For portfolio optimization and analysis
library(RColorBrewer) # For color palettes in plots
library(tidyquant) # For quantmod and PerformanceAnalytics functions
library(tidyverse) # For dplyr and ggplot2 functions (data manipulation and plotting)

## ----retrieve price data------------------------------------------------------
# Vector of tickers to include in portfolio
tickers <- c("PG", "WMT", "BKNG", "CRM", "MMM", "SBUX", "DIS", "HD", "KO", "NVDA")

startdate <- as.Date("2015-01-01")
enddate <- as.Date("2022-07-01")

price_data <- NULL

# Loop to get adjusted closing prices for all stocks
for(t in tickers) {
  price_data <- cbind(price_data,
                      quantmod::getSymbols(Symbols = t, src = "yahoo", auto.assign = FALSE,
                                           from = startdate, to = enddate, periodicity = "daily") %>% Ad())
}

# Check dimension of object, start and end date of data collected
dim(price_data); start(price_data); end(price_data)

# See first 6 observations in price_data
data.frame(head(price_data))

## ----calculate discrete returns-----------------------------------------------
return_data <- na.omit(PerformanceAnalytics::Return.calculate(prices = price_data, method = "discrete")) %>%
  `colnames<-`(paste("R", tickers, sep = "_"))

dim(return_data); data.frame(head(return_data))

## ----plot historical returns of WMT-------------------------------------------
chart.Histogram(R = return_data$R_PG, 
                method = c("add.density", "add.normal", "add.qqplot"), 
                main = "Density Plot of PG Historical Returns")

legend(x = "topright", legend = c("Density Plot", "Normal Distribution"), lwd = 2, col = c("darkblue", "blue"))

## ----mean and median of each stock--------------------------------------------
stock_means <- apply(X = return_data, MARGIN = 2, FUN = mean)

stock_medians <- apply(X = return_data, MARGIN = 2, FUN = median)

data.frame(rbind(Mean = stock_means, Median = stock_medians))

## ----standard deviation of each stock-----------------------------------------
stock_sd <- apply(X = return_data, MARGIN = 2, FUN = sd)

data.frame(rbind(SD = stock_sd))

## ----MAD around mean and median of each stock---------------------------------
stock_MAD <- apply(X = return_data, MARGIN = 2, FUN = function(x) {
  mean(abs(x - mean(x)))
})

stock_MADmed <- apply(X = return_data, MARGIN = 2, FUN = function(x) {
  mean(abs(x - median(x)))
})

data.frame(rbind(MAD_mean = stock_MAD, MAD_median = stock_MADmed))

## ----MeAD of each stock-------------------------------------------------------
stock_MeAD <- apply(X = return_data, MARGIN = 2, FUN = function(x) {
  median(abs(x - median(x)))
})

data.frame(rbind(MeAD = stock_MeAD))

## ----summary of section 4-----------------------------------------------------
data.frame(rbind(Mean = stock_means, Median = stock_medians, 
                 SD = stock_sd, MAD_mean = stock_MAD, MAD_median = stock_MADmed, MeAD = stock_MeAD))

## ----generate set of random portfolios----------------------------------------
portspec <- PortfolioAnalytics::portfolio.spec(assets = tickers)

# Sum of weights constrained to 1, can also specify as type = "full investment"
portspec <- PortfolioAnalytics::add.constraint(portfolio = portspec,
                                               type = "weight_sum",
                                               min_sum = 1, max_sum = 1)

# Weight of each portfolio component can vary between minimum of 0% and maximum of 100%
portspec <- PortfolioAnalytics::add.constraint(portfolio = portspec,
                                               type="box", 
                                               min = 0, max = 1)

portspec

set.seed(43594)

rand_port <- PortfolioAnalytics::random_portfolios(portfolio = portspec, 
                                                   permutations = 50000, 
                                                   rp_method = "sample", 
                                                   eliminate = TRUE)

dim(rand_port); head(rand_port)

## ----optimization and return period-------------------------------------------
opt_periods <- c("2015", "2015-07/2016-06", 
                 "2016", "2016-07/2017-06", 
                 "2017", "2017-07/2018-06", 
                 "2018", "2018-07/2019-06", 
                 "2019", "2019-07/2020-06", 
                 "2020", "2020-07/2021-06", 
                 "2021")

ret_periods <- c("2016-01/2016-06", "2016-07/2016-12", 
                 "2017-01/2017-06", "2017-07/2017-12",
                 "2018-01/2018-06", "2018-07/2018-12",
                 "2019-01/2019-06", "2019-07/2019-12",
                 "2020-01/2020-06", "2020-07/2020-12",
                 "2021-01/2021-06", "2021-07/2021-12",
                 "2022-01/2022-06")

data.frame(cbind(Optimization_Period = opt_periods, Return_Period = ret_periods))

## ----random portfolio returns-------------------------------------------------
rp_returns <- foreach(i = 1:nrow(rand_port), .combine = "cbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data,
                                                weights = rand_port[i, ],
                                                geometric = TRUE,
                                                rebalance_on = "quarters")
}

## ----return of equal weight portfolio-----------------------------------------
# If do not include weights, equal weight portfolio is assumed
ewp_return <- PerformanceAnalytics::Return.portfolio(R = return_data,
                                                     geometric = TRUE,
                                                     rebalance_on = "quarters")

## ----find weights for best mean return----------------------------------------
maxmean_weight <- foreach(i = opt_periods, .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Mean.arithmetic(x = rp_returns[i, ])
  
  opt_weight <- rand_port[which.max(tmp), ]
}

rownames(maxmean_weight) <- paste("OP", 1:nrow(maxmean_weight), sep = "")

data.frame(maxmean_weight)

## ----daily return of best mean portfolio--------------------------------------
# Returns of best mean portfolio in ret_period using weights from opt_period
maxmean_returns <- foreach(i = ret_periods, j = 1:nrow(maxmean_weight), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data[i, ], 
                                                weights = maxmean_weight[j, ], 
                                                geometric = TRUE, 
                                                rebalance_on = "quarters")
}

## ----find weights for best median return--------------------------------------
# Find weights that maximizes median of each optimization period
maxmed_weight <- foreach(i = opt_periods, .combine = "rbind") %do% {
  tmp <- apply(X = rp_returns[i, ], MARGIN = 2, FUN = median)
  
  opt_weight <- rand_port[which.max(tmp), ]
}

rownames(maxmed_weight) <- paste("OP", 1:nrow(maxmed_weight), sep = "")

data.frame(maxmed_weight)

## ----daily return of best median portfolio------------------------------------
# Returns of best median portfolio in ret_period using weights from opt_period
maxmed_returns <- foreach(i = ret_periods, j = 1:nrow(maxmed_weight), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data[i, ], 
                                                weights = maxmed_weight[j, ], 
                                                geometric = TRUE, 
                                                rebalance_on = "quarters")
}

## ----plot weights of best return portfolios-----------------------------------
chart.StackedBar(w = maxmean_weight, colorset = RColorBrewer::brewer.pal(n = 10, "Spectral"),
                 main = "Optimal Weights of Best Mean Portfolio", ylab = "Weight")

chart.StackedBar(w = maxmed_weight, colorset = RColorBrewer::brewer.pal(n = 10, "Spectral"),
                 main = "Optimal Weights of Best Median Portfolio", ylab = "Weight")

## ----plot returns of best return portfolios-----------------------------------
best_return <- cbind(maxmean_returns, maxmed_returns, ewp_return["2016/",]) %>%
  `colnames<-`(c("Best_Mean", "Best_Median", "Equal Weight"))

chart.CumReturns(R = best_return, geometric = TRUE,
                 legend.loc = "topleft",
                 main = "Cumulative Return of Best Return Portfolios")

chart.Drawdown(R = best_return, geometric = TRUE,
               legend.loc = "bottomleft",
               main = "Drawdown of Best Return Portfolios")

## ----perf summary of best return portfolios-----------------------------------
table.AnnualizedReturns(R = best_return, scale = 252, Rf = 0.03/252, geometric = TRUE)

table.DownsideRisk(R = best_return, scale = 252, Rf = 0.03/252, MAR = 0.08/252)

## ----find weights for minimum variance----------------------------------------
minstd_weight <- foreach(i = opt_periods, .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::StdDev(R = rp_returns[i, ])
  
  opt_weight <- rand_port[which.min(tmp), ]
}

rownames(minstd_weight) <- paste("OP", 1:nrow(minstd_weight), sep = "")

data.frame(minstd_weight)

## ----daily return of minimum variance portfolio-------------------------------
minstd_returns <- foreach(i = ret_periods, j = 1:nrow(minstd_weight), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data[i, ], 
                                                weights = minstd_weight[j, ], 
                                                geometric = TRUE, 
                                                rebalance_on = "quarters")
}

## ----find weights for minimum mean absolute deviation-------------------------
minmad_weight <- foreach(i = opt_periods, .combine = "rbind") %do% {
  tmp <- apply(X = rp_returns[i, ], MARGIN = 2, FUN = function(x) {
    mean(abs(x - mean(x)))
    })
  
  opt_weight <- rand_port[which.min(tmp), ]
}

rownames(minmad_weight) <- paste("OP", 1:nrow(minmad_weight), sep = "")

data.frame(minmad_weight)

## ----daily return of minimum mad portfolio------------------------------------
minmad_returns <- foreach(i = ret_periods, j = 1:nrow(minmad_weight), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data[i, ], 
                                                weights = minmad_weight[j, ], 
                                                geometric = TRUE, 
                                                rebalance_on = "quarters")
}

## ----find weights for minimum median absolute deviation-----------------------
minmead_weight <- foreach(i = opt_periods, .combine = "rbind") %do% {
  tmp <- apply(X = rp_returns[i, ], MARGIN = 2, FUN = function(x) {
    median(abs(x - median(x)))
    })
  
  opt_weight <- rand_port[which.min(tmp), ]
}

rownames(minmead_weight) <- paste("OP", 1:nrow(minmead_weight), sep = "")

data.frame(minmead_weight)

## ----daily return of minimum mead portfolio-----------------------------------
minmead_returns <- foreach(i = ret_periods, j = 1:nrow(minmead_weight), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data[i, ], 
                                                weights = minmead_weight[j, ], 
                                                geometric = TRUE, 
                                                rebalance_on = "quarters")
}

## ----plot weights of minimum risk portfolios----------------------------------
chart.StackedBar(w = minstd_weight, colorset = RColorBrewer::brewer.pal(n = 10, "Spectral"),
                 main = "Optimal Weights of Minimum Variance Portfolio", ylab = "Weight")

chart.StackedBar(w = minmad_weight, colorset = RColorBrewer::brewer.pal(n = 10, "Spectral"),
                 main = "Optimal Weights of Minimum MAD Portfolio", ylab = "Weight")

chart.StackedBar(w = minmead_weight, colorset = RColorBrewer::brewer.pal(n = 10, "Spectral"),
                 main = "Optimal Weights of Minimum MeAD Portfolio", ylab = "Weight")

## ----plot returns of minimum risk portfolios----------------------------------
min_risk <- cbind(minstd_returns, minmad_returns, minmead_returns, ewp_return["2016/",]) %>%
  `colnames<-`(c("Min_Var", "Min_MAD", "Min_MeAD", "Equal Weight"))

chart.CumReturns(R = min_risk, geometric = TRUE,
                 legend.loc = "topleft",
                 main = "Cumulative Return of Minimum Risk Portfolios")

chart.Drawdown(R = min_risk, geometric = TRUE,
               legend.loc = "bottomleft",
               main = "Drawdown of Minimum Risk Portfolios")

## ----perf summary of minimum risk portfolios----------------------------------
table.AnnualizedReturns(R = min_risk, scale = 252, Rf = 0.03/252, geometric = TRUE)

table.DownsideRisk(R = min_risk, scale = 252, Rf = 0.03/252, MAR = 0.08/252)

