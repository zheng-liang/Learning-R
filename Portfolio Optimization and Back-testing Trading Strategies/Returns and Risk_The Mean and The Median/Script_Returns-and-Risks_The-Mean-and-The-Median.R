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
                                           from = startdate, to = enddate, periodicity = "weekly") %>% Ad())
}

# Check dimension of object, start and end date of data collected
dim(price_data); start(price_data); end(price_data)

# See first 6 observations in price_data
head(price_data)

## ----calculate discrete returns-----------------------------------------------
return_data <- na.omit(PerformanceAnalytics::Return.calculate(prices = price_data, method = "discrete")) %>%
  `colnames<-`(paste(tickers, "Return", sep = "."))

dim(return_data); head(return_data)

## ----plot historical returns of WMT-------------------------------------------
chart.Histogram(R = return_data$PG.Return, 
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

# Weight of each portfolio component can vary between minimum of 0% and maximum of 25%
portspec <- PortfolioAnalytics::add.constraint(portfolio = portspec,
                                               type="box", 
                                               min = 0, max = 0.25)

portspec

set.seed(43594)

rand_port <- PortfolioAnalytics::random_portfolios(portfolio = portspec, 
                                                   permutations = 20000, 
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

## ----weekly return of best mean portfolio-------------------------------------
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

## ----weekly return of best median portfolio-----------------------------------
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
table.AnnualizedReturns(R = best_return, scale = 52, Rf = 0.025/52, geometric = TRUE)

table.DownsideRisk(R = best_return, scale = 52, Rf = 0.025/52, MAR = 0.07/52)

table.Stats(R = best_return)

## ----find weights for minimum variance----------------------------------------
minstd_weight <- foreach(i = opt_periods, .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::StdDev(R = rp_returns[i, ])
  
  opt_weight <- rand_port[which.min(tmp), ]
}

rownames(minstd_weight) <- paste("OP", 1:nrow(minstd_weight), sep = "")

data.frame(minstd_weight)

## ----weekly return of minimum variance portfolio------------------------------
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

## ----weekly return of minimum mad portfolio-----------------------------------
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

## ----weekly return of minimum mead portfolio----------------------------------
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
table.AnnualizedReturns(R = min_risk, scale = 52, Rf = 0.025/52, geometric = TRUE)

table.DownsideRisk(R = min_risk, scale = 52, Rf = 0.025/52, MAR = 0.07/52)

table.Stats(R = min_risk)

## ----find rand ports with mean return above ewp mean return-------------------
mean_port <- foreach(i = opt_periods) %do% {
  tmp <- PerformanceAnalytics::Mean.arithmetic(x = rp_returns[i, ])
  
  ewp_mean <- mean(x = ewp_return[i, ])
  
  mean_target <- rand_port[which(tmp > ewp_mean),]
}

## ----find rand ports with median return above ewp median return---------------
median_port <- foreach(i = opt_periods) %do% {
  tmp <- apply(X = rp_returns[i, ], MARGIN = 2, FUN = median)
  
  ewp_median <- median(x = ewp_return[i, ])
  
  median_target <- rand_port[which(tmp > ewp_median),]
}

## ----weekly return of subset portfolios---------------------------------------
# Find returns from rp_returns that satisfy the target mean return constraint
meanport_return <- foreach(i = opt_periods) %do% {
  tmp <- PerformanceAnalytics::Mean.arithmetic(x = rp_returns[i, ])
  
  ewp_mean <- PerformanceAnalytics::Mean.arithmetic(x = ewp_return[i, ])
  
  mean_target <- rp_returns[i, which(tmp > as.vector(ewp_mean))]
}

# Find returns from rp_returns that satisfy the target median return constraint
medianport_return <- foreach(i = opt_periods) %do% {
  tmp <- apply(X = rp_returns[i, ], MARGIN = 2, FUN = median)
  
  ewp_median <- median(x = ewp_return[i, ])
  
  median_target <- rp_returns[i, which(tmp > ewp_median)]
}

## ----find weights for mean variance portfolio---------------------------------
meanvar_weight <- foreach(i = 1:length(meanport_return), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::StdDev(R = meanport_return[[i]])
  
  opt_weight <- mean_port[[i]][which.min(tmp), ]
}

rownames(meanvar_weight) <- paste("OP", 1:nrow(meanvar_weight), sep = "")

data.frame(meanvar_weight)

## ----weekly return of mean variance portfolio---------------------------------
meanvar_returns <- foreach(i = ret_periods, j = 1:nrow(meanvar_weight), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data[i, ], 
                                                weights = meanvar_weight[j, ], 
                                                geometric = TRUE, 
                                                rebalance_on = "quarters")
}

## ----find weights for mean mad portfolio--------------------------------------
meanmad_weight <- foreach(i = 1:length(meanport_return), .combine = "rbind") %do% {
  tmp <- apply(X = meanport_return[[i]], MARGIN = 2, FUN = function(x) {
    mean(abs(x - mean(x)))
    })
  
  opt_weight <- mean_port[[i]][which.min(tmp), ]
}

rownames(meanmad_weight) <- paste("OP", 1:nrow(meanmad_weight), sep = "")

data.frame(meanmad_weight)

## ----weekly return of mean mad portfolio--------------------------------------
meanmad_returns <- foreach(i = ret_periods, j = 1:nrow(meanmad_weight), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data[i, ], 
                                                weights = meanmad_weight[j, ], 
                                                geometric = TRUE, 
                                                rebalance_on = "quarters")
}

## ----find weights for mean mead portfolio-------------------------------------
meanmead_weight <- foreach(i = 1:length(meanport_return), .combine = "rbind") %do% {
  tmp <- apply(X = meanport_return[[i]], MARGIN = 2, FUN = function(x) {
    median(abs(x - median(x)))
    })
  
  opt_weight <- mean_port[[i]][which.min(tmp), ]
}

rownames(meanmead_weight) <- paste("OP", 1:nrow(meanmead_weight), sep = "")

data.frame(meanmead_weight)

## ----weekly return of mean mead portfolio-------------------------------------
meanmead_returns <- foreach(i = ret_periods, j = 1:nrow(meanmead_weight), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data[i, ], 
                                                weights = meanmead_weight[j, ], 
                                                geometric = TRUE, 
                                                rebalance_on = "quarters")
}

## ----find weights for median variance portfolio-------------------------------
medvar_weight <- foreach(i = 1:length(medianport_return), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::StdDev(R = medianport_return[[i]])
  
  opt_weight <- median_port[[i]][which.min(tmp), ]
}

rownames(medvar_weight) <- paste("OP", 1:nrow(medvar_weight), sep = "")

data.frame(medvar_weight)

## ----weekly return of median variance portfolio-------------------------------
medvar_returns <- foreach(i = ret_periods, j = 1:nrow(medvar_weight), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data[i, ], 
                                                weights = medvar_weight[j, ], 
                                                geometric = TRUE, 
                                                rebalance_on = "quarters")
}

## ----find weights for median mad portfolio------------------------------------
medmad_weight <- foreach(i = 1:length(medianport_return), .combine = "rbind") %do% {
  tmp <- apply(X = medianport_return[[i]], MARGIN = 2, FUN = function(x) {
    mean(abs(x - mean(x)))
    })
  
  opt_weight <- median_port[[i]][which.min(tmp), ]
}

rownames(medmad_weight) <- paste("OP", 1:nrow(medmad_weight), sep = "")

data.frame(medmad_weight)

## ----weekly return of median mad portfolio------------------------------------
medmad_returns <- foreach(i = ret_periods, j = 1:nrow(medmad_weight), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data[i, ], 
                                                weights = medmad_weight[j, ], 
                                                geometric = TRUE, 
                                                rebalance_on = "quarters")
}

## ----find weights for median mead portfolio-----------------------------------
medmead_weight <- foreach(i = 1:length(medianport_return), .combine = "rbind") %do% {
  tmp <- apply(X = medianport_return[[i]], MARGIN = 2, FUN = function(x) {
    median(abs(x - median(x)))
    })
  
  opt_weight <- median_port[[i]][which.min(tmp), ]
}

rownames(medmead_weight) <- paste("OP", 1:nrow(medmead_weight), sep = "")

data.frame(medmead_weight)

## ----weekly return of median mead portfolio-----------------------------------
medmead_returns <- foreach(i = ret_periods, j = 1:nrow(medmead_weight), .combine = "rbind") %do% {
  tmp <- PerformanceAnalytics::Return.portfolio(R = return_data[i, ], 
                                                weights = medmead_weight[j, ], 
                                                geometric = TRUE, 
                                                rebalance_on = "quarters")
}

## ----plot weights of optimal portfolios---------------------------------------
optport_weights <- list(Mean_Variance = meanvar_weight, Mean_MAD = meanmad_weight, Mean_MeAD = meanmead_weight,
                        Median_Variance = medvar_weight, Median_MAD = medmad_weight, Median_MeAD = medmead_weight)

for(i in 1:6) {
  chart.StackedBar(w = optport_weights[[i]], colorset = RColorBrewer::brewer.pal(n = 10, "Spectral"),
                 main = paste("Weights of", gsub(pattern = "_", replacement = " ", names(optport_weights)[i]), "Portfolio", sep = " "), 
                 ylab = "Weight")
}

## ----plot returns of optimal portfolios---------------------------------------
opt_port <- cbind(meanvar_returns, meanmad_returns, meanmead_returns, medvar_returns, medmad_returns, medmead_returns, ewp_return["2016/"]) %>%
  `colnames<-`(c(names(optport_weights), "Equal Weight"))

chart.CumReturns(R = opt_port, geometric = TRUE, 
                 main = "Cumulative Return of Optimal Portfolios", 
                 plot.engine = "plotly")

chart.Drawdown(R = opt_port, geometric = TRUE, 
               main = "Drawdown of Optimal Portfolios", 
               plot.engine = "plotly")

## ----perf summary of optimal portfolios---------------------------------------
table.AnnualizedReturns(R = opt_port, scale = 52, Rf = 0.025/52, geometric = TRUE)

table.DownsideRisk(R = opt_port, scale = 52, Rf = 0.025/52, MAR = 0.07/52)

table.Stats(R = opt_port)

