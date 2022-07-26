## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.align="center")
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----Load packages, message=FALSE---------------------------------------------
#Use install.packages() or go to the Packages panel to install packages if they have not been installed
library(dplyr)
library(PerformanceAnalytics) # For portfolio performance and risk analysis
library(PortfolioAnalytics) # For portfolio optimization and analysis
library(quantmod) # For obtaining historical prices from Yahoo Finance
library(ROI) # For ROI solver in portfolio optimization
library(ROI.plugin.glpk) # Part of the ROI solver for linear optimization
library(ROI.plugin.quadprog) #Part of the ROI solver for quadratic optimization

## ----Retrieve price data------------------------------------------------------
AMZN <- getSymbols(Symbols = "AMZN",
                   src = "yahoo",
                   # Can instead choose "weekly" or "monthly" to import weekly or monthly price data
                   periodicity = "daily",
                   auto.assign = F)

## ----Understanding object-----------------------------------------------------
head(AMZN, n = 4); tail(AMZN, n = 4)

colSums(is.na(AMZN))

## ----Discrete return and cumulative return plot-------------------------------
AMZN.Adj <- AMZN[,6]

# na.omit() removes the first row, which is an NA value as there are no prior data to calculate returns
AMZN_Returns <- na.omit(Return.calculate(AMZN.Adj, method = "discrete"))
# We could use Return.calculate(AMZN.Adj, method = "log") to calculate the log return instead.

chart.CumReturns(AMZN_Returns, 
                 legend.loc = "topleft",
                 # Could specify geometric = F to use simple/arithmetic return when using log returns in the previous step
                 geometric = T,
                 main = "Cumulative Daily Return of Amazon")

## ----Vector of tickers--------------------------------------------------------
tickers <- c("JNJ", "PG", "AAPL", "TSM", "MSFT", "NVDA")

## ----Import and subset price data---------------------------------------------
price_data <- NULL

startdate <- "2012-01-01"
enddate <- "2022-05-23"

for (ticker in tickers) {
  price_data <- cbind(price_data,
                      getSymbols(ticker, 
                                 src = "yahoo",
                                 from = startdate, to = enddate, 
                                 periodicity = "daily", auto.assign = F)[,6])
}

colnames(price_data) <- tickers

## ----Brief details of price_data----------------------------------------------
head(price_data, n = 4); tail(price_data, n = 4)

nrow(price_data); colSums(is.na(price_data))

## ----Daily (discrete) return--------------------------------------------------
returns <- na.omit(Return.calculate(price_data, method = "discrete"))

## ----Portfolio returns--------------------------------------------------------
r_noRebal <- Return.portfolio(returns,
                              geometric = T,
                              verbose = T)

colnames(r_noRebal$returns) <- "Rp_noRebal"

lapply(r_noRebal, head, n = 4)

## ----Quarterly rebalancing----------------------------------------------------
r_withRebal <- Return.portfolio(returns, 
                                geometric = T, 
                                rebalance_on = "quarters", 
                                verbose = T)

colnames(r_withRebal$returns) <- "Rp_withRebal"

## ----End of period weights----------------------------------------------------
eop_weight_noRebal <- r_noRebal$EOP.Weight

eop_weight_wRebal <- r_withRebal$EOP.Weight

  par(mfrow = c(2,1), mar = c(2, 4, 2, 2))

  plot.zoo(eop_weight_wRebal$JNJ, 
           main = "JNJ End-of-Period Weights Over Time",
           ylab = "With Rebalancing")
           
  abline(h = 1/length(colnames(eop_weight_wRebal)), col = "red", lwd = 2)

  plot.zoo(eop_weight_noRebal$JNJ,
          ylab = "Without Rebalancing")
  
  abline(h = 1/length(colnames(eop_weight_wRebal)), col = "red", lwd = 2)

## ----Performance chart--------------------------------------------------------
ret_comp <- cbind(r_noRebal$returns, r_withRebal$returns)

charts.PerformanceSummary(R = ret_comp,
                          main = "Comparison of Cumulative Returns",
                          legend.loc = "topleft")

## ----Weights over time without rebalancing------------------------------------
par(mfrow = c(1,1), mar = c(2, 2, 2, 2))

plot.zoo(eop_weight_noRebal,
         main = "End-of-Period Weights Over Time Without Rebalancing")

## ----Stats of portfolios------------------------------------------------------
table.Stats(ret_comp)

## ----Import benchmark price and calculate returns-----------------------------
benchmark <- getSymbols("SPY", 
                        src = "yahoo",
                        from = startdate, to = enddate, 
                        periodicity = "daily", auto.assign = F)[,6]

nrow(benchmark)
colSums(is.na(benchmark))

benchmarkReturn <- na.omit(Return.calculate(benchmark, method = "discrete")) %>%
  `colnames<-`("SPY")

## ----metrics------------------------------------------------------------------
perfMetrics <- round(rbind(CAPM.beta(ret_comp, benchmarkReturn, Rf = 0.03/252),
                           CAPM.alpha(ret_comp, benchmarkReturn, Rf = 0.03/252),
                           SharpeRatio(ret_comp, Rf = 0.03/252, FUN = "StdDev"),
                           TreynorRatio(ret_comp, benchmarkReturn, Rf = 0.03/252),
                           InformationRatio(ret_comp, benchmarkReturn)), digits = 4
)

perfMetrics

## ----Table of annualized returns----------------------------------------------
cbind(table.AnnualizedReturns(ret_comp, Rf = 0.03/252),
      table.AnnualizedReturns(benchmarkReturn, Rf = 0.03/252))

## ----Table of calendar returns------------------------------------------------
table.CalendarReturns(benchmarkReturn)

## ----Adding constrains and objectives 1---------------------------------------
portspec1 <- portfolio.spec(colnames(returns))

# Sum of weights constrained to 1, can also specify as type = "full investment"
portspec1 <- add.constraint(portspec1, 
                            type = "weight_sum", 
                            min_sum = 1, max_sum = 1)

# Weight constraint on each stock, max is 35% of portfolio
portspec1 <- add.constraint(portspec1, 
                            type="box",
                            min=0, max=0.35)

# Objective to minimize risk based on variance (function will default to standard deviation as measure of risk)
portspec1 <- add.objective(portspec1,
                           type = "risk",
                           name = "var")

## ----Optimizing weights to minimize variance----------------------------------
port_MV <- optimize.portfolio(returns, 
                              portspec1,
                              optimize_method = "quadprog",
                              # Indicating "trace = T" allows us to use 
                              # additional information in the later parts
                              trace = T)

port_MV

## ----Adding constrains and objectives 2---------------------------------------
portspec2 <- portfolio.spec(colnames(returns))

# We use the same constraints as in Case 1
portspec2 <- add.constraint(portspec2, 
                            type = "weight_sum", 
                            min_sum = 1, max_sum = 1)

portspec2 <- add.constraint(portspec2, 
                            type="box", 
                            min=0, max=0.35)

# Objective to maximize return based on mean return
portspec2 <- add.objective(portspec2,
                           type = "return",
                           name = "mean")

## ----Optimizing weights to maximize return------------------------------------
port_MR <- optimize.portfolio(returns, 
                              portspec2,
                              optimize_method = "glpk",
                              trace = T)

port_MR

## ----Extract weights----------------------------------------------------------
weight_MV <- extractWeights(port_MV)

weight_MR <- extractWeights(port_MR)

par(mfrow = c(2,1), mar = c(2, 4, 2, 2))

barplot(weight_MV, 
        main = "Weights in Different Portfolios",
        ylab = "Portfolio MV")

barplot(weight_MR,
        ylab = "Portfolio MR")

## ----Calculate portfolio returns----------------------------------------------
rp_MV <- Return.portfolio(returns, 
                          weights = weight_MV,
                          rebalance_on = "quarters",
                          geometric = T) %>%
  `colnames<-`("Portfolio MV")

rp_MR <- Return.portfolio(returns, 
                          weights = weight_MR,
                          rebalance_on = "quarters",
                          geometric = T) %>%
  `colnames<-`("Portfolio MR")

## ----Chart of performance-----------------------------------------------------
comparison <- cbind(rp_MV, rp_MR, benchmarkReturn)

charts.PerformanceSummary(comparison,
                          main = "Comparing Performance of Portfolios",
                          legend.loc = "topleft")

## ----Stats and metrics of portfolios------------------------------------------
table.Stats(comparison)

table.AnnualizedReturns(comparison, 0.03/252, scale = 252)

## ----Adding constrains and objectives 3---------------------------------------
portspec3 <- portfolio.spec(colnames(returns))

# Slight change in weight_sum to reduce restrictiveness while optimizing
portspec3 <- add.constraint(portspec3, 
                            type = "weight_sum",
                            min_sum= 0.99, max_sum = 1.01)

portspec3 <- add.constraint(portspec3, 
                            type="box", 
                            min=0, max=0.4)

# Add a constraint to have a target return
portspec3 <- add.constraint(portspec3,
                            type = "return",
                            return_target = 0.15/252)

# Adding a return objective, thus we maximize mean return per unit of risk
portspec3 <- add.objective(portspec3,
                           type = "return",
                           name = "mean")

portspec3 <- add.objective(portspec3,
                           type = "risk",
                           name = "var")

## ----Optimizing portfolio, message=FALSE, warning=FALSE-----------------------
set.seed(123)

randport <- random_portfolios(portfolio = portspec3, 
                              permutations = 20000, 
                              rp_method = "sample", 
                              eliminate = T)

port_MeanVar_qRebal <- optimize.portfolio.rebalancing(returns,
                                                      portspec3,
                                                      optimize_method = "random",
                                                      rebalance_on = "quarters",
                                                      rp = randport,
                                                      search_size = 5000,
                                                      training_period = 504,
                                                      rolling_window = 45)

## ----Weights------------------------------------------------------------------
chart.Weights(port_MeanVar_qRebal)

## ----MeanVar Portfolio Returns, warning=FALSE---------------------------------
w_qOptim <- extractWeights(port_MeanVar_qRebal)

rp_MeanVar <- Return.portfolio(returns, 
                               weights = w_qOptim, 
                               geometric = T)

## ----Comparison of performance------------------------------------------------
evaluation <- cbind(rp_MeanVar,benchmarkReturn)

charts.PerformanceSummary(evaluation, Rf=0.03/252, main = "Performance of Mean-Variance Portfolio")

table.AnnualizedReturns(evaluation, Rf=0.03/252)

