## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.align="center")
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages, message=FALSE, warning=FALSE------------------------------
library(PortfolioAnalytics) # For portfolio optimization and analysis
library(RColorBrewer) # For color palettes in plots
library(ROI) # For ROI solver in portfolio optimization
library(ROI.plugin.glpk) # Part of the ROI solver for linear optimization
library(ROI.plugin.quadprog) #Part of the ROI solver for quadratic optimization
library(tidyquant) # For quantmod and PerformanceAnalytics functions
library(tidyverse) # For dplyr and ggplot2 functions (data manipulation and plotting)

## ----get index components using tidyquant, message=FALSE----------------------
# Only require ticker and maybe name of company, which is columns 1 and 2 of query result

sp500 <- tidyquant::tq_index(x = "SP500")[,1:2]

sp500

tickers <- sp500$symbol

## ----retrieve price data, warning=FALSE---------------------------------------
startdate <- as.Date("2015-01-01")
enddate <- as.Date("2022-07-01")

prices <- NULL

for (t in tickers) {   # Use for loop to get prices of all tickers
   
   tmp <- NULL
  
   tmp <- try(   # Use try function to skip loop and return error message when price cannot be retrieved
       expr = {quantmod::getSymbols(Symbols = t, src = "yahoo", auto.assign = FALSE, 
                                    from = startdate, to = enddate, periodicity = "daily")[,6]},
       silent = TRUE
   )
   
   if(inherits(tmp, "try-error")) {cat("ERROR:", t, sep = " "); next}
   
   names(tmp) <- t
   
   prices <- cbind(prices, tmp)
}

# Check dimension of object, start and end date of data collected
dim(prices); start(prices); end(prices)

## ----add BRK-B and BF-B-------------------------------------------------------
prices <- cbind(quantmod::getSymbols(Symbols = "BRK-B", src = "yahoo", auto.assign = F, 
                                     from = startdate, to = enddate, periodicity = "daily")[,6],
                quantmod::getSymbols(Symbols = "BF-B", src = "yahoo", auto.assign = F, 
                                     from = startdate, to = enddate, periodicity = "daily")[,6],
                prices)

colnames(prices)[1:2] <- c("BRK-B", "BF-B")

# Check which stocks have NA over the specified period
data.frame(rbind(which(colSums(is.na(prices)) > 0)), row.names = "Number of NAs")

# Keep only stocks that do not have NAs over the specified period
prices <- prices[, which(colSums(is.na(prices)) == 0)]

dim(prices) # Left with 482 stocks

## ----create 10 portfolios with 10 tickers each--------------------------------
samp_stocks <- list()

for (i in 1:10) {
  set.seed((50+i)^2)
  
  samp_stocks[[i]] <- prices[, sample(x = ncol(prices), size = 15, replace = FALSE)]
  
  names(samp_stocks)[i] <- paste("S", i, "_prices", sep = "")
}

# Check stocks sampled into each portfolio
data.frame(sapply(samp_stocks, colnames), row.names = 1:15)

## ----calculate returns for portfolios-----------------------------------------
stock_returns <- lapply(samp_stocks, function(x) {
  na.omit(PerformanceAnalytics::Return.calculate(x, method = "discrete"))
})

names(stock_returns) <- paste("S", c(seq(1:10)), "_returns", sep = "")

data.frame(sapply(stock_returns, dim), row.names = c("Number of rows", "Number of columns"))

# Show first 3 elements in stock_returns, first 5 columns and first 4 rows of data
lapply(stock_returns[1:2], function(x) {
  head(x[,1:5], n = 4)
})

## ----create mean-variance portfolio specification-----------------------------
portspecs <- list()

for (s in 1:10) {
  # Initialize portfolio specification
  portspecs[[s]] <- PortfolioAnalytics::portfolio.spec(assets = names(stock_returns[[s]]))
  
  # Sum of weights constrained to 1, can also specify as type = "full investment"
  portspecs[[s]] <- add.constraint(portspecs[[s]], 
                                       type = "weight_sum",
                                       min_sum= 1, max_sum = 1)

  # Weight constraint on each stock, max is 15% of portfolio
  portspecs[[s]] <- add.constraint(portspecs[[s]], 
                                       type="box", 
                                       min=0, max=0.15)
  
  # Objective to minimize risk based on variance (function will default to standard deviation as measure of risk)
  portspecs[[s]] <- add.objective(portspecs[[s]],
                                      type = "risk",
                                      name = "var")
  
  # Adding a return objective, thus we maximize mean return per unit of risk
  portspecs[[s]] <- add.objective(portspecs[[s]],
                                      type = "return",
                                      name = "mean")
  
  # Add name to specification to distinguish them
  names(portspecs)[s] <- paste("P", s, "_spec", sep = "")
}

# Example of what portspecs contains
portspecs$P1_spec

## ----optimize portfolios, warning=FALSE---------------------------------------
optports <- list()

for (p in 1:10) {
  optports[[p]] <- PortfolioAnalytics::optimize.portfolio.rebalancing(R = stock_returns[[p]], 
                                                                      portfolio = portspecs[[p]], 
                                                                      optimize_method = "ROI", 
                                                                      rebalance_on = "quarters", 
                                                                      training_period = 252, 
                                                                      rolling_window = 125,
                                                                      maxSR = TRUE)
  
  names(optports)[p] <- paste("P", p, "_optimal", sep = "")
}

# Example of what optports contains
optports$P1_optimal

## ----plot weights of first 4 portfolios---------------------------------------
chart.Weights(object = optports[[1]], 
              colorset = c(RColorBrewer::brewer.pal(n = 7, name = "Dark2"), RColorBrewer::brewer.pal(n = 8, name = "Accent")), 
              main = "Portfolio 1 Component Weights")

## ----extract weights from portfolios------------------------------------------
port_weights <- lapply(optports, FUN = PortfolioAnalytics::extractWeights)

names(port_weights) <- paste("P", c(seq(1:10)), "_weights", sep = "")

# Show first 3 elements in stock_returns, first 5 columns and first 4 rows of data each
lapply(port_weights[1:3], function(x) {
  head(x[,1:5], n = 4)
})

## ----calculate daily portfolio returns----------------------------------------
port_returns <- NULL

for (r in 1:10) {
  port_returns <- cbind(port_returns,
                        PerformanceAnalytics::Return.portfolio(R = stock_returns[[r]], 
                                                               weights = port_weights[[r]], 
                                                               geometric = TRUE))
  
  names(port_returns)[r] <- paste("P", r, "_returns", sep = "")
}

data.frame(head(port_returns))

## ----obtain adjusted closing prices of benchmarks-----------------------------
index_tickers <- c("SPY", "VTI", "QQQ", "IWM", "IJR", "IJH")

index_prices <- NULL

for (i in index_tickers) {
  index_prices <- cbind(index_prices,
                        quantmod::getSymbols(Symbols = i, src = "yahoo", auto.assign = FALSE, 
                                             from = startdate, to = enddate, periodicity = "daily")[,6])
}

colnames(index_prices) <- index_tickers

active_tickers <- c("ARKK", "ARKG", "EMLP", "ARKW", "ARKQ")

active_prices <- NULL

for (i in active_tickers) {
  active_prices <- cbind(active_prices,
                         quantmod::getSymbols(Symbols = i, src = "yahoo", auto.assign = FALSE, 
                                             from = startdate, to = enddate, periodicity = "daily")[,6])
}

colnames(active_prices) <- active_tickers

## ----calculate benchmark daily returns----------------------------------------
index_returns <- na.omit(PerformanceAnalytics::Return.calculate(prices = index_prices, method = "discrete"))

data.frame(head(index_returns, 4))

active_returns <- na.omit(PerformanceAnalytics::Return.calculate(prices = active_prices, method = "discrete"))

data.frame(head(active_returns, 4))

## ----plot cum returns against index ETFs, message=FALSE, warning=FALSE--------
PerformanceAnalytics::chart.CumReturns(R = cbind(port_returns[,1:5], index_returns), 
                                       geometric = TRUE,
                                       main = "Cumulative Returns of Portfolios and Index ETFs", 
                                       colorset = RColorBrewer::brewer.pal(n = 11, name = "Spectral"), 
                                       plot.engine = "plotly")

PerformanceAnalytics::chart.CumReturns(R = cbind(port_returns[,6:10], index_returns),
                                       geometric = TRUE,
                                       main = "Cumulative Returns of Portfolios and Index ETFs",
                                       colorset = RColorBrewer::brewer.pal(n = 11, name = "Spectral"),
                                       plot.engine = "plotly")

## ----plot drawdowns against index ETFs, message=FALSE, warning=FALSE----------
PerformanceAnalytics::chart.Drawdown(R = cbind(port_returns[,1:5], index_returns), 
                                     geometric = TRUE,
                                     main = "Drawdowns of Portfolios and Index ETFs", 
                                     colorset = RColorBrewer::brewer.pal(n = 11, name = "Spectral"), 
                                     plot.engine = "plotly")

PerformanceAnalytics::chart.Drawdown(R = cbind(port_returns[,6:10], index_returns),
                                     geometric = TRUE,
                                     main = "Drawdowns of Portfolios and Index ETFs",
                                     colorset = RColorBrewer::brewer.pal(n = 11, name = "Spectral"),
                                     plot.engine = "plotly")

## ----evaluation metrics against index ETFs------------------------------------
table.AnnualizedReturns(R = cbind(port_returns, index_returns), scale = 252, Rf = 0.025/252, geometric = TRUE, digits = 4)

table.CAPM(Ra = port_returns, Rb = index_returns$SPY, scale = 252, Rf = 0.025/252, digits = 4)

table.DownsideRisk(R = cbind(port_returns, index_returns), scale = 252, Rf = 0.025/252, MAR = 0.07/252, digits = 4)

## ----plot cum returns against active ETFs, message=FALSE, warning=FALSE-------
PerformanceAnalytics::chart.CumReturns(R = cbind(port_returns[,1:5], active_returns), 
                                       geometric = TRUE,
                                       main = "Cumulative Returns of Portfolios and Active ETFs", 
                                       colorset = RColorBrewer::brewer.pal(n = 10, name = "Spectral"), 
                                       plot.engine = "plotly")

PerformanceAnalytics::chart.CumReturns(R = cbind(port_returns[,6:10], active_returns),
                                       geometric = TRUE,
                                       main = "Cumulative Returns of Portfolios and Active ETFs",
                                       colorset = RColorBrewer::brewer.pal(n = 10, name = "Spectral"),
                                       plot.engine = "plotly")

## ----plot drawdowns against active ETFs, message=FALSE, warning=FALSE---------
PerformanceAnalytics::chart.Drawdown(R = cbind(port_returns[,1:5], active_returns), 
                                     geometric = TRUE,
                                     main = "Drawdowns of Portfolios and Active ETFs", 
                                     colorset = RColorBrewer::brewer.pal(n = 11, name = "Spectral"), 
                                     plot.engine = "plotly")

PerformanceAnalytics::chart.Drawdown(R = cbind(port_returns[,6:10], active_returns),
                                     geometric = TRUE,
                                     main = "Drawdowns of Portfolios and Active ETFs",
                                     colorset = RColorBrewer::brewer.pal(n = 11, name = "Spectral"),
                                     plot.engine = "plotly")

## ----evaluation metrics against active ETFs-----------------------------------
table.AnnualizedReturns(R = cbind(port_returns, active_returns), scale = 252, Rf = 0.025/252, geometric = TRUE, digits = 4)

table.CAPM(Ra = cbind(port_returns, active_returns), Rb = index_returns$SPY, scale = 252, Rf = 0.025/252, digits = 4)

table.DownsideRisk(R = cbind(port_returns, active_returns), scale = 252, Rf = 0.025/252, MAR = 0.07/252, digits = 4)

