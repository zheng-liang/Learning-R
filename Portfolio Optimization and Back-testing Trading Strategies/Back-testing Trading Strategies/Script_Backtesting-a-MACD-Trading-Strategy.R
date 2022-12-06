## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.align="center")
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages, message=FALSE, warning=FALSE, class.source = "fold-show"----
library(tidyquant) # Loads quantmod, PerformanceAnalytics and TTR packages
library(tidyverse) # Loads dplyr and ggplot2 packages (data manipulation and plotting)

## ----obtain historical prices-------------------------------------------------
# Set start and end date for data retrieval
startdate <- as.Date("2018-01-01")
enddate <- as.Date("2022-11-01")

# Create vector of tickers
ticker <- c("AAPL", "XOM", "MSFT", "PG", "MRK")

dat <- list()

for (t in seq(ticker)) {
  dat[[ticker[t]]] <- quantmod::getSymbols(Symbols = ticker[t], src = "yahoo", auto.assign = FALSE,
                                           from = startdate, to = enddate, periodicity = "daily")
}

lapply(dat, head, 3)

## ----calculate MACD and signal values and slope-------------------------------
# Create MACD and signal values, time component and regress to find slops
for (i in seq(dat)) {
  dat[[i]] <- cbind(dat[[i]], TTR::MACD(x = quantmod::Cl(dat[[i]]),
                                        nFast = 20,
                                        nSlow = 50,
                                        nSig = 5,
                                        maType = "EMA"))
  
  dat[[i]]$MACDslope <- diff(dat[[i]]$macd, lag = 5)
  
  dat[[i]]$SIGslope <- diff(dat[[i]]$signal, lag = 5)
}

## ----generate MACD trading signals--------------------------------------------
# Remove rows with missing data
dat2 <- lapply(dat, na.omit)

# Use for loops to determine if conditions were met
for (i in seq(dat2)) {
  dat2[[i]]$trade.signal <- (dat2[[i]]$macd > dat2[[i]]$signal) & (dat2[[i]]$MACDslope > 0) | (dat2[[i]]$SIGslope > 0)
}

## ----returns from MACD trading strategy---------------------------------------
# Calculate returns using opening price and lag the trade.signal twice for backtesting
# Reason: buy the next day after the trade.signal and returns are calculated using previous day value
for (i in seq(dat2)) {
  dat2[[i]]$MACDreturn <- Return.calculate(prices = Op(dat2[[i]]), method = "log") * Lag(dat2[[i]]$trade.signal, k = 2)
}

## ----plot returns from MACD strategy------------------------------------------
# Calculate the returns earned by holding the stocks using adjusted prices
# Adjusted prices include splits and dividends information
for (i in seq(dat2)) {
  dat2[[i]]$total.return <- Return.calculate(prices = Ad(dat2[[i]]), method = "log")
}

# Save the cumulative return data in a separate dataframe
return.plot <- NULL

for (i in seq(dat2)) {
  return.plot <- cbind(return.plot,
                       10000 * exp(cumsum(na.omit(dat2[[i]]$MACDreturn))),
                       10000 * exp(cumsum(na.omit(dat2[[i]]$total.return))))
}

colnames(return.plot) <- paste(rep(ticker, each = 2), 
                               rep(c("MACD Trading Strategy", "Total Return"), length(dat2)))

for (i in seq(from = 1, to = ncol(return.plot), by = 2)) {
  plot(return.plot[, c(i, i+1)],
       main = paste("Comparison of Cumulative Returns for", rep(ticker, each = 2)[i]),
       legend.loc = "topleft", grid.col = NA) %>% 
    print()
}

## ----annualized return and sd of MACD trading strategy------------------------
for (i in seq(dat2)) {
  PerformanceAnalytics::table.AnnualizedReturns(R = dat2[[i]][, c("MACDreturn","total.return")],
                                                scale = 252,
                                                Rf = 0.03/252,
                                                geometric = FALSE) %>%
    `colnames<-`(paste(rep(ticker[i], 2),
                       rep(c("MACD Trading Strategy", "Total Return")))) %>%
    print()
}

