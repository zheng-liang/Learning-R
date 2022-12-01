## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.align="center")
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages, message=FALSE, warning=FALSE, class.source = "fold-show"----
library(tidyquant) # Loads quantmod, PerformanceAnalytics and TTR packages
library(tidyverse) # Loads dplyr and ggplot2 packages (data manipulation and plotting)

## ----obtain closing prices----------------------------------------------------
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

names(dat); lapply(dat, head, 3)

## ----calculate SMA, class.source = "fold-show"--------------------------------
for (i in seq(dat)) {
  dat[[i]]$SMA5 <- TTR::SMA(x = quantmod::Cl(dat[[i]]), n = 5)
  
  dat[[i]]$SMA20 <- TTR::SMA(x = quantmod::Cl(dat[[i]]), n = 20)
}

## ----generate SMA trading signals, class.source = "fold-show"-----------------
# Generate TRUE/FALSE boolean, which is converted to 1 or 0
for (i in seq(dat)) {
  dat[[i]]$SMAsignal <- dat[[i]]$SMA5 > dat[[i]]$SMA20 | diff(dat[[i]]$SMA20, lag = 5)/5 > 0
}

## ----returns from SMA trading strategy, class.source = "fold-show"------------
# Calculate returns using opening price and lag the SMAsignal twice for backtesting
# Reason: buy the next day after the SMAsignal and returns are calculated using previous day value
for (i in seq(dat)) {
  dat[[i]]$SMAreturn <- Return.calculate(prices = Op(dat[[i]]), method = "log") * Lag(dat[[i]]$SMAsignal, k = 2)
}

## ----plot returns from SMA strategy-------------------------------------------
# Calculate the returns earned by holding the stocks using adjusted prices
# Adjusted prices include splits and dividends information
for (i in seq(dat)) {
  dat[[i]]$total.return <- Return.calculate(prices = Ad(dat[[i]]), method = "log")
}

# Save the cumulative return data in a separate dataframe
SMAreturn_plot <- NULL

for (i in seq(dat)) {
  SMAreturn_plot <- cbind(SMAreturn_plot,
                          10000 * exp(cumsum(na.omit(dat[[i]]$SMAreturn))),
                          10000 * exp(cumsum(na.omit(dat[[i]]$total.return))))
}

colnames(SMAreturn_plot) <- paste(rep(ticker, each = 2), 
                                  rep(c("SMA Trading Strategy", "Total Return"), length(dat)))

for (i in seq(from = 1, to = ncol(SMAreturn_plot), by = 2)) {
  plot(SMAreturn_plot[, c(i, i+1)],
       main = paste("Comparison of Cumulative Returns for", rep(ticker, each = 2)[i]),
       legend.loc = "topleft", grid.col = NA) %>% 
    print()
}

## ----annualized return and sd of SMA trading strategy-------------------------
for (i in seq(dat)) {
  PerformanceAnalytics::table.AnnualizedReturns(R = dat[[i]][, c("SMAreturn","total.return")],
                                                scale = 252,
                                                Rf = 0.03/252,
                                                geometric = FALSE) %>%
    `colnames<-`(paste(rep(ticker[i], 2),
                       rep(c("SMA Trading Strategy", "Total Return")))) %>%
    print()
}

## ----calculate EMA, class.source = "fold-show"--------------------------------
for (i in seq(dat)) {
  dat[[i]]$EMA5 <- TTR::EMA(x = quantmod::Cl(dat[[i]]), n = 5)
  
  dat[[i]]$EMA20 <- TTR::EMA(x = quantmod::Cl(dat[[i]]), n = 20)
}

## ----generate EMA trading signals, class.source = "fold-show"-----------------
# Generate TRUE/FALSE boolean, which is converted to 1 or 0
for (i in seq(dat)) {
  dat[[i]]$EMAsignal <- dat[[i]]$EMA5 > dat[[i]]$EMA20 | diff(dat[[i]]$EMA20, lag = 5)/5 > 0
}

## ----returns from EMA trading strategy, class.source = "fold-show"------------
# Calculate returns using opening price and lag the EMAsignal twice for backtesting
# Reason: buy the next day after the EMAsignal and returns are calculated using previous day value
for (i in seq(dat)) {
  dat[[i]]$EMAreturn <- Return.calculate(prices = Op(dat[[i]]), method = "log") * Lag(dat[[i]]$EMAsignal, k = 2)
}

## ----plot returns from EMA strategy-------------------------------------------
# Save the cumulative return data in a separate dataframe
EMAreturn_plot <- NULL

for (i in seq(dat)) {
  EMAreturn_plot <- cbind(EMAreturn_plot,
                          10000 * exp(cumsum(na.omit(dat[[i]]$EMAreturn))),
                          10000 * exp(cumsum(na.omit(dat[[i]]$total.return))))
}

colnames(EMAreturn_plot) <- paste(rep(ticker, each = 2), 
                                  rep(c("EMA Trading Strategy", "Total Return"), length(dat)))

for (i in seq(from = 1, to = ncol(EMAreturn_plot), by = 2)) {
  plot(EMAreturn_plot[, c(i, i+1)],
       main = paste("Comparison of Cumulative Returns for", rep(ticker, each = 2)[i]),
       legend.loc = "topleft", grid.col = NA) %>% 
    print()
}

## ----annualized return and sd of EMA trading strategy-------------------------
for (i in seq(dat)) {
  PerformanceAnalytics::table.AnnualizedReturns(R = dat[[i]][, c("EMAreturn","total.return")],
                                                scale = 252,
                                                Rf = 0.03/252,
                                                geometric = FALSE) %>%
    `colnames<-`(paste(rep(ticker[i], 2),
                       rep(c("EMA Trading Strategy", "Total Return")))) %>%
    print()
}

## ----generate EMA+SMA trading signals, class.source = "fold-show"-------------
# Generate TRUE/FALSE boolean, which is converted to 1 or 0
for (i in seq(dat)) {
  dat[[i]]$EMA_SMA.signal <- dat[[i]]$EMA5 > dat[[i]]$SMA20 | diff(dat[[i]]$SMA20, lag = 5)/5 > 0
}

## ----returns from EMA+SMA trading strategy, class.source = "fold-show"--------
# Calculate returns using opening price and lag the EMAsignal twice for backtesting
# Reason: buy the next day after the EMAsignal and returns are calculated using previous day value
for (i in seq(dat)) {
  dat[[i]]$EMA_SMA.return <- Return.calculate(prices = Op(dat[[i]]), method = "log") * Lag(dat[[i]]$EMA_SMA.signal, k = 2)
}

## ----plot returns from EMA+SMA strategy---------------------------------------
# Save the cumulative return data in a separate dataframe
EMASMA_plot <- NULL

for (i in seq(dat)) {
  EMASMA_plot <- cbind(EMASMA_plot,
                       10000 * exp(cumsum(na.omit(dat[[i]]$EMA_SMA.return))),
                       10000 * exp(cumsum(na.omit(dat[[i]]$total.return))))
}

colnames(EMASMA_plot) <- paste(rep(ticker, each = 2), 
                                   rep(c("EMA+SMA Trading Strategy", "Total Return"), length(dat)))

for (i in seq(from = 1, to = ncol(EMASMA_plot), by = 2)) {
  plot(EMASMA_plot[, c(i, i+1)],
       main = paste("Comparison of Cumulative Returns for", rep(ticker, each = 2)[i]),
       legend.loc = "topleft", grid.col = NA) %>% 
    print()
}

## ----annualized return and sd of EMA+SMA trading strategy---------------------
for (i in seq(dat)) {
  PerformanceAnalytics::table.AnnualizedReturns(R = dat[[i]][, c("EMA_SMA.return","total.return")],
                                                scale = 252,
                                                Rf = 0.03/252,
                                                geometric = FALSE) %>%
    `colnames<-`(paste(rep(ticker[i], 2),
                       rep(c("EMA+SMA Trading Strategy", "Total Return")))) %>%
    print()
}

