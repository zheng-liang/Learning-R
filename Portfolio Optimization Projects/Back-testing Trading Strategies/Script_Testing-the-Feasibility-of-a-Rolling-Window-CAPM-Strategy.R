## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.align="center")
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages, message=FALSE, warning=FALSE------------------------------
library(tidyquant) # Loads quantmod and PerformanceAnalytics packages
library(tidyverse) # Loads dplyr and ggplot2 packages (data manipulation and plotting)

## ----obtain prices of AAPL and SPY--------------------------------------------
# Set start and end date for data retrieval (does not work when retrieving data from FRED)
startdate <- as.Date("2017-12-31")
enddate <- as.Date("2022-11-01")

spy <- quantmod::getSymbols(Symbols = "SPY", src = "yahoo", auto.assign = FALSE,
                            from = startdate, to = enddate, periodicity = "daily")

aapl <- quantmod::getSymbols(Symbols = "AAPL", src = "yahoo", auto.assign = FALSE,
                            from = startdate, to = enddate, periodicity = "daily")

## ----obtain 1-month US T-Bill yield-------------------------------------------
# Retrieving data from FRED automatically retrieves the whole series available
US1M <- quantmod::getSymbols(Symbols = "DGS1MO", src = "FRED", auto.assign = FALSE)

rfr <- US1M["2017-12-31/2022-10-31"]

## ----merge data---------------------------------------------------------------
# Merge adjusted closing prices (col. 6) of AAPL and SPY with risk-free rate
dat <- merge(aapl[,6], spy[,6], rfr, all = F)

# Rename columns
colnames(dat) <- c("AAPL", "SPY", "RFR")

# Check for missing data
colSums(is.na(dat))

# Replace NA in rfr with last observation and convert to daily rate by dividing 252
dat$RFR <- na.locf(dat$RFR)/252

# Preview data
head(dat)

## ----calculate log returns for stocks-----------------------------------------
# Calculate log returns for AAPL and SPY using PerformanceAnalytics::Return.calculate()
# Convert returns to percentage
dat <- cbind(dat, 
             PerformanceAnalytics::Return.calculate(prices = dat$AAPL, method = "log") * 100, 
             PerformanceAnalytics::Return.calculate(prices = dat$SPY, method = "log") * 100) %>%
  `colnames<-`(c(colnames(dat), "rAAPL", "rSPY"))

head(dat)

## ----beta and expected return-------------------------------------------------
dat$beta <- rollapply(data = dat[-1, 4:5], width = 5, by.column = F, 
                      function(x) CAPM.beta(Ra = x[,1], Rb = x[,2]))

dat$exp.return <- rollapply(data = dat[-1, 4], width = 5, mean)

## ----required return----------------------------------------------------------
dat$mean.RFR <- rollapply(data = dat[-1, 3], width = 5, mean)

dat$mean.RM <- rollapply(data = dat[-1, 5], width = 5, mean)

dat$req.return <- dat$mean.RFR + dat$beta * (dat$mean.RM - dat$mean.RFR)

## ----trading signal-----------------------------------------------------------
# Create a new xts object saving only the needed columns of data
dat2 <- na.omit(merge(dat[,c(7,10)], aapl$AAPL.Open))

# Use the Lag() function to shift the results down one row
dat2$signal <- Lag(dat2$exp.return > dat2$req.return & dat2$exp.return > 0)

## ----returns from trading strategy--------------------------------------------
# Signal created can be seen as the weight of the asset on a particular day
# Need to lag one more period as I assume a buy or sell on the next trading day opening price
trading_returns <- Return.calculate(prices = dat2$AAPL.Open, method = "log") * Lag(dat2$signal)

# Plot cumulative return from trading strategy by converting log returns to standard returns
# Assume starting capital of $10,000
plot(10000*(exp(cumsum(na.omit(trading_returns)))), 
     main = "Cumulative Return from CAPM Trading Strategy",
     grid.col = NA)

## ----plot returns for comparison----------------------------------------------
return_plot <- merge(10000*(exp(cumsum(na.omit(dat$rAAPL/100)))), 
           10000*(exp(cumsum(na.omit(trading_returns)))), 
           10000*(exp(cumsum(na.omit(dat$rSPY/100)))))

colnames(return_plot) <- c("Buy-and-Hold", "CAPM Trading Strategy", "S&P 500 ETF")

plot(return_plot, 
     main = "Comparison of Cumulative Returns", 
     legend.loc = "topleft",
     grid.col = NA)

## ----risk and return metrics--------------------------------------------------
return_dat <- merge(dat$rAAPL/100, trading_returns, dat$rSPY/100)

colnames(return_dat) <- c("Buy-and-Hold", "CAPM Trading Strategy", "S&P 500 ETF")

table.AnnualizedReturns(R = return_dat, scale = 252, geometric = FALSE)

