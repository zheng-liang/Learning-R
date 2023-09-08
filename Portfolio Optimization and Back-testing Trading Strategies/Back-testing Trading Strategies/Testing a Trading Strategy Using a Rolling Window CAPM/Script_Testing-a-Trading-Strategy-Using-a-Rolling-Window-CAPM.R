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
# Merge closing prices of AAPL and SPY with risk-free rate
dat <- merge(Cl(aapl), Cl(spy), rfr, all = FALSE)

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

## ----beta required return and expected return---------------------------------
dat$beta <- rollapply(data = dat[-1, 4:5], width = 30, by.column = F, 
                      function(x) CAPM.beta(Ra = x[,1], Rb = x[,2]))

# Calculate the required return for AAPL based on CAPM each day, after obtaining the average market return and risk-free rate:
dat$mean.RFR <- SMA(x = dat$RFR[-1], n = 30)

dat$mean.RM <- SMA(x = dat$rSPY[-1], n = 30)

dat$req.return <- dat$mean.RFR + dat$beta * (dat$mean.RM - dat$mean.RFR)

dat$exp.return <- SMA(x = dat$rAAPL[-1], n = 30)

## ----trading signal, class.source = 'fold-hide'-------------------------------
# Create a new xts object saving only the needed columns of data
dat2 <- na.omit(merge(dat[,c("req.return","exp.return")], Op(aapl)))

# Include one-period lag to indicate buy/sell on the next period
dat2$signal <- Lag(dat2$exp.return > dat2$req.return & dat2$req.return > 0)

# Create a position column
dat2$position <- 0

for (i in 2:nrow(dat2)) {
  if (i == 2 & dat2$signal[i] == 1) {
    dat2$position[i] <- 1
  } else {
    dat2$position[i] <- 0
  }
  
  if (i > 2 & dat2$signal[i] == 1) {
    if (dat2$signal[i-1] == 0) {
      dat2$position[i] <- 1
    } else {
      dat2$position[i] <- 0
    }
  }
  
  if (i > 2 & dat2$signal[i] == 0) {
    if (dat2$signal[i-1] == 1) {
      dat2$position[i] <- -1
    } else {
      dat2$position[i] <- 0
    }
  }
}

# Remove the missing value due to lagging the signal by one period
dat2 <- na.omit(dat2)

head(dat2)

## ----returns from trading strategy, class.source = 'fold-hide'----------------
# Assume that I started with $10,000 capital and buy as many shares as possible with
# the portfolio value available
# Also assume a 0.5% buy/sell transaction cost
portfolio_value <- xts(matrix(nrow = nrow(dat2), ncol = 2, dimnames = list(index(dat2), c("Stock.Value", "Cash"))), order.by = index(dat2))

for (i in seq(nrow(dat2))) {
  if (i == 1 & dat2$position[i] == 1) {
    
    # Apply 0.5% to stock price when calculating number of shares to buy to ensure that 
    # there is cash available to pay the transaction cost
    portfolio_value$Stock.Value[i] <- floor(10000 / (dat2$AAPL.Open[i] * 1.005)) * dat2$AAPL.Open[i]
    
    portfolio_value$Cash[i] <- 10000 - floor(10000 / (dat2$AAPL.Open[i] * 1.005)) * dat2$AAPL.Open[i] * 1.005
    
  } else if (i == 1 & dat2$position[i] == 0) {
    
    portfolio_value$Stock.Value[i] <- 0
    
    portfolio_value$Cash[i] <- 10000
    
  } else if (i > 1 & dat2$position[i] == 1) {
    
    portfolio_value$Stock.Value[i] <- floor(as.vector(portfolio_value$Cash)[i-1] / (as.vector(dat2$AAPL.Open)[i] * 1.005)) * as.vector(dat2$AAPL.Open)[i]
    
    portfolio_value$Cash[i] <- as.vector(portfolio_value$Cash)[i-1] - floor(as.vector(portfolio_value$Cash)[i-1] / (as.vector(dat2$AAPL.Open)[i] * 1.005)) * dat2$AAPL.Open[i] * 1.005
    
  } else if (i > 1 & dat2$position[i] == -1) {
    
    portfolio_value$Stock.Value[i] <- 0
    
    portfolio_value$Cash[i] <- as.vector(portfolio_value$Cash)[i-1] + as.vector(portfolio_value$Stock.Value)[i-1] * 0.995
    
  } else if (i > 1 & dat2$position[i] == 0 & dat2$signal[i] == 1) {
    
    portfolio_value$Stock.Value[i] <- as.vector(portfolio_value$Stock.Value)[i-1] / as.vector(dat2$AAPL.Open)[i-1] * dat2$AAPL.Open[i]
    
    portfolio_value$Cash[i] <- portfolio_value$Cash[i-1]
    
  } else {
    
    portfolio_value$Stock.Value[i] <- 0
    
    portfolio_value$Cash[i] <- portfolio_value$Cash[i-1]
    
  }
}

plot(portfolio_value$Stock.Value + portfolio_value$Cash, 
     main = "Cumulative Return from CAPM Trading Strategy",
     grid.col = NA)

## ----plot returns for comparison, class.source = 'fold-hide'------------------
return_plot <- merge(10000*(exp(cumsum(na.omit(Return.calculate(prices = Ad(aapl), method = "log"))))), 
                     portfolio_value$Stock.Value + portfolio_value$Cash, 
                     10000*(exp(cumsum(na.omit(Return.calculate(prices = Ad(spy), method = "log"))))))

colnames(return_plot) <- c("Buy-and-Hold", "CAPM Trading Strategy", "S&P 500 ETF")

plot(return_plot,
     main = "Comparison of Cumulative Returns",
     legend.loc = "topleft",
     grid.col = NA)

## ----risk and return metrics--------------------------------------------------
return_dat <- merge(Return.calculate(prices = Ad(aapl), method = "log"), 
                    Return.calculate(prices = portfolio_value$Stock.Value + portfolio_value$Cash, method = "log"), 
                    Return.calculate(prices = Ad(spy), method = "log"))

colnames(return_dat) <- c("Buy-and-Hold", "CAPM Trading Strategy", "S&P 500 ETF")

table.AnnualizedReturns(R = return_dat, scale = 252, geometric = FALSE)

