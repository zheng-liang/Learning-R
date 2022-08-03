## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.align="center", message = FALSE, warning = FALSE, rows.print = 5, max.print = 40)
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages------------------------------------------------------------
library(dplyr) # For data manipulation and piping function
library(PerformanceAnalytics) # For portfolio analysis and return calculation
library(quantmod) # For retrieving data 

## ----retrieve SPY daily-------------------------------------------------------
spy_daily <- quantmod::getSymbols(Symbols = "SPY", src = "yahoo", auto.assign = FALSE)

head(spy_daily)

# Check start and end date of data
start(spy_daily); end(spy_daily)

## ----retrieve IWM monthly-----------------------------------------------------
iwm_monthly <- quantmod::getSymbols(Symbols = "IWM", src = "yahoo", auto.assign = FALSE, 
                                    from = as.Date("2005-01-01"), to = as.Date("2022-07-01"), periodicity = "monthly")

head(iwm_monthly)

# Check start and end date of data
start(iwm_monthly); end(iwm_monthly)

## ----convert SPY periodicity--------------------------------------------------
# Convert to weekly series
spy_weekly <- xts::to.period(x = spy_daily, period = "weeks")

head(spy_weekly)

# Convert to monthly series
spy_monthly <- xts::to.period(x = spy_daily, period = "months")

head(spy_monthly)

# Convert to weekly series
spy_quarterly <- xts::to.period(x = spy_daily, period = "quarters")

head(spy_quarterly)

# Convert to weekly series
spy_yearly <- xts::to.period(x = spy_daily, period = "years")

head(spy_yearly)

## ----subset IWM data by rows--------------------------------------------------
# Subset 2013 data
iwm_monthly["2013"]

# Subset 2013 October to 2014 June data
iwm_monthly["2013-10/2014-06"]

## ----subset IWM data by columns-----------------------------------------------
# Subset opening prices
quantmod::Op(iwm_monthly) %>% head()

# Subset volume
quantmod::Vo(iwm_monthly) %>% head()

# Other similar functions are Hi(), Lo(), Cl(), Ad() for high, low, close and adjusted close prices

## ----retrieve multiple stocks-------------------------------------------------
# Tickers for Nvidia, Procter & Gamble, Mastercard, Walt Disney and Costco
tickers <- c("NVDA", "PG", "MA", "DIS", "COST")

startdate <- as.Date("2012-01-01")
enddate <- as.Date("2022-07-01")

price_data <- NULL

# Retrieve only the Adjusted Closing Price column
for(t in tickers) {
  price_data <- cbind(price_data,
                      quantmod::getSymbols(Symbols = t, src = "yahoo", auto.assign = FALSE,
                                           from = startdate, to = enddate, periodicity = "daily")) %>% Ad()
}

head(price_data)

## ----discrete returns of SPY--------------------------------------------------
# Discrete returns of spy_daily
spy_dailyR <- PerformanceAnalytics::Return.calculate(prices = Cl(spy_daily), method = "discrete")
head(spy_dailyR)

# Remove NA from spy_dailyR
spy_dailyR <- na.omit(spy_dailyR)

# Check that only first observation was removed
dim(Cl(spy_daily)); dim(spy_dailyR)

# Check that the Return.calculate() is the same as using manual calculation
head(Cl(spy_daily) / stats::lag(Cl(spy_daily)) - 1)

# Weekly, monthly, quarterly and yearly discrete returns to compare with log returns later
spy_weeklyR <- na.omit(PerformanceAnalytics::Return.calculate(prices = Cl(spy_weekly), method = "discrete"))

spy_monthlyR <- na.omit(PerformanceAnalytics::Return.calculate(prices = Cl(spy_monthly), method = "discrete"))

spy_quarterlyR <- na.omit(PerformanceAnalytics::Return.calculate(prices = Cl(spy_quarterly), method = "discrete"))

spy_yearlyR <- na.omit(PerformanceAnalytics::Return.calculate(prices = Cl(spy_yearly), method = "discrete"))

## ----log returns of SPY-------------------------------------------------------
# Log returns of spy_daily
spy_dailylog <- na.omit(PerformanceAnalytics::Return.calculate(prices = Cl(spy_daily), method = "log"))
head(spy_dailylog)

# Check that Return.calculate() is the same as using manual calculation
head(log(Cl(spy_daily) / stats::lag(Cl(spy_daily))))

# Weekly, monthly, quarterly and yearly log returns
spy_weeklylog <- na.omit(PerformanceAnalytics::Return.calculate(prices = Cl(spy_weekly), method = "log"))

spy_monthlylog <- na.omit(PerformanceAnalytics::Return.calculate(prices = Cl(spy_monthly), method = "log"))

spy_quarterlylog <- na.omit(PerformanceAnalytics::Return.calculate(prices = Cl(spy_quarterly), method = "log"))

spy_yearlylog <- na.omit(PerformanceAnalytics::Return.calculate(prices = Cl(spy_yearly), method = "log"))

## ----compare discrete and log returns-----------------------------------------
cbind(spy_dailyR, spy_dailylog) %>% `colnames<-`(c("Daily Discrete Return", "Daily Log Return")) %>% head()

cbind(spy_weeklyR, spy_weeklylog) %>% `colnames<-`(c("Weekly Discrete Return", "Weekly Log Return")) %>% head()

cbind(spy_monthlyR, spy_monthlylog) %>% `colnames<-`(c("Monthly Discrete Return", "Monthly Log Return")) %>% head()

cbind(spy_quarterlyR, spy_quarterlylog) %>% `colnames<-`(c("Quarterly Discrete Return", "Quarterly Log Return")) %>% head()

cbind(spy_yearlyR, spy_yearlylog) %>% `colnames<-`(c("Yearly Discrete Return", "Yearly Log Return")) %>% head()

## ----returns of SPY using adjusted price--------------------------------------
# Discrete returns using adjusted price
adjspy_dailyR <- na.omit(PerformanceAnalytics::Return.calculate(prices = Ad(spy_daily), method = "discrete"))

# Log returns using adjusted price
adjspy_dailylog <- na.omit(PerformanceAnalytics::Return.calculate(prices = Ad(spy_daily), method = "log"))

cbind(spy_dailyR, adjspy_dailyR, spy_dailylog, adjspy_dailylog) %>% 
  `colnames<-`(c("Discrete Return", "Discrete Adjusted Return", "Log Return", "Log Adjusted Return")) %>% 
  head()

## ----chart distribution of returns--------------------------------------------
# Chart distribution of returns
PerformanceAnalytics::chart.Histogram(R = spy_monthlyR, 
                                      main = "Distribution of SPY Monthly Returns")

# Chart density of returns and normal distribution
PerformanceAnalytics::chart.Histogram(R = spy_monthlyR, 
                                      main = "Density Plot of SPY Monthly Returns", 
                                      methods = c("add.density", "add.normal"))

# Chart distribution of returns with VaR and Modified VaR risk metrics
PerformanceAnalytics::chart.Histogram(R = spy_monthlyR,
                                      main = "VaR and Modified VaR", 
                                      methods = "add.risk")

# Chart distribution of returns with Q-Q plot
PerformanceAnalytics::chart.Histogram(R = spy_monthlyR, 
                                      main = "Return Distribution with Q-Q plot", 
                                      methods = "add.qqplot")

## ----table of distribution statistics-----------------------------------------
# Table of distribution statistics
PerformanceAnalytics::table.Distributions(R = spy_monthlyR) %>% `colnames<-`("SPY Monthly Returns")

# PerformanceAnalytics package has individual functions for these statistics
# E.g. StdDev(), skewness(), kurtosis() can have different methods of calculation

## ----chart cumulative discrete returns----------------------------------------
par(mfrow = c(2,1))

PerformanceAnalytics::chart.CumReturns(R = spy_dailyR, geometric = FALSE, main = "Arithmetic Chaining")

PerformanceAnalytics::chart.CumReturns(R = spy_dailyR, geometric = TRUE, main = "Geometric Chaining")

## ----cumsum and cumprod for return chaining-----------------------------------
# Arithmetic chaining is simply the cumulative sum of returns
cs_spyR <- base::cumsum(x = spy_dailyR)

# Geometric chaining is the cumulative product of returns
cp_spyR <- base::cumprod(x = spy_dailyR + 1) - 1

cbind(cs_spyR, cp_spyR) %>% 
  `colnames<-`(c("Cumulative Sum", "Cumulative Product")) %>% head()

## ----plot cs_spyR and cp_spyR-------------------------------------------------
par(mfrow = c(2,1))

plot(cs_spyR, main = "Cumulative Sum of SPY Daily Returns")

plot(cp_spyR, main = "Cumulative Product of SPY Daily Returns")

## ----returns of adjusted vs non-adjusted closing price------------------------
spy_returns <- cbind(spy_dailyR, adjspy_dailyR) %>% `colnames<-`(c("Non-Adjusted", "Adjusted"))

PerformanceAnalytics::charts.PerformanceSummary(R = spy_returns, 
                                                geometric = TRUE,
                                                main = "Adjusted vs Non-Adjusted Returns")

## ----plot drawdown of SPY-----------------------------------------------------
PerformanceAnalytics::chart.Drawdown(R = spy_dailyR, geometric = TRUE, main = "Drawdown of SPY")

## ----plot performance summary of SPY------------------------------------------
PerformanceAnalytics::charts.PerformanceSummary(R = spy_dailyR, geometric = TRUE, main = "SPY Performance")

## ----table drawdown summary of SPY--------------------------------------------
# Show the top 5 drawdown of SPY
PerformanceAnalytics::table.Drawdowns(R = spy_dailyR, top = 5, geometric = TRUE)

## ----portfolio returns with geometric chaining--------------------------------
# Calculate discrete returns of stocks in price_data
return_data <- na.omit(PerformanceAnalytics::Return.calculate(prices = price_data, method = "discrete"))

# Do not supply weight and set geometric = TRUE for this example
port_return1 <- PerformanceAnalytics::Return.portfolio(R = return_data, 
                                                       weights = NULL, 
                                                       geometric = TRUE, 
                                                       verbose = TRUE)

# Portfolio returns
data.frame(port_return1$returns)

# Contribution of individual asset to portfolio returns
data.frame(port_return1$contribution)

# Beginning of period weights
data.frame(port_return1$BOP.Weight)

# End of period weights
data.frame(port_return1$EOP.Weight)

# Beginning of period value
data.frame(port_return1$BOP.Value)

# End of period value
data.frame(port_return1$EOP.Value)

## ----portfolio returns without geometric chaining-----------------------------
# Set geometric = FALSE for this example
port_return2 <- PerformanceAnalytics::Return.portfolio(R = return_data, 
                                                       weights = NULL, 
                                                       geometric = FALSE, 
                                                       verbose = TRUE)

# Portfolio returns
data.frame(port_return2$returns)

# Contribution of individual asset to portfolio returns
data.frame(port_return2$contribution)

# Beginning of period weights
data.frame(port_return2$BOP.Weight)

# End of period weights
data.frame(port_return2$EOP.Weight)

## ----check returns calculation------------------------------------------------
cbind(rowSums(return_data * 0.2), port_return2$returns) %>% 
  `colnames<-`(c("Manual Calculation", "From Function")) %>% head()

## ----portfolio weights rebalanced weekly--------------------------------------
# State vector of weights in order of their columns in return_data
w <- c(0.3, 0.2, 0.15, 0.15, 0.2)

# Monthly rebalancing 
wk_rebal <- PerformanceAnalytics::Return.portfolio(R = return_data,
                                                   weights = w,
                                                   geometric = TRUE,
                                                   rebalance_on = "weeks", 
                                                   verbose = TRUE)

# Portfolio returns
data.frame(wk_rebal$returns)

# Contribution of individual asset to portfolio returns
data.frame(wk_rebal$contribution)

# Beginning of period weights
data.frame(wk_rebal$BOP.Weight)

# End of period weights
data.frame(wk_rebal$EOP.Weight)

## ----time series of weights and rebalancing-----------------------------------
# Starting weights on 2012-01-04 and change weights on 2012-01-06
# Need to -1 from dates because rebalanced weights take effect on next day 
# Think of it as setting the EOP weights
ts_w <- xts(x = rbind(w, c(0.2, 0.3, 0.15, 0.2, 0.15)),
            order.by = index(return_data[c(1,3),]) - 1)

ts_w

rebal_ts_w <- PerformanceAnalytics::Return.portfolio(R = return_data, 
                                                     weights = ts_w,
                                                     geometric = TRUE, 
                                                     rebalance_on = "weeks",
                                                     verbose = TRUE)

# Beginning of period weights
data.frame(rebal_ts_w$BOP.Weight)

# End of period weights
data.frame(rebal_ts_w$EOP.Weight)

## ----plot weights of rebal and no rebal---------------------------------------
# Plot weights of portfolio that is not rebalanced frequently
plot(port_return1$BOP.Weight, legend.loc = "topleft")

# Plot weights of portfolio rebalanced weekly
plot(wk_rebal$BOP.Weight, legend.loc = "left")

## ----plot portfolio with benchmark--------------------------------------------
# Subset SPY returns such that it starts and ends on the same periods as the portfolio
spy_benchmark <- adjspy_dailyR[paste(as.character(start(return_data)), "/", as.character(end(return_data)), sep = ""),]

# Merge returns of portfolio with and without rebalancing and the spy_benchmark
return_comp <- cbind(port_return1$returns, wk_rebal$returns, spy_benchmark) %>%
  `colnames<-`(c("No Rebalancing", "Weekly Rebalancing", "SPY"))

PerformanceAnalytics::charts.PerformanceSummary(R = return_comp, 
                                                main = "Performance of Portfolios Against Benchmark", 
                                                geometric = TRUE, 
                                                legend.loc = "topleft")

## ----use plotly engine for performance chart----------------------------------
PerformanceAnalytics::chart.CumReturns(R = return_comp, 
                                       main = "Cumulative Return Using Plotly", 
                                       geometric = TRUE, 
                                       legend.loc = "topleft",
                                       plot.engine = "plotly")

## ----annualized returns-------------------------------------------------------
# scale = 252 because returns are in daily periodicity. Use 52 for weekly, 12 for monthly, 4 for quarterly
# Input risk-free rate to calculate Sharpe Ratio, which should be in the same periodicity as returns
PerformanceAnalytics::table.AnnualizedReturns(R = return_comp, scale = 252, Rf = 0.03/252, geometric = TRUE)

## ----capm measures------------------------------------------------------------
# Use SPY as benchmark (Rb)
PerformanceAnalytics::table.CAPM(Ra = return_comp[,1:2], Rb = return_comp[,3], scale = 252, Rf = 0.03/252)

## ----downside risk measure----------------------------------------------------
# Input Minimum Acceptable Rate (MAR) to calculate downside deviation
PerformanceAnalytics::table.DownsideRisk(R = return_comp, scale = 252, Rf = 0.03/252, MAR = 0.07/252)

## ----summary of return statistics---------------------------------------------
PerformanceAnalytics::table.Stats(R = return_comp)

