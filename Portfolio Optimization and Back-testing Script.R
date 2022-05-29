# Project 1: Portfolio Optimization, Re-balancing and Back-testing

# Load package
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(dplyr)

# Getting To Know Some Useful Functions --------------------------------------------------

# Getting data from Yahoo Finance for a single stock, example used is Amazon
AMZN <- getSymbols(Symbols = "AMZN",
                   src = "yahoo",
                   periodicity = "daily",
                   auto.assign = F)
View(AMZN)
# Understanding object AMZN
str(AMZN)
head(AMZN)
tail(AMZN)
summary(AMZN)
colSums(is.na(AMZN))
# Sub-setting the Adjusted Closing Price column
AMZN.Adj <- AMZN[,6]
View(AMZN.Adj)
# Determining daily returns of AMZN using discrete calculation
# Using discrete returns allows us to use additive properties when adding
# stocks to portfolios. Using method = "log" would result in non-linearity.
AMZN_Returns <- na.omit(Return.calculate(AMZN.Adj, method = "discrete"))
# We can also chart the cumulative return of AMZN
chart.CumReturns(AMZN_Returns, 
                 legend.loc = "topleft",
                 geometric = T,
                 main = "Cumulative Daily Return of Amazon")



# Building a Portfolio With 2 or More Stocks ------------------------------

# The above is for getting to understand the features that will be used
# for building a portfolio with 2 or more stocks down below

# Create a vector of tickers that are to be added into portfolio 
# Ticker naming convention follows Yahoo Finance.
tickers <- c("JNJ", "PG", "AAPL", "TSM", "MSFT", "NVDA")

# Obtain 6th column (Adjusted Closing Price) from the table of each stock in
# tickers and adding them into price_data
price_data <- NULL

startdate <- "2012-01-01"
enddate <- "2022-05-25"
for (ticker in tickers) {
  price_data <- cbind(price_data, 
                      getSymbols(ticker, 
                                 src = "yahoo",
                                 from = startdate, to = enddate, 
                                 periodicity = "daily", auto.assign = F)[,6])
}
colnames(price_data) <- tickers

# Understanding object price_data and checking for missing data
head(price_data)
tail(price_data)
nrow(price_data)
colSums(is.na(price_data))

## Calculate daily return (discrete) of each stock in price_data
returns <- na.omit(Return.calculate(price_data, method = "discrete"))



## Calculate portfolio return ====================================
# default is an equal weight portfolio
r_noRebal <- Return.portfolio(returns,
                              geometric = T,
                              verbose = T)

  colnames(r_noRebal$returns) <- ("Rp_noRebal")

  lapply(r_noRebal, head, n = 4)

r_withRebal <- Return.portfolio(returns, 
                                geometric = T, 
                                rebalance_on = "quarters", 
                                verbose = T)

  colnames(r_withRebal$returns) <- ("Rp_withRebal")

ret_comp <- cbind(r_noRebal$returns, r_withRebal$returns)

charts.PerformanceSummary(R = ret_comp,
                          main = "Comparison of Cumulative Returns",
                          legend.loc = "topleft")

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

stats <- table.Stats(ret_comp)



## Create a benchmark for comparison of performance (market portfolio: S&P500) =====
benchmark <- getSymbols("SPY", 
                        src = "yahoo",
                        from = startdate, to = enddate, 
                        periodicity = "daily", auto.assign = F)[,6]
View(benchmark)
colSums(is.na(benchmark))

# Calculate benchmark return
benchmarkReturn <- na.omit(Return.calculate(benchmark, method = "discrete")) %>%
  `colnames<-`("SPY")

## Some Performance Metrics =========================================
# Risk-free rate assume 3%, but need to remember that data frequency is daily
perfMetrics <- round(rbind(CAPM.beta(ret_comp, benchmarkReturn, Rf = 0.03/252),
                           CAPM.alpha(ret_comp, benchmarkReturn, Rf = 0.03/252),
                           SharpeRatio(ret_comp, Rf = 0.03/252, FUN = "StdDev"),
                           TreynorRatio(ret_comp, benchmarkReturn, Rf = 0.03/252, scale = 252),
                           InformationRatio(ret_comp, benchmarkReturn)), digits = 4)

table.CAPM(ret_comp, benchmarkReturn, Rf=0.03/252, scale=252)

# To view annualized return and breakdown of return by months
# and compare between portfolio and benchmark
cbind(table.AnnualizedReturns(ret_comp, Rf = 0.03/252),
      table.AnnualizedReturns(benchmarkReturn, Rf = 0.03/252))

table.CalendarReturns(exp_portReturns$returns)
table.CalendarReturns(benchmarkReturn)



# Portfolio Optimization --------------------------------------------------

# Instead of equally weighted portfolio, we can optimize the portfolio by
# maximizing return (mean) or minimizing risk (variance/standard deviation)
 
## Case 1: Minimizing Variance =============================================
# Adding constraints and objectives to portspec1 using portfolio.spec()
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

# Optimization of weights in portfolio based on minimum variance
# We use "quadprog" as we are solving a quadratic optimization problem
# Inputting "ROI" instead automatically chooses "quadprog" for this OP
port_MV <- optimize.portfolio(returns, 
                              portspec1, 
                              optimize_method = "quadprog",
                              trace = T)
port_MV
## Case 2: Maximizing Return ===============================================
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
# Optimization of weights in portfolio based on maximum return
# We use "glpk" as we are solving a linear optimization problem
# Inputting "ROI" instead automatically chooses "glpk" for this OP
port_MR <- optimize.portfolio(returns, 
                              portspec2,
                              optimize_method = "glpk",
                              trace = T)

port_MR

## Compare returns and standard deviation of portfolios in Case 1 and 2 ======
# Extract weights from the optimized portfolios
weight_MV <- extractWeights(port_MV)

weight_MR <- extractWeights(port_MR)

par(mfrow = c(2,1), mar = c(2, 4, 2, 2))

barplot(weight_MV, 
        main = "Weights in Different Portfolios",
        ylab = "Portfolio MV")

barplot(weight_MR,
        ylab = "Portfolio MR")

# Calculate portfolio return with same periodicity as data input
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

# Calculate annualized return, standard deviation and Sharpe ratio
# Assume risk-free rate to be 3%, remember to change it to daily values
comparison <- cbind(rp_MV, rp_MR, benchmarkReturn)

charts.PerformanceSummary(comparison,
                          main = "Comparing Performance of Portfolios",
                          legend.loc = "topleft")

table.Stats(comparison)

table.AnnualizedReturns(comparison, 0.03/252, scale = 252)


# Back-testing Performance -----------------------------------------------

## Case 3: Mean-Variance Portfolio Optimization ==========================
portspec3 <- portfolio.spec(colnames(returns))

# Slight change in weight_sum to reduce restrictiveness while optimizing
portspec3 <- add.constraint(portspec3, 
                            type = "weight_sum",
                            min_sum=0.99, max_sum=1.01)

portspec3 <- add.constraint(portspec3, 
                            type="box", 
                            min=0, max=0.4)

# Include transaction cost, assumed to be 2%
portspec3 <- add.constraint(portspec3,
                            type = "transaction_cost",
                            ptc = 0.02)

# Add a constraint to have a target mean
portspec3 <- add.constraint(portspec3,
                            type = "return",
                            return_target = 0.15/252)

# Adding a return and risk objective, thus we maximize mean return per unit of risk
portspec3 <- add.objective(portspec3,
                           type = "return",
                           name = "mean")

portspec3 <- add.objective(portspec3,
                           type = "risk",
                           name = "var")

# Finding optimal portfolio weights using estimation period
# Generate a set of random portfolios that will be used in optimization
randport <- random_portfolios(portfolio = portspec3,
                              permutations = 20000,
                              rp_method = "simplex")

# Quarterly re-balancing back-test
port_MeanVar_qRebal <- optimize.portfolio.rebalancing(returns,
                                                      portspec3,
                                                      optimize_method = "random",
                                                      rebalance_on = "quarters",
                                                      # max Sharpe 
                                                      maxSR = T,
                                                      rp = randport,
                                                      search_size = 5000,
                                                      training_period = 252,
                                                      rolling_window = 45,
                                                      trace = T)
chart.Weights(port_MeanVar_qRebal)

w_qOptim <- extractWeights(port_MeanVar_qRebal)

rp_MeanVar <- Return.portfolio(returns, 
                               weights = w_qOptim, 
                               geometric = T)

evaluation <- cbind(rp_MeanVar,benchmarkReturn)

charts.PerformanceSummary(evaluation, Rf=0.03/252)

table.AnnualizedReturns(evaluation, Rf=0.03/252, scale = 252)
