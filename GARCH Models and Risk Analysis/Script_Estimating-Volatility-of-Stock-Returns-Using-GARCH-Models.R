## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages, message=FALSE, warning=FALSE------------------------------
library(dplyr)
library(forecast) # For ARIMA models, forecasting and evaluation
library(PerformanceAnalytics) # For portfolio performance and risk analysis
library(PortfolioAnalytics) # For portfolio optimization and analysis
library(quantmod) # For obtaining historical prices from Yahoo Finance
library(rugarch) # For univariate GARCH models
library(urca) # For unit root tests

## ----load AAPL price data-----------------------------------------------------
# Load AAPL price data from Yahoo Finance using quantmod package

# Set start and end dates for data retrieval
startdate <- as.Date("2010-01-01")
enddate <- as.Date("2022-07-01")

# Retrieve price data
AAPL_price <- quantmod::getSymbols(Symbols = "AAPL", 
                                   src = "yahoo", 
                                   from = startdate, to = enddate, 
                                   periodicity = "weekly", 
                                   auto.assign = F)

# View the first and last 6 observations in AAPL_price
c(head(AAPL_price), tail(AAPL_price))

# Check number of observations
nrow(AAPL_price)

# Check for missing data
colSums(is.na(AAPL_price))

## ----calculate log returns, fig.align='center'--------------------------------
# Calculate log/continuous returns using Adjusted column (column 6)

# Remove first row of calculated returns since returns cannot be calculated for first observation
rAAPL <- PerformanceAnalytics::Return.calculate(prices = AAPL_price[, 6], method = "log")[-1,] * 100
colnames(rAAPL) <- "Returns"

# Check that first row has been removed
head(rAAPL)
nrow(rAAPL)

# Chart weekly log-returns over time
plot.zoo(rAAPL, main = "Weekly Log-Returns of AAPL", xlab = "Time", ylab = "Log-Return (in %)")

## ----rolling volatility of AAPL returns, fig.align='center'-------------------
# Plot rolling volatility of AAPL returns

# width = 4 to approximately calculate annualized sd using a monthly rolling-window
PerformanceAnalytics::chart.RollingPerformance(R = rAAPL, width = 4, FUN = "sd.annualized", scale = 52, main = "1-Month Rolling Volatility of AAPL Returns")

## ----ADF test on AAPL returns-------------------------------------------------
# ADF Test with Drift, lags selected based on AIC

rAAPL %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

# KPSS Test

rAAPL %>% urca::ur.kpss(type = "mu", lags = "long") %>% summary()

## ----distribution of AAPL returns, fig.align='center'-------------------------
# Plot distribution of AAPL's weekly log returns

PerformanceAnalytics::chart.Histogram(R = rAAPL,
                                      main = "Distribution of AAPL Weekly Log-Returns", 
                                      methods = c("add.density", "add.normal"), 
                                      colorset = c("blue", "red", "black"),
                                      ylim = c(0, 0.15))

legend(x = "topleft", legend = c("Log-Return", "Density", "Normal"), col = c("blue", "red", "black"), lty = 1)

## ----descriptive stats of AAPL returns----------------------------------------
# Look at skewness and kurtosis of AAPL returns

PerformanceAnalytics::table.Distributions(R = rAAPL)

## ----split data---------------------------------------------------------------
# Split the data for training and testing models
trainset <- rAAPL["/2022-04",]
nrow(trainset)

testset <- rAAPL["2022-05/",]
nrow(testset)

## ----estimate mean equation using ARMA model----------------------------------
# Find mean equation using auto.arima from forecast package

meaneqn <- forecast::auto.arima(y = trainset, 
                                max.p = 2, max.q = 2, 
                                stepwise = F, approximation = F, trace = F, 
                                ic = "aic", method = "ML")
summary(meaneqn)

## ----create ARCH(1) model specification---------------------------------------
# Create ARCH(1) model specification using rugarch package, arguments to be in a list
# Check ?ugarchspec for the arguments

# Specify ARCH(1) model by setting order of GARCH terms to 0
archmodel <- list(model = "sGARCH", garchOrder = c(1, 0))

# Specify mean model as found using auto.arima
meanmodel <- list(armaOrder = c(0, 0), include.mean = T)

# Assume a normal distribution of the error term z_t
archspec <- rugarch::ugarchspec(variance.model = archmodel, mean.model = meanmodel, distribution.model = "norm")

## ----fit ARCH(1) model with normal distribution-------------------------------
# Fit the data to the ARCH(1) specification

fitARCH <- rugarch::ugarchfit(spec = archspec, data = trainset, solver = "hybrid")

fitARCH

## ----ARCH(2) model with normal distribution-----------------------------------
# Specify an ARCH(2) model
arch2model <- list(model = "sGARCH", garchOrder = c(2, 0))

# Assume normal distribution of error term z_t
arch2spec <- rugarch::ugarchspec(variance.model = arch2model, mean.model = meanmodel, distribution.model = "norm")

fitARCH2 <- ugarchfit(spec = arch2spec, data = trainset, solver = "hybrid")

fitARCH2

## ----ARCH(2) model with t-distribution----------------------------------------
# Assume a normal distribution of the error term z_t
arch2spec_std <- rugarch::ugarchspec(variance.model = arch2model, mean.model = meanmodel, distribution.model = "std")

fitARCH2_std <- ugarchfit(spec = arch2spec_std, data = trainset, solver = "hybrid")

fitARCH2_std

## ----ARCH(2) model with skewed t-distribution---------------------------------
# Use skewed t-distribution

arch2spec_sstd <- ugarchspec(variance.model = arch2model, mean.model = meanmodel, distribution.model = "sstd")

fitARCH2_sstd <- rugarch::ugarchfit(spec = arch2spec_sstd, data = trainset, solver = "hybrid")

fitARCH2_sstd

## ----plot ARCH(2) with t-distribution, fig.align='center'---------------------
# Plot some data from the ARCH(2) with t-distribution

par(mfrow = c(2, 2))

# Plot 1: Actual return series with conditional SD
# Plot 3: Conditional volatility vs absolute returns
# Plot 8: Distribution of residuals
# Plot 9: Q-Q plot of residuals to check for normality

plotnum <- c(1, 3, 8, 9)

for (i in plotnum) {
  plot(fitARCH2_std, which = i)
}

## ----GARCH(1,1) with normal distribution--------------------------------------
# Specify GARCH(1,1) model 
garchmodel <- list(model = "sGARCH", garchOrder = c(1, 1))

# Assume a normal distribution of the error term z_t
garchspec <- rugarch::ugarchspec(variance.model = garchmodel, mean.model = meanmodel, distribution.model = "norm")

# Fit GARCH(1, 1)
fitGARCH11 <- rugarch::ugarchfit(spec = garchspec, data = trainset, solver = "hybrid")

fitGARCH11

## ----GARCH(1,1) with t-distribution-------------------------------------------
# GARCH(1, 1) with errors that follow a t-distribution

garchspec_std <- rugarch::ugarchspec(variance.model = garchmodel, mean.model = meanmodel, distribution.model = "std")

fitGARCH11_std <- rugarch::ugarchfit(spec = garchspec_std, data = trainset, solver = "hybrid")

fitGARCH11_std

## ----GARCH(1,1) with skewed t-distribution------------------------------------
# GARCH(1, 1) with errors that follow a skewed t-distribution

garchspec_sstd <- rugarch::ugarchspec(variance.model = garchmodel, mean.model = meanmodel, distribution.model = "sstd")

fitGARCH11_sstd <- rugarch::ugarchfit(spec = garchspec_sstd, data = trainset, solver = "hybrid")

fitGARCH11_sstd

## ----plot GARCH(1,1) with t-distribution, fig.align='center'------------------
# Plot some data from the GARCH(1, 1) with t-distribution

par(mfrow = c(2, 2))

for (i in plotnum) {
  plot(fitGARCH11_std, which = i)
}

## ----GJR-GARCH(1,1) with normal distribution----------------------------------
# Specify GJR-GARCH(1,1) model 
gjrmodel <- list(model = "gjrGARCH", garchOrder = c(1, 1))

# Assume a normal distribution of the error term z_t
gjrspec <- rugarch::ugarchspec(variance.model = gjrmodel, mean.model = meanmodel, distribution.model = "norm")

# Fit GARCH(1, 1)
fitGJR11 <- rugarch::ugarchfit(spec = gjrspec, data = trainset, solver = "hybrid")

fitGJR11

## ----GJR-GARCH(1,1) with t-distribution---------------------------------------
# Assume Student's t-distribution of error term z_t
gjrspec_std <- rugarch::ugarchspec(variance.model = gjrmodel, mean.model = meanmodel, distribution.model = "std")

# Fit GARCH(1, 1)
fitGJR11_std <- rugarch::ugarchfit(spec = gjrspec_std, data = trainset, solver = "hybrid")

fitGJR11_std

## ----GJR-GARCH(1,1) with skewed t-distribution--------------------------------
# Assume skewed t-distribution of error term z_t
gjrspec_sstd <- rugarch::ugarchspec(variance.model = gjrmodel, mean.model = meanmodel, distribution.model = "sstd")

# Fit GARCH(1, 1)
fitGJR11_sstd <- rugarch::ugarchfit(spec = gjrspec_sstd, data = trainset, solver = "hybrid")

fitGJR11_sstd

## ----plot GJR-GARCH(1,1) with t-distribution, fig.align='center'--------------
# Plot some data from the GJR-GARCH(1, 1) with t-distribution

par(mfrow = c(2, 2))

for (i in plotnum) {
  plot(fitGJR11_std, which = i)
}

## ----EGARCH(1,1) with normal distribution-------------------------------------
# Specify EGARCH(1,1) model 
egarchmodel <- list(model = "eGARCH", garchOrder = c(1, 1))

# Assume a normal distribution of the error term z_t
egarchspec <- rugarch::ugarchspec(variance.model = egarchmodel, mean.model = meanmodel, distribution.model = "norm")

# Fit GARCH(1, 1)
fitEGARCH11 <- rugarch::ugarchfit(spec = egarchspec, data = trainset, solver = "hybrid")

fitEGARCH11

## ----EGARCH(1,1) with t-distribution------------------------------------------
# Assume a t-distribution of the error term z_t
egarchspec_std <- rugarch::ugarchspec(variance.model = egarchmodel, mean.model = meanmodel, distribution.model = "std")

# Fit GARCH(1, 1)
fitEGARCH11_std <- rugarch::ugarchfit(spec = egarchspec_std, data = trainset, solver = "hybrid")

fitEGARCH11_std

## ----EGARCH(1,1) with skewed t-distribution-----------------------------------
# Assume a skewed t-distribution of the error term z_t
egarchspec_sstd <- rugarch::ugarchspec(variance.model = egarchmodel, mean.model = meanmodel, distribution.model = "sstd")

# Fit GARCH(1, 1)
fitEGARCH11_sstd <- rugarch::ugarchfit(spec = egarchspec_sstd, data = trainset, solver = "hybrid")

fitEGARCH11_sstd

## ----plot EGARCH(1,1) with t-distribution, fig.align='center'-----------------
# Plot some data from the EGARCH(1, 1) with t-distribution

par(mfrow = c(2, 2))

for (i in plotnum) {
  plot(fitEGARCH11_std, which = i)
}

## ----high low price-----------------------------------------------------------
# Extract high (column 2) and low (column 3) prices within test period

proxy <- AAPL_price["2022-05/", 2:3]

proxy$Volatility <- log(proxy[, 1] / proxy[, 2]) * 100

proxy

## ----forecast ARCH(2) models--------------------------------------------------
# Forecast ARCH(2) with normal distribution of innovations 8 periods ahead
fcstARCH2_nd <- rugarch::ugarchforecast(fitORspec = fitARCH2, n.ahead = 8)

# Forecast ARCH(2) with t-distribution of innovations 8 periods ahead
fcstARCH2_std <- rugarch::ugarchforecast(fitORspec = fitARCH2_std, n.ahead = 8)

# Forecast ARCH(2) with t-distribution of innovations 8 periods ahead
fcstARCH2_sstd <- rugarch::ugarchforecast(fitORspec = fitARCH2_sstd, n.ahead = 8)

## ----store forecast of ARCH---------------------------------------------------
fcstARCH2 <- cbind(sigma(fcstARCH2_nd), sigma(fcstARCH2_std), sigma(fcstARCH2_sstd)) %>%
  `colnames<-`(c("ARCH2_ND", "ARCH2_STD", "ARCH2_SSTD"))

fcstARCH2

## ----evaluation metrics for ARCH(2) models------------------------------------
evalARCH <- NULL

for (i in 1:3) {
  mse <- mean((proxy$Volatility^2 - fcstARCH2[,i]^2)^2)
  
  rmse <- sqrt(mse)
  
  mae <- mean(abs(proxy$Volatility^2 - fcstARCH2[,i]^2))
  
  evalARCH <- rbind(evalARCH, c(MSE = mse, RMSE = rmse, MAE= mae))
}

rownames(evalARCH) <- colnames(fcstARCH2)

evalARCH

## ----forecast GARCH(1,1) models-----------------------------------------------
# Forecast GARCH(1, 1) with normal distribution of innovations 8 periods ahead
fcstGARCH11_nd <- rugarch::ugarchforecast(fitORspec = fitGARCH11, n.ahead = 8)

# Forecast GARCH(1, 1) with t-distribution of innovations 8 periods ahead
fcstGARCH11_std <- rugarch::ugarchforecast(fitORspec = fitGARCH11_std, n.ahead = 8)

# Forecast GARCH(1, 1) with t-distribution of innovations 8 periods ahead
fcstGARCH11_sstd <- rugarch::ugarchforecast(fitORspec = fitGARCH11_sstd, n.ahead = 8)

## ----store forecast of GARCH--------------------------------------------------
fcstGARCH11 <- cbind(sigma(fcstGARCH11_nd), sigma(fcstGARCH11_std), sigma(fcstGARCH11_sstd)) %>%
  `colnames<-`(c("GARCH11_ND", "GARCH11_STD", "GARCH11_SSTD"))

fcstGARCH11

## ----evaluation metrics for GARCH(1,1) models---------------------------------
evalGARCH <- NULL

for (i in 1:3) {
  mse <- mean((proxy$Volatility^2 - fcstGARCH11[,i]^2)^2)
  
  rmse <- sqrt(mse)
  
  mae <- mean(abs(proxy$Volatility^2 - fcstGARCH11[,i]^2))
  
  evalGARCH <- rbind(evalGARCH, c(MSE = mse, RMSE = rmse, MAE= mae))
}

rownames(evalGARCH) <- colnames(fcstGARCH11)

evalGARCH

## ----forecast GJR-GARCH(1,1) models-------------------------------------------
# Forecast GJR-GARCH(1, 1) with normal distribution of innovations 8 periods ahead
fcstGJR11_nd <- rugarch::ugarchforecast(fitORspec = fitGJR11, n.ahead = 8)

# Forecast GJR-GARCH(1, 1) with t-distribution of innovations 8 periods ahead
fcstGJR11_std <- rugarch::ugarchforecast(fitORspec = fitGJR11_std, n.ahead = 8)

# Forecast GJR-GARCH(1, 1) with t-distribution of innovations 8 periods ahead
fcstGJR11_sstd <- rugarch::ugarchforecast(fitORspec = fitGJR11_sstd, n.ahead = 8)

## ----store forecast of GJR-GARCH----------------------------------------------
fcstGJR11 <- cbind(sigma(fcstGJR11_nd), sigma(fcstGJR11_std), sigma(fcstGJR11_sstd)) %>%
  `colnames<-`(c("GJR11_ND", "GJR11_STD", "GJR11_SSTD"))

fcstGJR11

## ----evaluation metrics for GJR-GARCH(1,1) models-----------------------------
evalGJR <- NULL

for (i in 1:3) {
  mse <- mean((proxy$Volatility^2 - fcstGJR11[,i]^2)^2)
  
  rmse <- sqrt(mse)
  
  mae <- mean(abs(proxy$Volatility^2 - fcstGJR11[,i]^2))
  
  evalGJR <- rbind(evalGJR, c(MSE = mse, RMSE = rmse, MAE= mae))
}

rownames(evalGJR) <- colnames(fcstGJR11)

evalGJR

## ----forecast EGARCH(1,1) models----------------------------------------------
# Forecast EGARCH(1, 1) with normal distribution of innovations 8 periods ahead
fcstEGARCH11_nd <- rugarch::ugarchforecast(fitORspec = fitEGARCH11, n.ahead = 8)

# Forecast EGARCH(1, 1) with t-distribution of innovations 8 periods ahead
fcstEGARCH11_std <- rugarch::ugarchforecast(fitORspec = fitEGARCH11_std, n.ahead = 8)

# Forecast EGARCH(1, 1) with t-distribution of innovations 8 periods ahead
fcstEGARCH11_sstd <- rugarch::ugarchforecast(fitORspec = fitEGARCH11_sstd, n.ahead = 8)

## ----store forecast of EGARCH-------------------------------------------------
fcstEGARCH11 <- cbind(sigma(fcstEGARCH11_nd), sigma(fcstEGARCH11_std), sigma(fcstEGARCH11_sstd)) %>%
  `colnames<-`(c("EGARCH11_ND", "EGARCH11_STD", "EGARCH11_SSTD"))

fcstEGARCH11

## ----evaluation metrics for EGARCH(1,1) models--------------------------------
evalEGARCH <- NULL

for (i in 1:3) {
  mse <- mean((proxy$Volatility^2 - fcstEGARCH11[,i]^2)^2)
  
  rmse <- sqrt(mse)
  
  mae <- mean(abs(proxy$Volatility^2 - fcstEGARCH11[,i]^2))
  
  evalEGARCH <- rbind(evalEGARCH, c(MSE = mse, RMSE = rmse, MAE= mae))
}

rownames(evalEGARCH) <- colnames(fcstEGARCH11)

evalEGARCH

## ----summary of evaluation metrics--------------------------------------------
rbind(evalARCH, evalGARCH, evalGJR, evalEGARCH)

