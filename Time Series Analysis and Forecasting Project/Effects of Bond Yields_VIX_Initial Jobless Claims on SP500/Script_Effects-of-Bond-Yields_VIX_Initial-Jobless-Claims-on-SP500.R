## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages, message=FALSE, warning=FALSE------------------------------
library(ARDL) # For ARDL models lag selection
library(corrplot) # For visualizing correlation between variables
library(dLagM) # For ARDL models and forecasting
library(dplyr) # For data manipulation
library(forecast) # For ARIMA model and evaluation metrics
library(lubridate) # For manipulating time series objects
library(quantmod) # For obtaining historical data from Yahoo Finance and FRED
library(urca) # For unit root tests
library(stargazer) # For tidy regression output and tables, where possible

## ----retrieve and store SPY price data----------------------------------------
# Retrieve SPY historical data from Yahoo Finance using quantmod package

# Set start and end date for data retrieval
startdate <- as.Date("2000-01-03") # First trading day of year 2000
enddate <- as.Date("2022-07-01") 

spy_price <- getSymbols(Symbols = "SPY", 
                        src = "yahoo", 
                        auto.assign = F, 
                        from = startdate, to = enddate, 
                        periodicity = "weekly")

# Show first and last 6 observations
head(spy_price); tail(spy_price)

# Number of rows of data
nrow(spy_price)

# Check for missing data
colSums(is.na(spy_price))

## ----plot price data of SPY---------------------------------------------------
spy_close <- spy_price[,"SPY.Close"] %>% 
  `colnames<-`("SPY")

plot(spy_close, main = "SPY Weekly Closing Price")

## ----check for stationarity of SPY price--------------------------------------
# Add deterministic trend to tests since the chart seems to show it

spy_close %>% urca::ur.df(type = "trend", selectlags = "AIC") %>% summary()

spy_close %>% urca::ur.kpss(type = "tau", lags = "long") %>% summary()

## ----calculate discrete returns and check stationarity------------------------
# Calculate discrete returns, na.omit to remove first observation as it will return NA
returns_spy <- na.omit(diff(x = spy_close, lag = 1, differences = 1) / stats::lag(x = spy_close, k = 1))

nrow(returns_spy)

plot(returns_spy, main = "SPY Weekly Returns")
  
returns_spy %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

returns_spy %>% urca::ur.kpss(type = "mu", lags = "long") %>% summary()

## ----retrieve and store yield spread data-------------------------------------
# Retrieve 10Y/2Y Treasury Yield Spread from St. Louis Fed's FRED using quantmod package

tys <- getSymbols(Symbols = "T10Y2Y", src = "FRED", auto.assign = F, from = startdate, to = enddate, periodicity = "weekly")

head(tys); tail(tys)

## ----adjusting yield spread data----------------------------------------------
# 1. Adjust data collected to start from 2000 and end in 2022

tys <- tys["2000/2022-06",] %>% 
  `colnames<-`("TYS")

# 2. Check for missing data. Replace NAs with prior observation.

sum(is.na(tys))
clean_tys <- na.locf(object = tys)

# 3. Change daily frequency to weekly frequency (since SPY closing price is Friday, we extract Friday data)
# But last observation in closing price is the last trading day of June 2022, so remember to add that in

# Indicate week_start = 1 for week to start on Monday
weekly_tys <- rbind(clean_tys[wday(clean_tys, week_start = 1) == 5], last(clean_tys))

# Third, adjust dates since second step will return dates on Friday

index(weekly_tys) <- index(spy_close)

head(weekly_tys); tail(weekly_tys); nrow(weekly_tys)

## ----plot SPY price and yield spread------------------------------------------
plot(merge(spy_close, weekly_tys), multi.panel = T, yaxis.same = F, main = "SPY Closing Price and 10Y/2Y Treasury Yield Spread")

## ----stationarity of yield spread---------------------------------------------
weekly_tys %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

weekly_tys %>% urca::ur.kpss(type = "mu", lags = "long") %>% summary()

## ----calculate first diff of yield spread and test for stationarity-----------
# Remove first observation since it will return NA after taking first difference
diff_tys <- na.omit(diff(x = weekly_tys, lag = 1, differences = 1))

nrow(diff_tys)

plot(diff_tys, main = "First-Difference of 10Y/2Y Treasury Yield Spread")
  
diff_tys %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

diff_tys %>% urca::ur.kpss(type = "mu", lags = "long") %>% summary()

## ----retrieve and store VIX data----------------------------------------------
# Retrieve VIX from FRED, adjustment of data is similar to the yield spread

vix <- getSymbols(Symbols = "VIXCLS", src = "FRED", auto.assign = F)

vix <- vix["2000/2022-06",] %>%
  `colnames<-`("VIX")

# Check for missing data and replace with prior observation
sum(is.na(vix))
clean_vix <- na.locf(object = vix)

weekly_vix <- rbind(clean_vix[wday(clean_vix, week_start = 1) == 5], last(clean_vix))

index(weekly_vix) <- index(spy_close)

head(weekly_vix); tail(weekly_vix); nrow(weekly_vix)

## ----plot SPY price and VIX---------------------------------------------------
plot(merge(spy_close, weekly_vix), multi.panel = T, yaxis.same = F, main = "SPY Closing Price and VIX")

## ----stationarity of VIX------------------------------------------------------
weekly_vix %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

weekly_vix %>% urca::ur.kpss(type = "mu", lags = "long") %>% summary()

## ----retrieve and store initial jobless claims data---------------------------
# Retrieve initial jobless claims from FRED, adjustment of data is similar to the yield spread

ijc <- getSymbols(Symbols = "ICSA", src = "FRED", auto.assign = F)

ijc <- ijc["2000/2022-06",] %>%
  `colnames<-`("IJC")

# Check for missing data
sum(is.na(ijc))

nrow(ijc) 

# Initial Jobless Claims is weekly data, but adjust the dates so that it can be plotted
# and merged into same object with other data later.
index(ijc) <- index(spy_close)

head(ijc); tail(ijc)

## ----plot SPY price and IJC---------------------------------------------------
plot(merge(spy_close, ijc), multi.panel = T, yaxis.same = F, main = "SPY Closing Price and Initial Jobless Claims")

## ----SPY and VIX before 2020--------------------------------------------------
plot(merge(spy_close, ijc)["/2019",], multi.panel = T, yaxis.same = F, main = "SPY and Initial Jobless Claims Before 2020")

## ----stationarity of initial jobless claims-----------------------------------
ijc["/2019",] %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

ijc["/2019",] %>% urca::ur.kpss(type = "mu", lags = "long") %>% summary()

## ----calculate growth of ijc and check for stationarity-----------------------
# Take difference of data at time t and t-1 and divide by data at time t-1
# Gives us the growth in initial jobless claims (in decimal format)
# Remove first observation since it will return NA 
ijc_growth <- na.omit(diff(x = ijc, lag = 1, differences = 1) / stats::lag(x = ijc, k = 1))

head(ijc_growth)

nrow(ijc_growth)

ijc_growth["/2019",] %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

ijc_growth["/2019",] %>% urca::ur.kpss(type = "mu", lags = "long") %>% summary()

## ----correlation heatmaps-----------------------------------------------------
corr_level <- cor(x = merge(spy_close, weekly_tys, weekly_vix, ijc), method = "spearman")

corrplot(corr = corr_level, method = "color", type = "lower", title = "Correlation of variables at level", addCoef.col = "black", mar = c(1,1,2,1))

# Remove first row of weekly_vix because it did not require differencing, so it had an additional first observation
corr_stationary <- cor(x = merge(returns_spy, diff_tys, weekly_vix[-1,], ijc_growth), method = "spearman")

corrplot(corr = corr_stationary, method = "color", type = "lower", title = "Correlation of stationary variables", addCoef.col = "black", mar = c(1,1,2,1))

## ----ACF and PACF plot--------------------------------------------------------
par(mfrow = c(2,1), mar = c(2,3,4,2))

forecast::Acf(x = returns_spy["/2022-05",], main = "ACF of SPY Returns")

forecast::Pacf(x = returns_spy["/2022-05",], main = "PACF of SPY Returns")

## ----ARMA(p,q) model----------------------------------------------------------
arma <- forecast::auto.arima(y = returns_spy["/2022-05",], 
                             max.p = 4, max.q = 4, 
                             ic = "aic", 
                             stepwise = F, approximation = F, 
                             trace = F)

arma

## ----plot of actual and arma fitted values------------------------------------
plot.zoo(returns_spy["/2022-05",], 
         col = "black", type = "l", lwd = 2, 
         ylab = "SPY Returns", xlab = "Time", 
         main = "Fitted SPY Returns Using ARMA")

lines(zoo(x = fitted(arma), order.by = index(returns_spy["/2022-05",])), col = "red", type = "l", lwd = 1.5)

legend(x = "bottomleft", legend = c("Actual Returns", "Fitted Values"), col = c("black", "red"), lwd = 2)

## ----SPY returns and treasury yield spread------------------------------------
# max_order = 4 to indicate maximum lag in the search for all variables is 4
# possible to state a vector of length equal to no. of variables in max_order
# grid = T to to prevent stepwise search of models

spytys <- ARDL::auto_ardl(formula = SPY ~ TYS, 
                          data = as.zoo(merge(returns_spy, diff_tys)["/2022-05",]), 
                          max_order = 4, 
                          selection = "AIC", grid = T)

summary(spytys$best_model)

## ----plot of actual and SPYTYS fitted values----------------------------------
plot.zoo(returns_spy["/2022-05",], 
         col = "black", type = "l", lwd = 2, 
         ylab = "SPY Returns", xlab = "Time", 
         main = "Regressing SPY Returns on Weekly Change in TYS")

# NA for first value as the lag used reduces observations by 1
lines(fitted(spytys$best_model), col = "red", type = "l", lwd = 1.5)

legend(x = "bottomleft", legend = c("Actual Returns", "Fitted Values"), col = c("black", "red"), lwd = 2)

## ----SPY returns and VIX------------------------------------------------------
spyvix <- ARDL::auto_ardl(formula = SPY ~ VIX, 
                          data = as.zoo(merge(returns_spy, weekly_vix[-1,])["/2022-05",]), 
                          max_order = 4, 
                          selection = "AIC", grid = T)

summary(spyvix$best_model)

## ----plot of actual and SPYVIX fitted values----------------------------------
plot.zoo(returns_spy["/2022-05",], 
         col = "black", type = "l", lwd = 2, 
         ylab = "SPY Returns", xlab = "Time", 
         main = "Regressing SPY Returns on VIX")

# NA for first 4 values as the lag used reduces observations by 4
lines(fitted(spyvix$best_model), col = "red", type = "l", lwd = 1.5)

legend(x = "bottomleft", legend = c("Actual Returns", "Fitted Values"), col = c("black", "red"), lwd = 2)

## ----SPY returns and IJC------------------------------------------------------
spyijc <- ARDL::auto_ardl(formula = SPY ~ IJC, 
                          data = as.zoo(merge(returns_spy, ijc_growth)["/2022-05",]), 
                          max_order = 4, 
                          selection = "AIC", grid = T)

summary(spyijc$best_model)

## ----plot of actual and SPYIJC fitted values----------------------------------
plot.zoo(returns_spy["/2022-05",], 
         col = "black", type = "l", lwd = 2, 
         ylab = "SPY Returns", xlab = "Time", 
         main = "Regressing SPY Returns on Weekly Percentage Change in IJC")

# NA for first 3 values as the lag used reduces observations by 3
lines(fitted(spyijc$best_model), col = "red", type = "l", lwd = 1.5)

legend(x = "bottomleft", legend = c("Actual Returns", "Fitted Values"), col = c("black", "red"), lwd = 2)

## ----ARDL with multiple independent variables---------------------------------
spyall <- ARDL::auto_ardl(formula = SPY ~ TYS + VIX + IJC, 
                          data = as.zoo(merge(returns_spy, diff_tys, weekly_vix[-1,], ijc_growth)["/2022-05",]), 
                          max_order = 4, 
                          selection = "AIC", grid = T)

summary(spyall$best_model)

## ----plot of actual and SPY_ALL fitted values---------------------------------
plot.zoo(returns_spy["/2022-05",], 
         col = "black", type = "l", lwd = 2, 
         ylab = "SPY Returns", xlab = "Time", 
         main = "Regressing SPY Returns on All Independent Variables")

# NA for first 4 values as the lag used reduces observations by 4
lines(fitted(spyall$best_model), col = "red", type = "l", lwd = 1.5)

legend(x = "bottomleft", legend = c("Actual Returns", "Fitted Values"), col = c("black", "red"), lwd = 2)

## ----forecast ARMA model------------------------------------------------------
f_arma <- forecast::forecast(object = arma, h = 4, level = 95)

stargazer(as.data.frame(f_arma), 
          type = "text", 
          title = "Forecasted SPY Returns for June 2022 Using ARMA Model",
          summary = F)

stargazer(forecast::accuracy(f_arma, x = returns_spy["2022-06"]), 
          type = "text", 
          title = "Evaluation of ARMA Model Forecast")

## ----plot forecast of ARMA model----------------------------------------------
# Add in-sample periods Jan to May 2022, and out-of-sample period Jun 2022 (26 observations)
# Bind actual with forecasted values 

f_arma.level <- zoo(x = cbind(fspy = as.vector(last(spy_close["2022-05",])) * cumprod(1 + f_arma$mean),
                              fspy_lower = as.vector(last(spy_close["2022-05",])) * cumprod(1 + f_arma$lower),
                              fspy_upper = as.vector(last(spy_close["2022-05",])) * cumprod(1 + f_arma$upper)),
                    order.by = index(spy_close["2022-06",]))

plot.zoo(cbind(f_arma.level, spy_close["2022-06",], spy_close["2022-01/2022-05",]), 
         plot.type = "single", 
         col = c("red", "gray", "gray", "blue", "black"), lwd = 2, 
         ylab = "SPY Weekly Close Price", main = "Actual and Forecasted SPY Price Using ARMA Model")

legend(x = "bottomleft", 
       legend = c("Forecasted Price", "Lower and Upper Bound", "Actual Price June 2022", "Actual Price in Train Period"), 
       col = c("red", "gray", "blue", "black"), lwd = 1.5)

## ----ARDL using dLagM---------------------------------------------------------
# Using dLagM package to create same ARDL model as per Section 4.3
spyall.new <- dLagM::ardlDlm(formula = SPY ~ TYS + VIX + IJC, 
                             data = data.frame(merge(returns_spy, diff_tys, weekly_vix[-1,], ijc_growth)["/2022-05",]), 
                             p = 4, q = 4, 
                             remove = list(p = list(TYS = c(1:4)))) # Remove all lags of TYS

stargazer(spyall.new$model, type = "text",
          title = "ARDL Regression Using dLagM Package", 
          dep.var.labels.include = F, 
          column.labels = "SPY.t")

## ----forecast ARDL model------------------------------------------------------
# Create matrix containing values of exogenous variables during forecast period
# Number of columns = forecast period, Number of rows = exogenous variables
x.new = rbind(as.vector(diff_tys["2022-06",]),
              as.vector(weekly_vix["2022-06",]),
              as.vector(ijc_growth["2022-06",]))

f_ardl <- dLagM::forecast(model = spyall.new, x = x.new, h = 4, interval = T, level = 0.95)

stargazer(f_ardl$forecasts, 
          type = "text", 
          title = "Forecasted SPY Returns for June 2022 Using ARDL Model",
          summary = F)

## ----create forecast class structure------------------------------------------
f_ardl.new <- structure(list(level = 95,
                             mean = f_ardl$forecasts$Forecast,
                             lower = f_ardl$forecasts$`95% LB`,
                             upper = f_ardl$forecasts$`95% UB`,
                             x = returns_spy["/2022-05",],
                             fitted = c(rep(NA, 4), spyall.new$model$fitted.values),
                             residuals = spyall.new$model$residuals),
                        class = "forecast")

stargazer(forecast::accuracy(object = f_ardl.new, x = returns_spy["2022-06"]), 
          type = "text", 
          title = "Evaluation of ARDL Model Forecast")

## ----plot forecast of ARDL model----------------------------------------------
f_ardl.level <- zoo(x = cbind(fspy = as.vector(last(spy_close["2022-05",])) * cumprod(1 + f_ardl.new$mean),
                              fspy_lower = as.vector(last(spy_close["2022-05",])) * cumprod(1 + f_ardl.new$lower),
                              fspy_upper = as.vector(last(spy_close["2022-05",])) * cumprod(1 + f_ardl.new$upper)),
                    order.by = index(spy_close["2022-06",]))

plot.zoo(cbind(f_ardl.level, spy_close["2022-06",], spy_close["2022-01/2022-05",]), 
         plot.type = "single", 
         col = c("red", "gray", "gray", "blue", "black"), lwd = 2, 
         ylab = "SPY Weekly Close Price", main = "Actual and Forecasted SPY Price Using ARDL Model")

legend(x = "bottomleft", 
       legend = c("Forecasted Price", "Lower and Upper Bound", "Actual Price June 2022", "Actual Price in Train Period"), 
       col = c("red", "gray", "blue", "black"), lwd = 1.5)

