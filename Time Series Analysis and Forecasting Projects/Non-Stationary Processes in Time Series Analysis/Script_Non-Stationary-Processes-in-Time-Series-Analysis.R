## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.align="center", message = FALSE, warning = FALSE)
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages------------------------------------------------------------
library(forecast) # For ARIMA modelling, forecasting and evaluation
library(urca) # For unit root and stationarity tests

## ----simulate RW--------------------------------------------------------------
# Number of observations
N = 500

# Model to simulate
# For RW model, only need d = 1 in (p,d,q) to include unit root
M = list(order = c(0, 1, 0))

set.seed(123)

RW <- stats::arima.sim(model = M, n = N)
                       
# Plot RW
plot(x = RW, main = "Random Walk", ylab = "y", lwd = 2)

## ----ADF and KPSS tests for RW------------------------------------------------
RW %>% urca::ur.df(type = "none", selectlags = "AIC") %>% summary()

# KPSS requires a deterministic component, either drift (mu) or trend (tau)
RW %>% urca::ur.kpss(type = "mu", lags = "short") %>% summary()

## ----simulate RW with drift---------------------------------------------------
# Use same number of observations (N) and model (M) as RW model

# Include a non-zero mean value and optionally the standard deviation
# RW with positive constant
set.seed(234)
RW_posDrift <- stats::arima.sim(model = M, n = N, mean = 0.5, sd = 10)

# RW with negative constant
set.seed(234)
RW_negDrift <- stats::arima.sim(model = M, n = N, mean = -0.5, sd = 10)

# Plot RW with drift
plot(x = cbind(RW_posDrift, RW_negDrift), 
     main = "Random Walk with Drift", ylab = "y", 
     plot.type = "single", 
     col = c("black", "red"), lwd = 2)

legend(x = "topleft", legend = c("RW with positive drift", "RW with negative drift"), 
       col = c("black", "red"), lwd = 2)

## ----ADF and KPSS tests for RW with drift-------------------------------------
RW_posDrift %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

RW_posDrift %>% urca::ur.kpss(type = "mu", lags = "short") %>% summary()

## ----simulate simple model with deterministic trend---------------------------
# Use same number of observations as RW model, observation begans from time 0
tt <- 0:N

set.seed(345)

# Set sd = 20 so that the ups and downs can be seen clearly
DT <- as.ts(tt + rnorm(n = N, mean = 0, sd = 10))

plot(x = DT, main = "Deterministic Trend", ylab = "y", lwd = 2)

## ----ADF and KPSS tests for model with DT-------------------------------------
DT %>% urca::ur.df(type = "trend", selectlags = "AIC") %>% summary()

DT %>% urca::ur.kpss(type = "tau", lags = "short") %>% summary()

## ----simulate RW with drift and DT--------------------------------------------
# Create simulated model by adding time trend (tt) to RW_posDrift
RW_posDrift_DT <- tt + RW_posDrift

plot(x = RW_posDrift_DT, main = "Random Walk with Drift and Deterministic Trend", ylab = "y", lwd = 2)

## ----ADF and KPSS tests for RW with drift and DT------------------------------
RW_posDrift_DT %>% urca::ur.df(type = "trend", selectlags = "AIC") %>% summary()

RW_posDrift_DT %>% urca::ur.kpss(type = "tau", lags = "short") %>% summary()

## ----plot models in same chart------------------------------------------------
plot(cbind(RW, RW_posDrift, DT, RW_posDrift_DT),
     main = "Non-Stationary Processes", ylab = "y", 
     plot.type = "single", 
     col = c("black", "red", "blue", "green"), lwd = 2)

legend(x = "topleft",
       legend = c("RW", "RW with positive drift", "Deterministic trend", "RW with positive drift and deterministic trend"), 
       col = c("black", "red", "blue", "green"), lwd = 2)

## ----differencing RW and RW with drift----------------------------------------
RW_diff <- diff(x = RW, lag = 1, differences = 1)

RWdrift_diff <- diff(x = RW_posDrift, lag = 1, differences = 1)

plot(x = cbind(RW_diff, RWdrift_diff), 
     main = "First-Differenced Random Walk with and without Drift", 
     ylab = "Differenced y", plot.type = "single",
     col = c("black", "red"), lwd = 2)

legend(x = "topleft", 
       legend = c("Differenced RW", "Differenced RW with drift"), 
       col = c("black", "red"), lwd = 2)

## ----ADF and KPSS test of differenced RW--------------------------------------
RW_diff %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

RW_diff %>% urca::ur.kpss(type = "mu", lags = "short") %>% summary()

## ----removing time trend from model-------------------------------------------
DT_detrend <- DT - tt

plot(x = cbind(DT, tt, DT_detrend), 
     main = "Before and After Detrending", ylab = "y",
     plot.type = "single",
     col = c("black", "red", "blue"), lwd = 2)

legend(x = "topleft", legend = c("Deterministic trend", "Time Trend", "Detrend"),
       col = c("black", "red", "blue"), lwd = 2)

## ----ADF and KPSS tests on model after de-trending----------------------------
DT_detrend %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

DT_detrend %>% urca::ur.kpss(type = "mu", lags = "short") %>% summary()

## ----difference model with DT once--------------------------------------------
DT_diff <- diff(x = DT, lag = 1, differences = 1)

# Remove first row of DT_detrend as differencing causes us to lose one observation
plot(x = cbind(DT_detrend[-1], DT_diff), 
     main = "De-trending vs Differencing of Deterministic Trend Process", ylab = "",
     plot.type = "single",
     col = c("black", "red"), lwd = c(2, 1), lty = c(1, 2))

legend(x = "topleft",
       legend = c("De-trended", "Differenced"),
       col = c("black", "red"), lwd = c(2, 1), lty = c(1, 2))

## ----ADF and KPSS tests for DT_diff-------------------------------------------
DT_diff %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

DT_diff %>% urca::ur.kpss(type = "mu", lags = "short") %>% summary()

## ----stationarity of RW with drift and DT-------------------------------------
reg_y <- lm(formula = RW_posDrift_DT ~ tt)

delta_y <- diff(x = residuals(reg_y), lag = 1, differences = 1)

# Remove first observation from residuals as differencing causes one observation to be lost
plot(cbind(as.ts(residuals(reg_y)[-1]), delta_y),
     main = "De-trending Only and With Differencing", ylab = "",
     plot.type = "single",
     col = c("black", "red"), lwd = 2)

legend(x = "top",
       legend = c("After De-trending", "After De-trending and Differencing"),
       col = c("black", "red"), lwd = 2)

## ----ADF and KPSS tests for delta_y-------------------------------------------
delta_y %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

delta_y %>% urca::ur.kpss(type = "mu", lags = "short") %>% summary()

## ----simulate ARIMA with quadratic time trend---------------------------------
# Number of observations
N = 300

# Model to simulate, need to include coefficients of AR and MA terms
M = list(order = c(2, 1, 1), ar = c(1.02, -0.43), ma = -0.41)

set.seed(456)

stoch_proc <- stats::arima.sim(model = M, n = N, mean = 0.23, sd = 3)

t <- as.ts(0:N)

y <- stoch_proc + 1.39 * t - 0.0027 * t^2

plot(y, main = "Simulated ARIMA(2,1,1) with Quadratic Time Trend", lwd = 2)

## ----ADF and KPSS tests for y-------------------------------------------------
y %>% urca::ur.df(type = "trend", selectlags = "AIC") %>% summary()

y %>% urca::ur.kpss(type = "tau", lags = "short") %>% summary()

## ----split training and testing set-------------------------------------------
train_y <- as.ts(y[0:281])
train_t <- as.ts(t[0:281])

test_y <- ts(data = y[282:301], start = 282)
test_t <- ts(data = t[282:301], start = 282)

## ----plot trends--------------------------------------------------------------
linear_trend <- lm(train_y ~ train_t)

quad_trend <- lm(train_y ~ train_t + I(train_t^2))

plot(cbind(train_y, fitted(linear_trend), fitted(quad_trend)),
     main = "Actual Training Data and Fitted Trend", ylab = "y",
     plot.type = "single",
     col = c("black", "red", "blue"), lwd = 2)

legend(x = "topleft",
       legend = c("Actual", "Linear trend", "Quadratic trend"),
       col = c("black", "red", "blue"), lwd = 2)

## ----residual of quad_trend---------------------------------------------------
plot(as.ts(cbind(residuals(quad_trend), residuals(linear_trend))),
     main = "Residuals from Linear Trend and Quadratic Trend", ylab = "",
     plot.type = "single",
     col = c("black", "red"), lwd = 2)

legend(x = "topleft",
       legend = c("Residuals quad_trend", "Residuals linear_trend"),
       col = c("black", "red"), lwd = 2)

## ----ADF and KPSS tests for residuals-----------------------------------------
residuals(quad_trend) %>% urca::ur.df(type = "drift", selectlags = "AIC") %>% summary()

residuals(quad_trend) %>% urca::ur.kpss(type = "mu", lags = "short") %>% summary()

## ----estimate ARIMA with only differencing------------------------------------
# Restrict AR and MA terms to maximum of 4 lags
model1 <- forecast::auto.arima(y = train_y, ic = "aic",
                               seasonal = FALSE, stepwise = FALSE, approximation = FALSE)

model1

## ----estimate ARIMA with xreg quad trend--------------------------------------
model2 <- forecast::auto.arima(y = train_y, ic = "aic",
                               xreg = cbind(train_t, train_t^2),
                               seasonal = FALSE, stepwise = FALSE, approximation = FALSE)

model2

## ----estimate model2 with differencing included-------------------------------
model3 <- forecast::auto.arima(y = train_y, ic = "aic", d = 1,
                               xreg = cbind(train_t, train_t^2),
                               seasonal = FALSE, stepwise = FALSE, approximation = FALSE)

model3

## ----forecast of models-------------------------------------------------------
f_model1 <- forecast::forecast(object = model1, h = 20, level = 95)

f_model2 <- forecast::forecast(object = model2, h = 20, level = 95, xreg = cbind(test_t, test_t^2))

f_model3 <- forecast::forecast(object = model3, h = 20, level = 95, xreg = cbind(test_t, test_t^2))

## ----plot f_model1 against actual value and accuracy--------------------------
plot(cbind(y, f_model1$mean, f_model1$lower, f_model1$upper),
     main = "Actual and Forecasted Values of model1", ylab = "y",
     plot.type = "single",
     col = c("black", "red", "gray", "gray"), lwd = 2)

legend(x = "topleft",
       legend = c("Actual", "Forecasted", "Lower and Upper Interval"),
       col = c("black", "red", "gray"), lwd = 2)

data.frame(forecast::accuracy(object = f_model1, x = test_y))

## ----plot f_model2 against actual value and accuracy--------------------------
plot(cbind(y, f_model2$mean, f_model2$lower, f_model2$upper),
     main = "Actual and Forecasted Values of model2", ylab = "y",
     plot.type = "single",
     col = c("black", "red", "gray", "gray"), lwd = 2)

legend(x = "topleft",
       legend = c("Actual", "Forecasted", "Lower and Upper Interval"),
       col = c("black", "red", "gray"), lwd = 2)

data.frame(forecast::accuracy(object = f_model2, x = test_y))

## ----plot f_model3 against actual value and accuracy--------------------------
plot(cbind(y, f_model3$mean, f_model3$lower, f_model3$upper),
     main = "Actual and Forecasted Values of model3", ylab = "y",
     plot.type = "single",
     col = c("black", "red", "gray", "gray"), lwd = 2)

legend(x = "topleft",
       legend = c("Actual", "Forecasted", "Lower and Upper Interval"),
       col = c("black", "red", "gray"), lwd = 2)

data.frame(forecast::accuracy(object = f_model3, x = test_y))

