## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages, message=FALSE, warning=FALSE, results="hide", class.source = "fold-show"----
# Use install.packages("packagename") if they are not already installed

# Packages that will be needed for obtaining data via an API
library(httr)
library(jsonlite)

# Packages that produces neat regression tables (only when possible)
library(broom)
library(knitr)
library(stargazer)

# Packages that will be needed for the main parts of the project
library(ARDL) # For ARDL models lag selection
library(bruceR) # For multivariate granger causality test
library(corrplot) # For visualizing correlation between variables
library(dLagM) # For creating ARDL objects that can work with a forecast function
library(forecast) # For ARIMA models, forecasting and evaluation
library(lmtest) # For tests such as Breusch-Godfrey, Breusch-Pagan
library(lubridate) # For working with dates
library(tidyverse) # For ggplot2 and dplyr
library(urca) # For unit root tests
library(xts) # For converting data to and working with xts objects
library(vars) # For VAR models

## ----cpi url------------------------------------------------------------------
cpi_url <- "https://tablebuilder.singstat.gov.sg/api/table/tabledata/M212881?isTestApi=true&seriesNoORrowNo=1"

raw_cpi <- httr::GET(url = cpi_url)

## ----explore rawdata----------------------------------------------------------
raw_cpi

names(raw_cpi)

head(raw_cpi$content)

## ----convert to text----------------------------------------------------------
cpi <- jsonlite::fromJSON(rawToChar(raw_cpi$content))

names(cpi)

lapply(cpi, FUN = class)

## ----data element-------------------------------------------------------------
names(cpi$Data)

names(cpi$Data$row)

head(lapply(cpi$Data$row$columns, dim))

## ----convert to df------------------------------------------------------------
cpi_data <- as.data.frame(cpi$Data$row$columns[[1]])

## ----cpidata------------------------------------------------------------------
stargazer(rbind(head(cpi_data, n = 8), tail(cpi_data, n = 8)),
          type = "text",
          title = "First and Last 8 Values in cpi data",
          summary = F)

## ----reordering date----------------------------------------------------------
cpi_data <- cpi_data %>% 
  dplyr::arrange(lubridate::ym(cpi_data$key))

stargazer(rbind(head(cpi_data, n = 8), tail(cpi_data, n = 8)),
          type = "text",
          title = "Adjusted First and Last 8 Values in cpi data",
          summary = F)

## ----dspi---------------------------------------------------------------------
dspi_url <- "https://tablebuilder.singstat.gov.sg/api/table/tabledata/M212701?isTestApi=true&seriesNoORrowNo=1"

raw_dspi <- httr::GET(url = dspi_url)

dspi <- jsonlite::fromJSON(rawToChar(raw_dspi$content))

dspi_data <- as.data.frame(dspi$Data$row$columns[[1]])

dspi_data <- dspi_data %>%
  dplyr::arrange(lubridate::ym(dspi_data$key))

stargazer(rbind(head(dspi_data, n = 8), tail(dspi_data, n = 8)),
          type = "text",
          title = "Adjusted First and Last 8 Values in dspi data",
          summary = F)

## ----cli----------------------------------------------------------------------
cli_url <- "https://tablebuilder.singstat.gov.sg/api/table/tabledata/M240421?isTestApi=true"

raw_cli <- httr::GET(url = cli_url)

cli <- jsonlite::fromJSON(rawToChar(raw_cli$content))

cli_data <- as.data.frame(cli$Data$row$columns[[1]])

# Data is ordered by the years and quarters so there was no need for reordering

stargazer(rbind(head(cli_data, n = 8), tail(cli_data, n = 8)),
          type = "text",
          title = "First and Last 8 Values in cli data",
          summary = F)

## ----convert df to xts--------------------------------------------------------
cpi.xts <- xts(x = cpi_data$value, 
               order.by = as.yearmon(cpi_data$key, format = "%Y %b")) %>%
  `colnames<-`("CPI")

dspi.xts <- xts(x = dspi_data$value, 
                order.by = as.yearmon(dspi_data$key, format = "%Y %b")) %>%
  `colnames<-`("DSPI")

cli.xts <- xts(x = cli_data$value, 
               order.by = as.yearqtr(gsub("Q", "", cli_data$key), format = "%Y %q")) %>%
  `colnames<-`("CLI")

## ----convert value to numeric-------------------------------------------------
storage.mode(cpi.xts) <- "numeric"

storage.mode(dspi.xts) <- "numeric"

storage.mode(cli.xts) <- "numeric"

## ----convert cpi to qtrly-----------------------------------------------------
cpi_q.xts <- xts::apply.quarterly(cpi.xts, FUN = mean)

# Changing the date index to an appropriate format
tclass(cpi_q.xts) <- "yearqtr"

## ----subset cpi_q-------------------------------------------------------------
#Subset cpi_q to match the cli data

cpi_q.xts <- cpi_q.xts["1978-01/2022-03"]

## ----plot cpi, fig.align='center', echo=FALSE---------------------------------
plot.xts(cpi_q.xts, main = "Singapore Quarterly CPI from 1978Q1 to 2022Q1")

## ----seasonality in cpi, fig.align='center', echo=FALSE-----------------------
boxplot(cpi_q.xts ~ cycle(cpi_q.xts),
        ylab = "Quarterly CPI",
        xlab = "Quarters")

## ----convert and subset dspi qtrly--------------------------------------------
dspi_q.xts <- xts::apply.quarterly(dspi.xts, FUN = mean)

tclass(dspi_q.xts) <- "yearqtr"

dspi_q.xts <- dspi_q.xts["1978-01/2022-03"]

## ----plot dspi, fig.align='center', echo=FALSE--------------------------------
plot.xts(dspi_q.xts, main = "Singapore Quarterly DSPI from 1978Q1 to 2022Q1")

## ----seasonality in dspi, fig.align='center', echo=FALSE----------------------
boxplot(dspi_q.xts ~ cycle(dspi_q.xts),
        ylab = "Quarterly DSPI",
        xlab = "Quarters")

## ----plot cli, fig.align='center', echo=FALSE---------------------------------
plot.xts(cli.xts, main = "Singapore Quarterly CLI from 1978Q1 to 2022Q1")

## ----merge data---------------------------------------------------------------
data <- merge(cpi_q.xts, dspi_q.xts, cli.xts)

stargazer(head(data, n = 8),
          type = "text",
          title = "Unadjusted data",
          rownames = T)

## ----clean data---------------------------------------------------------------
data[, 3] <- na.locf(data[, 3])

data <- na.omit(data)

stargazer(head(data, n = 8),
          type = "text",
          title = "Adjusted data",
          rownames = T)

dim(data)

## ----scatplot data, fig.align='center', echo=FALSE----------------------------
pairs( ~ CPI + DSPI + CLI,
       data = data,
       lower.panel = NULL,
       main = "Scatterplot Matrix of CPI, DSPI and CLI")

## ----corrplot data, fig.align='center', echo=FALSE----------------------------
correl <- cor(x = data)

corrplot(correl,
         method = "color", 
         type = "lower", 
         addCoef.col = "black", 
         title = "Correlation Matrix of CPI, DSPI and CLI",
         mar = c(0, 0, 1, 0))

## ----split data---------------------------------------------------------------
train <- data["/2019"]

test <- data["2020/"]

## ----adf cpi level------------------------------------------------------------
# Indicate type = "trend" as there was trend in the series based on plot
# Indicate selectlags = "AIC" to select lags based on Akaike IC

train$CPI %>%
  ur.df(type = "trend", selectlags = "AIC") %>%
  summary()

## ----kpss cpi level-----------------------------------------------------------
# Indicate type = "tau" for trend

train$CPI %>%
  ur.kpss(type = "tau" , lags = "short") %>%
  summary()

## ----adf dspi level-----------------------------------------------------------
# Indicate type = "drift" as DSPI as there is no particular long term trend

train$DSPI %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()

# Indicate type = "mu" for drift

train$DSPI %>%
  ur.kpss(type = "mu", lags = "short") %>%
  summary()

## ----adf cli level------------------------------------------------------------
# Indicate type = "trend"  since there was a clear trend in CLI plot

train$CLI %>%
  ur.df(type = "trend", selectlags = "AIC") %>%
  summary()

train$CLI %>%
  ur.kpss(type = "tau", lags = "short") %>%
  summary()

## ----diff data----------------------------------------------------------------
# By default, the diff function uses lag = 1 and differences = 1

diff.data <- diff(data) %>%
  `colnames<-`(c("dCPI", "dDSPI", "dCLI"))

head(diff.data) # First observation removed because we cannot take difference on it. 

diff.data <- na.omit(diff.data)

# Split the train and test sets again

diff.train <- diff.data["/2019"]

diff.test <- diff.data["2020/"]

## ----plot diff data, fig.align='center', echo=FALSE---------------------------
par(mfrow = c(2, 2))
for (i in 1:3) {
  print(plot.xts(diff.train[,i], main = names(diff.train[,i])))
}

## ----adf dcpi-----------------------------------------------------------------
# After differencing, there should be no deterministic trend in the plots, but it may contain drift terms (especially dealing with non-zero means)
# KPSS test null hypothesis is that variables are stationary with deterministic parts, so just indicate type = "mu"

diff.train$dCPI %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()

diff.train$dCPI %>%
  ur.kpss(type = "mu", lags = "short") %>%
  summary()

## ----adf ddspi----------------------------------------------------------------
diff.train$dDSPI %>%
  ur.df(type = "none", selectlags = "AIC") %>%
  summary()

diff.train$dDSPI %>%
  ur.kpss(type = "mu", lags = "short") %>%
  summary()

## ----adf dcli-----------------------------------------------------------------
diff.train$dCLI %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()

diff.train$dCLI %>%
  ur.kpss(type = "mu", lags = "short") %>%
  summary()

## ----scatplot diff train, fig.align='center', echo=FALSE----------------------
pairs(~ dCPI + dDSPI + dCLI, 
      data = diff.train,
      lower.panel = NULL,
      main = "Scatterplot Matrix of First Differenced Variables")

## ----corrplot diff train, fig.align='center', echo=FALSE----------------------
correldiff <- cor(diff.train)

corrplot(correldiff,
         method = "color",
         type = "lower",
         addCoef.col = "black",
         title = "Correlation Matrix of First Differenced Variables",
         mar = c(0, 0, 1, 0))

## ----acf pacf, fig.align='center'---------------------------------------------
par(mfrow = c(2,1), mar = c(3, 3, 4, 2))

forecast::Acf(x = diff.train$dCPI, main = "ACF of dCPI")

forecast::Pacf(x = diff.train$dCPI, main = "PACF of dCPI")

## ----arima114-----------------------------------------------------------------
# order refers to the (p,d,q) of the ARIMA model
# d = 0 as we are using the differenced data
# method for estimation is to use maximum likelihood

arima1 <- forecast::Arima(diff.train$dCPI, 
                          order = c(1, 0, 4),
                          method = "ML")

summary(arima1)

## ----auto arima---------------------------------------------------------------
arima2 <- forecast::auto.arima(diff.train$dCPI, 
                               seasonal = F, 
                               ic = "aic", 
                               stepwise = F, 
                               approximation = F,
                               trace = F,
                               method = "ML")

summary(arima2)

## ----plot arima, fig.align='center', echo=FALSE-------------------------------
# indicated xlim to zoom into the chart to have a better view of the patterns
plot(ts(diff.train$dCPI, start = c(1978, 2), frequency = 4),
     col = "darkblue", type = "l", lwd = 3,
     ylab = "dCPI", main = "Actual vs Fitted CPI at First Difference Using ARIMA",
     xlim = c(1990, 2019))

lines(ts(fitted(arima1), start = c(1978, 2), frequency = 4), col = "red", type = "l", lwd = 3)

lines(ts(fitted(arima2), start = c(1978, 2), frequency = 4), col = "gray", type = "l", lwd = 3)

abline(h = arima2$coef[4], col = "black", lwd = 2)

legend(x = "topleft", legend = c("Actual dCPI", "arima1", "arima2", "Mean from arima2"),
       col = c("darkblue", "red", "gray", "black"), lwd = 1.5)

## ----ardl1--------------------------------------------------------------------
# max_order = 4 to indicate maximum lag in the search for all variables is 4
# possible to state a vector of length equal to no. of variables in max_order
# grid = T to to prevent stepwise search of models

ardl_lag <- ARDL::auto_ardl(formula = dCPI ~ dDSPI + dCLI,
                            data = as.zoo(diff.train),
                            max_order = 4, grid = T,
                            selection = "AIC")

# Obtain the best lag order, arranged by how variables were entered in the formula argument
ardl_lag$best_order

# Save the coefficients of the model selected by auto_ardl
ardl <- ardl_lag$best_model

summary(ardl)

## ----loglik and aic-----------------------------------------------------------
logLik(ardl)

AIC(ardl)

## ----plot ardl, fig.align='center', echo=FALSE--------------------------------
plot.zoo(diff.train$dCPI,
         col = "darkblue", type = "l", lwd = 3,
         ylab = "dCPI", xlab = "Time",
         main = "Actual vs Fitted CPI at First Difference Using ARDL",
         xlim = c(1990, 2019))

lines(fitted(ardl), col = "red", type = "l", lwd = 3)

legend(x = "bottomleft", legend = c("Actual dCPI", "ardl"),
       col = c("darkblue", "red"), lwd = 1.5)

## ----var lag------------------------------------------------------------------
var_lag <- vars::VARselect(y = diff.train,
                           lag.max = 4,
                           type = "const")

var_lag

## ----var----------------------------------------------------------------------
var <- vars::VAR(y = diff.train,
                 p = 2,
                 type = "const")

stargazer(var$varresult, 
          type = "text", 
          title = "VAR Estimation Result for dCPI, dDSPI, dCLI", 
          digits = 3, 
          column.labels = c("dCPI", "dDSPI", "dCLI"),
          dep.var.labels.include = F)

## ----plot var dcpi, fig.align='center', echo=FALSE----------------------------
plot(ts(diff.train$dCPI, start = c(1978, 2), frequency = 4),
     col = "darkblue", type = "l", lwd = 3,
     ylab = "dCPI", xlab = "Time",
     main = "Actual vs Fitted CPI at First Difference Using VAR")

# Start period is 1978Q4 since we used 2 lags, resulting in loss of first 2 observations
lines(ts(fitted(var$varresult$dCPI), start = c(1978, 4), frequency = 4), col = "red", type = "l", lwd = 3)

legend(x = "bottomleft", legend = c("Actual dCPI", "var"),
       col = c("darkblue", "red"), lwd = 1.5)

## ----plot var ddspi, fig.align='center', echo=FALSE---------------------------
plot(ts(diff.train$dDSPI, start = c(1978, 2), frequency = 4),
     col = "darkblue", type = "l", lwd = 3,
     ylab = "dDSPI", xlab = "Time",
     main = "Actual vs Fitted DSPI at First Difference Using VAR")

lines(ts(fitted(var$varresult$dDSPI), start = c(1978, 4), frequency = 4), col = "red", type = "l", lwd = 3)

legend(x = "bottomleft", legend = c("Actual dDSPI", "var"),
       col = c("darkblue", "red"), lwd = 1.5)

## ----plot var dcli, fig.align='center', echo=FALSE----------------------------
plot(ts(diff.train$dCLI, start = c(1978, 2), frequency = 4),
     col = "darkblue", type = "l", lwd = 3,
     ylab = "dCLI", xlab = "Time",
     main = "Actual vs Fitted CLI at First Difference Using VAR")

lines(ts(fitted(var$varresult$dCLI), start = c(1978, 4), frequency = 4), col = "red", type = "l", lwd = 3)

legend(x = "bottomleft", legend = c("Actual dCLI", "var"),
       col = c("darkblue", "red"), lwd = 1.5)

## ----gc test------------------------------------------------------------------
granger_causality(varmodel = var)

## ----lag for jo test----------------------------------------------------------
# Use the data at levels, not at first difference
lagselect <- vars::VARselect(train, lag.max = 4, type = "const")

lagselect$selection

## ----jo test trace------------------------------------------------------------
# Indicate type = "trace" for trace test, "eigen" for eigenvalue test

# Indicate ecdet = "none" assuming no intercept in the cointegration process, indicate "const" or "trend" to add constant or trend into the equation
# It is common to indicate no deterministic terms in the error correction term unless there are reasons to believe otherwise

# K is the number of lags from the lag selection using VARselect

jotest <- urca::ca.jo(x = train, 
                      type = "trace", 
                      ecdet = "none", 
                      K = 3)

summary(jotest)

## ----ardl_lr------------------------------------------------------------------
ardl_lr_lag <- ARDL::auto_ardl(formula = CPI ~ DSPI + CLI,
                               data = as.zoo(train),
                               max_order = 4,
                               selection = "AIC",
                               grid = T)

ardl_lr <- ardl_lr_lag$best_model

summary(ardl_lr)

## ----uecm---------------------------------------------------------------------
un_ecm <- ARDL::uecm(ardl_lr)

summary(un_ecm)

## ----recm---------------------------------------------------------------------
# Indicated case = 3 to include constant in the model
r_ecm <- ARDL::recm(un_ecm, case = 3)

summary(r_ecm)

## ----lr multiplier------------------------------------------------------------
stargazer(multipliers(un_ecm), # Can also be used on ardl_lr
          type = "text", 
          title = "Long Run Multipliers of var Model",
          summary = F)

## ----plot ardl ecm, fig.align='center', echo=FALSE----------------------------
plot.zoo(diff.train$dCPI, col = "darkblue", type = "l", lwd = 3,
         ylab = "dCPI", xlab = "Time",
         main = "Actual vs Fitted CPI at First Difference Using ARDL-ECM")

lines(fitted(r_ecm), col = "red", type = "l", lwd = 3)

legend(x = "bottomleft", legend = c("Actual dCPI", "ardl-ecm"),
       col = c("darkblue", "red"), lwd = 1.5)

## ----vecm---------------------------------------------------------------------
# K = 3 was determined in Section 9 in the Johansen Test
# Add in spec = "transitory" to obtain the VECM formula

get_vecm <- ca.jo(x = train, 
                  type = "trace", 
                  ecdet = "none", 
                  K = 3,
                  spec = "transitory")


# Get estimated VAR parameters
# r represents number of cointegrations

vec_ecm <- cajorls(get_vecm, r = 1)

kable(tidy(summary(vec_ecm$rlm)[[1]]), format = "pipe",
      caption = "VECM Estimation Output for dCPI",
      digits = 4)

kable(tidy(summary(vec_ecm$rlm)[[2]]), format = "pipe",
      caption = "VECM Estimation Output for dDSPI",
      digits = 4)

kable(tidy(summary(vec_ecm$rlm)[[3]]), format = "pipe",
      caption = "VECM Estimation Output for dCLI",
      digits = 4)

## ----plot vecm cpi, fig.align='center', echo=FALSE----------------------------
plot(ts(diff.train$dCPI, start = c(1978, 2), frequency = 4),
     col = "darkblue", type = "l", lwd = 3,
     ylab = "dCPI", xlab = "Time",
     main = "Actual vs Fitted CPI at First Difference Using VECM")

# Start period is 1978Q4 since we used 2 lags, resulting in loss of first 2 observations
lines(ts(fitted(vec_ecm$rlm)[,"CPI.d"], start = c(1978, 4), frequency = 4), col = "red", type = "l", lwd = 3)

legend(x = "bottomleft", legend = c("Actual dCPI", "vecm"),
       col = c("darkblue", "red"), lwd = 1.5)

## ----plot vecm dspi, fig.align='center', echo=FALSE---------------------------
plot(ts(diff.train$dDSPI, start = c(1978, 2), frequency = 4),
     col = "darkblue", type = "l", lwd = 3,
     ylab = "dDSPI", xlab = "Time",
     main = "Actual vs Fitted DSPI at First Difference Using VECM")

# Start period is 1978Q4 since we used 2 lags, resulting in loss of first 2 observations
lines(ts(fitted(vec_ecm$rlm)[,"DSPI.d"], start = c(1978, 4), frequency = 4), col = "red", type = "l", lwd = 3)

legend(x = "bottomleft", legend = c("Actual dDSPI", "vecm"),
       col = c("darkblue", "red"), lwd = 1.5)

## ----plot vecm cli, fig.align='center', echo=FALSE----------------------------
plot(ts(diff.train$dCLI, start = c(1978, 2), frequency = 4),
     col = "darkblue", type = "l", lwd = 3,
     ylab = "dCLI", xlab = "Time",
     main = "Actual vs Fitted CLI at First Difference Using VECM")

# Start period is 1978Q4 since we used 2 lags, resulting in loss of first 2 observations
lines(ts(fitted(vec_ecm$rlm)[,"CLI.d"], start = c(1978, 4), frequency = 4), col = "red", type = "l", lwd = 3)

legend(x = "bottomleft", legend = c("Actual dCLI", "vecm"),
       col = c("darkblue", "red"), lwd = 1.5)

## ----vec2var------------------------------------------------------------------
var_ecm <- vec2var(get_vecm, r = 2)

var_ecm

## ----bg ardl------------------------------------------------------------------
# Test with up to 4 lags of the residuals

lmtest::bgtest(ardl, order = 4)

## ----bp ardl------------------------------------------------------------------
lmtest::bptest(ardl, studentize = T)

## ----bg var-------------------------------------------------------------------
vars::serial.test(var, lags.bg = 4, type = "BG")

## ----arch var-----------------------------------------------------------------
vars::arch.test(var, lags.single = 4, lags.multi = 4, multivariate.only = F)

## ----bg vecm------------------------------------------------------------------
vars::serial.test(var_ecm, lags.bg = 4, type = "BG")

## ----arch vecm----------------------------------------------------------------
vars::arch.test(var_ecm, lags.single = 4, lags.multi = 4, multivariate.only = F)

## ----arima1 h8----------------------------------------------------------------
arima1_h8 <- forecast(object = arima1, h = 8, level = 95)

stargazer(as.data.frame(arima1_h8), 
          type = "text",
          title = "Forecasted dCPI 2020Q1 to 2021Q4 Using arima1 Model", 
          summary = F)

stargazer(accuracy(object = arima1_h8, x = diff.test["2020/2021"]$dCPI), 
          type = "text", 
          title = "Accuracy Measures of arima1 Model for 8-Steps Ahead Forecasting")

## ----plot f_arima1, fig.align='center', echo=FALSE----------------------------
fcpi_arima1 <- ts(cbind(fcpi = as.vector(last(train$CPI)) + cumsum(arima1_h8$mean),
                     fcpi_lower = as.vector(last(train$CPI)) + cumsum(arima1_h8$lower),
                     fcpi_upper = as.vector(last(train$CPI)) + cumsum(arima1_h8$upper)),
                  start = c(2020, 1),
                  frequency = 4)

plot.zoo(cbind(as.zoo(fcpi_arima1), test$CPI["2020/2021",], train$CPI["2016/2019"]), 
         plot.type = "single", 
         col = c("red", "gray", "gray", "blue", "black"), lwd = 3,
         ylab = "CPI", xlab = "Year/Quarter",
         main = "Actual and Forecasted CPI Using Model arima1")

legend(x = "topleft", 
       legend = c("Forecasted CPI", "Lower and Upper Bound", "Actual CPI 1Q20 to 4Q21", "Actual CPI 1Q16 to 4Q19"), 
       col = c("red", "gray", "blue", "black"), 
       lwd = 1.5)

## ----arima2 h8, echo=FALSE----------------------------------------------------
arima2_h8 <- forecast(object = arima2, h = 8, level = 95)

stargazer(as.data.frame(arima2_h8), 
          type = "text",
          title = "Forecasted dCPI 2020Q1 to 2021Q4 Using arima2 Model", 
          summary = F)

stargazer(accuracy(object = arima2_h8, x = diff.test["2020/2021"]$dCPI), 
          type = "text", 
          title = "Accuracy Measures of arima2 Model for 8-Steps Ahead Forecasting")

## ----plot f_arima2, fig.align='center', echo=FALSE----------------------------
fcpi_arima2 <- ts(cbind(fcpi = as.vector(last(train$CPI)) + cumsum(arima2_h8$mean),
                     fcpi_lower = as.vector(last(train$CPI)) + cumsum(arima2_h8$lower),
                     fcpi_upper = as.vector(last(train$CPI)) + cumsum(arima2_h8$upper)),
                  start = c(2020, 1),
                  frequency = 4)

plot.zoo(cbind(as.zoo(fcpi_arima2), test$CPI["2020/2021",], train$CPI["2016/2019"]), 
         plot.type = "single", 
         col = c("red", "gray", "gray", "blue", "black"), lwd = 3,
         ylab = "CPI", xlab = "Year/Quarter",
         main = "Actual and Forecasted CPI Using Model arima2")

legend(x = "topleft", 
       legend = c("Forecasted CPI", "Lower and Upper Bound", "Actual CPI 1Q20 to 4Q21", "Actual CPI 1Q16 to 4Q19"), 
       col = c("red", "gray", "blue", "black"), 
       lwd = 1.5)

## ----ardl h8------------------------------------------------------------------
ardl.new <- ardlDlm(formula = dCPI ~ dDSPI + dCLI, # Indicate variables for the ARDL model
                    data = data.frame(diff.train), 
                    p = 2, q = 4, # Include 2 lags for exogenous variables, 4 lags for dependent variable
                    remove = list(p = list(dDSPI = c(2)))) # Remove second lag for dDSPI so that model is same as auto_ardl

stargazer(ardl.new$model, type = "text",
          title = "ARDL Regression Using dLagM Package", 
          dep.var.labels.include = F, 
          column.labels = "CPI.t")

x.new <- rbind(as.vector(diff.test["2020/2021", 2]), as.vector(diff.test["2020/2021", 3])) # function only accepts new data in matrix class

ardl_h8 <- dLagM::forecast(model = ardl.new, x = x.new, h = 8, interval = T, level = 0.95)

stargazer(ardl_h8$forecasts, 
          type = "text",
          title = "Forecasted dCPI 2020Q1 to 2021Q4 Using ardl Model", 
          summary = F)

# To use the accuracy function in the forecast package, we need to trick it into thinking it is a object of class "forecast"

ardl_h8adj <- structure(list(level = 95,
                             mean = ardl_h8$forecasts$Forecast, 
                             lower = ardl_h8$forecasts$`95% LB`, 
                             upper = ardl_h8$forecasts$`95% UB`, 
                             x = diff.train[, 1], 
                             fitted = c(rep(NA, 4), ardl.new$model$fitted.values),
                             residuals = c(rep(NA, 4), ardl.new$model$residuals)), 
                        class = "forecast")

stargazer(accuracy(object = ardl_h8adj, x = diff.test["2020/2021"]$dCPI), 
          type = "text", 
          title = "Accuracy Measures of ardl Model for 8-Steps Ahead Forecasting")

## ----plot f_ardl, fig.align='center', echo=FALSE------------------------------
fcpi_ardl <- ts(cbind(fcpi = as.vector(last(train$CPI)) + cumsum(ardl_h8adj$mean),
                      fcpi_lower = as.vector(last(train$CPI)) + cumsum(ardl_h8adj$lower),
                      fcpi_upper = as.vector(last(train$CPI)) + cumsum(ardl_h8adj$upper)),
                start = c(2020, 1),
                frequency = 4)

plot.zoo(cbind(as.zoo(fcpi_ardl), test$CPI["2020/2021",], train$CPI["2016/2019"]), 
         plot.type = "single", 
         col = c("red", "gray", "gray", "blue", "black"), lwd = 3,
         ylab = "CPI", xlab = "Year/Quarter",
         main = "Actual and Forecasted CPI Using Model ardl")

legend(x = "topleft", 
       legend = c("Forecasted CPI", "Lower and Upper Bound", "Actual CPI 1Q20 to 4Q21", "Actual CPI 1Q16 to 4Q19"), 
       col = c("red", "gray", "blue", "black"), 
       lwd = 1.5)

## ----ardl ecm h8--------------------------------------------------------------
ardlecm.new <- dLagM::ardlDlm(formula = CPI ~ DSPI + CLI, data = data.frame(train), p = 2, q = 3)

stargazer(ardlecm.new$model, 
          type = "text",
          title = "ECM Regression Using dLagM Package", 
          dep.var.labels.include = F, 
          column.labels = "CPI.t")

newdata <- rbind(as.vector(test["2020/2021", 2]), as.vector(test["2020/2021", 3]))

ecm_h8 <- dLagM::forecast(model = ardlecm.new, x = newdata, h = 8, interval = T, level = 0.95)

stargazer(ecm_h8$forecasts, 
          type = "text",
          title = "Forecasted dCPI 2020Q1 to 2021Q4 Using ecm Model", 
          summary = F)

# Since forecast values are CPI at level, we cannot directly compare to the accuracy measures of other models since they have different scales
# We can get the first difference CPI by taking the difference of the forecasted values

dcpi_ecm <- data.frame(fcst = na.omit(diff(append(ecm_h8$forecasts$Forecast, as.vector(last(train$CPI)), 0))),
                       lowerb = na.omit(diff(append(ecm_h8$forecasts$`95% LB`, as.vector(last(train$CPI)), 0))),
                       upperb = na.omit(diff(append(ecm_h8$forecasts$`95% UB`, as.vector(last(train$CPI)), 0))))

# Creating structure of class "forecast" that can be used with the accuracy function

ecm_h8adj <- structure(list(level = 95,
                            mean = dcpi_ecm$fcst,
                            lower = dcpi_ecm$lowerb,
                            upper = dcpi_ecm$upperb,
                            x = diff.train[, 1],
                            fitted = c(rep(NA, 2), r_ecm$fitted.values),
                            residuals = c(rep(NA, 2), r_ecm$residuals)),
                       class = "forecast")

stargazer(accuracy(object = ecm_h8adj, x = diff.test["2020/2021"]$dCPI), 
          type = "text", 
          title = "Accuracy Measures of ecm Model for 8-Steps Ahead Forecasting")

## ----plot ecm, fig.align='center', echo=FALSE---------------------------------
fcpi_ecm <- ts(cbind(fcpi = ecm_h8$forecasts$Forecast,
                     fcpi_lower = ecm_h8$forecasts$`95% LB`,
                     fcpi_upper = ecm_h8$forecasts$`95% UB`),
               start = c(2020, 1),
               frequency = 4)

plot.zoo(cbind(as.zoo(fcpi_ecm), test$CPI["2020/2021",], train$CPI["2016/2019"]), 
         plot.type = "single", 
         col = c("red", "gray", "gray", "blue", "black"), lwd = 3,
         ylab = "CPI", xlab = "Year/Quarter",
         main = "Actual and Forecasted CPI Using Model ardl-ecm")

legend(x = "topleft", 
       legend = c("Forecasted CPI", "Lower and Upper Bound", "Actual CPI 1Q20 to 4Q21", "Actual CPI 1Q16 to 4Q19"), 
       col = c("red", "gray", "blue", "black"), 
       lwd = 1.5)

## ----var h8-------------------------------------------------------------------
var_h8 <- predict(object = var, n.ahead = 8, ci = 0.95)

stargazer(var_h8$fcst, 
          type = "text", 
          title = c("Forecasted dCPI from 1Q20 to 4Q21", "Forecasted dDSPI from 1Q20 to 4Q21", "Forecasted dCLI from 1Q20 to 4Q21"))

# Creating structure of class "forecast" that can be used with the accuracy function

var_acc_list <- list()

for (i in 1:3) {
  fc <- structure(list(level = 95,
                       mean = var_h8$fcst[[i]][,"fcst"],
                       lower = var_h8$fcst[[i]][,"lower"],
                       upper = var_h8$fcst[[i]][,"upper"],
                       x = diff.train[,i],
                       fitted = c(NA, NA, fitted(var)[,i]),
                       residuals = c(NA, NA, resid(var)[,i])),
                  class = "forecast")
  
  name <- paste(names(var_h8$fcst[i]), "_acc", sep = "") 
  
  var_acc_list[[name]] <- fc
}

stargazer(accuracy(object = var_acc_list$dCPI_acc, x = diff.test["2020/2021"]$dCPI), 
          type = "text", 
          title = "Accuracy Measures of var Model for 8-Steps Ahead Forecasting for dCPI")

stargazer(accuracy(object = var_acc_list$dDSPI_acc, x = diff.test["2020/2021"]$dDSPI), 
          type = "text", 
          title = "Accuracy Measures of var Model for 8-Steps Ahead Forecasting for dDSPI")

stargazer(accuracy(object = var_acc_list$dCLI_acc, x = diff.test["2020/2021"]$dCLI), 
          type = "text", 
          title = "Accuracy Measures of var Model for 8-Steps Ahead Forecasting for dCLI")

## ----plot f_var, fig.align='center', echo=FALSE-------------------------------
fcpi_var <- ts(cbind(fcpi = as.vector(last(train$CPI)) + cumsum(var_h8$fcst[["dCPI"]][,"fcst"]),
                     fcpi_lower = as.vector(last(train$CPI)) + cumsum(var_h8$fcst[["dCPI"]][,"lower"]),
                     fcpi_upper = as.vector(last(train$CPI)) + cumsum(var_h8$fcst[["dCPI"]][,"upper"])),
               start = c(2020, 1),
               frequency = 4)

plot.zoo(cbind(as.zoo(fcpi_var), test$CPI["2020/2021",], train$CPI["2016/2019"]), 
         plot.type = "single", 
         col = c("red", "gray", "gray", "blue", "black"), lwd = 3,
         ylab = "CPI", xlab = "Year/Quarter",
         main = "Actual and Forecasted CPI Using Model var")

legend(x = "topleft", 
       legend = c("Forecasted CPI", "Lower and Upper Bound", "Actual CPI 1Q20 to 4Q21", "Actual CPI 1Q16 to 4Q19"), 
       col = c("red", "gray", "blue", "black"), 
       lwd = 1.5)

## ----var ecm h8---------------------------------------------------------------
vecm_h8 <- predict(var_ecm, n.ahead = 8, ci = 0.95)

stargazer(vecm_h8$fcst, 
          type = "text", 
          title = c("Forecasted dCPI from 1Q20 to 4Q21", "Forecasted dDSPI from 1Q20 to 4Q21", "Forecasted dCLI from 1Q20 to 4Q21"))

# Just as in the ARDL-ECM case, the forecasted values are in levels and we cannot directly compare to the other models.
# Need to take the difference of the forecasted values

dforecast_vecm <- list()

for (i in 1:3) {
  dforecast <- data.frame(fcst = na.omit(diff(append(vecm_h8$fcst[[i]][,"fcst"], as.vector(last(train$CPI)), 0))),
                          lowerb = na.omit(diff(append(vecm_h8$fcst[[i]][,"lower"], as.vector(last(train$CPI)), 0))),
                          upperb = na.omit(diff(append(vecm_h8$fcst[[i]][,"upper"], as.vector(last(train$CPI)), 0))))
  
  name <- paste("diff_", names(vecm_h8$fcst[i]), sep = "")
  
  dforecast_vecm[[name]] <- dforecast
}

# Create "forecast" structure to be used with the accuracy function

vecm_acc_list <- list()

for (i in 1:3) {
  fc <- structure(list(level = 95,
                       mean = dforecast_vecm[[i]][,"fcst"],
                       lower = dforecast_vecm[[i]][,"lowerb"],
                       upper = dforecast_vecm[[i]][,"upperb"],
                       x = diff.train[,i],
                       fitted = c(rep(NA, 2), fitted(vec_ecm[["rlm"]])[,i]),
                       residuals = c(rep(NA, 2), resid(vec_ecm[["rlm"]])[,i])),
                  class = "forecast")
  
  name <- paste(names(dforecast_vecm[i]), "_acc", sep = "") 
  
  vecm_acc_list[[name]] <- fc
}

stargazer(accuracy(object = vecm_acc_list$diff_CPI_acc, x = diff.test["2020/2021"]$dCPI), 
          type = "text", 
          title = "Accuracy Measures of vecm Model for 8-Steps Ahead Forecasting for dCPI")

stargazer(accuracy(object = vecm_acc_list$diff_DSPI_acc, x = diff.test["2020/2021"]$dDSPI), 
          type = "text", 
          title = "Accuracy Measures of vecm Model for 8-Steps Ahead Forecasting for dDSPI")

stargazer(accuracy(object = vecm_acc_list$diff_CLI_acc, x = diff.test["2020/2021"]$dCLI), 
          type = "text", 
          title = "Accuracy Measures of vecm Model for 8-Steps Ahead Forecasting for dCLI")

## ----plot f_vecm, fig.align='center', echo=FALSE------------------------------
fcpi_vecm <- ts(cbind(fcpi = vecm_h8$fcst[["CPI"]][,"fcst"],
                      fcpi_lower = vecm_h8$fcst[["CPI"]][,"lower"],
                      fcpi_upper = vecm_h8$fcst[["CPI"]][,"upper"]),
                start = c(2020, 1),
                frequency = 4)

plot.zoo(cbind(as.zoo(fcpi_vecm), test$CPI["2020/2021",], train$CPI["2016/2019"]), 
         plot.type = "single", 
         col = c("red", "gray", "gray", "blue", "black"), lwd = 3,
         ylab = "CPI", xlab = "Year/Quarter",
         main = "Actual and Forecasted CPI Using Model vecm")

legend(x = "topleft", 
       legend = c("Forecasted CPI", "Lower and Upper Bound", "Actual CPI 1Q20 to 4Q21", "Actual CPI 1Q16 to 4Q19"), 
       col = c("red", "gray", "blue", "black"), 
       lwd = 1.5)

