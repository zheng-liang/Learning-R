## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages, message=FALSE, warning=FALSE------------------------------
# Use install.packages("packagename") if they are not already installed

# Packages that will be needed for obtaining data via an API
library(httr)
library(jsonlite)

# Packages that will be needed for the main parts of the project
library(corrplot) # For visualizing correlation between variables
library(foreach) # Foreach loops
library(forecast) # For ARIMA models, forecasting and evaluation
library(tidyverse) # For ggplot2 and dplyr
library(zoo) # For converting data to and working with zoo objects

## ----import CPI data----------------------------------------------------------
# Import Singapore Monthly Consumer Price Index 

cpi_url <- "https://tablebuilder.singstat.gov.sg/api/table/tabledata/M212881?isTestApi=true&seriesNoORrowNo=1"

raw_cpi <- httr::GET(url = cpi_url)

cpi_content <- jsonlite::fromJSON(rawToChar(raw_cpi$content))

cpi_data <- as.data.frame(cpi_content$Data$row$columns[[1]])

# order.by argument helps to sort the data in time ascending order
cpi <- zoo(x = cpi_data$value, order.by = as.yearmon(cpi_data$key, format = "%Y %b"))

# Values are of class "character", need to change them to numeric 
storage.mode(cpi) <- "numeric"

head(cpi); tail(cpi)

## ----import DSPI data---------------------------------------------------------
# Import Singapore Monthly Domestic Supply Price Index

dspi_url <- "https://tablebuilder.singstat.gov.sg/api/table/tabledata/M212701?isTestApi=true&seriesNoORrowNo=1"

raw_dspi <- httr::GET(url = dspi_url)

dspi_content <- jsonlite::fromJSON(rawToChar(raw_dspi$content))

dspi_data <- as.data.frame(dspi_content$Data$row$columns[[1]])

dspi <- zoo(x = dspi_data$value, order.by = as.yearmon(dspi_data$key, format = "%Y  %b"))

storage.mode(dspi) <- "numeric"

head(dspi); tail(dspi)

## ----import CLI data----------------------------------------------------------
# Import Singapore Quarterly Composite Leading Index

cli_url <- "https://tablebuilder.singstat.gov.sg/api/table/tabledata/M240421?isTestApi=true"

raw_cli <- httr::GET(url = cli_url)

cli_content <- jsonlite::fromJSON(rawToChar(raw_cli$content))

cli_data <- as.data.frame(cli_content$Data$row$columns[[1]])

cli <- zoo(x = cli_data$value, order.by = as.yearqtr(cli_data$key, format = "%Y %q"))

storage.mode(cli) <- "numeric"

head(cli); tail(cli)

## ----quarterly CPI and DSPI data----------------------------------------------
# Aggregate monthly data to quarterly data using mean value of the months in a quarter

cpi_q <- aggregate(cpi, as.yearqtr, mean)

dspi_q <- aggregate(dspi, as.yearqtr, mean)

# Adjust the sample size to match CLI data

cpi_q <- window(x = cpi_q, start = start(cli), end = end(cli))

head(cpi_q); tail(cpi_q)

dspi_q <- window(x = dspi_q, start = start(cli), end = end(cli))

head(dspi_q); tail(dspi_q)

## ----merge CPI DSPI and CLI---------------------------------------------------
dat <- cbind(CPI = cpi_q, DSPI = dspi_q, CLI = cli)

head(dat); dim(dat)

## ----difference data----------------------------------------------------------
diff_dat <- diff(x = dat, lag = 1, differences = 1) %>% 
  `colnames<-`(c("dCPI", "dDSPI", "dCLI"))

head(diff_dat); dim(diff_dat)

## ----plot of level and differenced CPI, fig.align='center'--------------------
par(mfrow = c(2, 1))

plot(dat$CPI, main = "Singapore Quarterly CPI from 1978Q1 to 2022Q1", ylab = "CPI")

plot(diff_dat$dCPI, main = "Differenced CPI from 1978Q2 to 2022Q1", ylab = "dCPI")

## ----visually check for seasonality in dCPI, fig.align='center'---------------
startdates = c("1978Q1", "1988Q1", "1998Q1", "2008Q1")
enddates = c("1987Q4", "1997Q4", "2007Q4", "2017Q4")

par(mfrow = c(2, 2))

invisible(
foreach(i = startdates, j = enddates) %do% {
  
  boxplot(window(x = diff_dat$dCPI, start = i, end = j) ~ cycle(window(x = diff_dat$dCPI, start = i, end = j)),
          ylab = "Quarterly dCPI", xlab = "Quarters",
          main = paste(i, "to", j, sep = " "))
  
}
)

## ----create dummy variables---------------------------------------------------
sdum <- zoo(forecast::seasonaldummy(x = as.ts(diff_dat)), order.by = index(diff_dat))

diff_dat <- merge(diff_dat, sdum)

head(diff_dat); tail(diff_dat)

## ----train and test dataset---------------------------------------------------
train <- window(x = diff_dat, end = "2020 Q4")

test <- window(x = diff_dat, start = "2021 Q1")

dim(train); dim(test)

## ----ARMA model, fig.align='center'-------------------------------------------
train$dCPI %>% tsdisplay(main = "ACF and PACF of dCPI")

# From the PACF, we might use 1 or 2 AR lags. From the ACF, we might use up to 4 MA lags

mod1 <- forecast::auto.arima(y = train$dCPI,
                             max.p = 4, max.q = 4, 
                             seasonal = F, ic = "aic", 
                             stepwise = F, approximation = F, trace = F)

mod1

checkresiduals(object = mod1)

## ----plot fitted ARMA model and actual values, fig.align='center'-------------
plot(train$dCPI, 
     type = "l", lwd = 2, col = "black", 
     ylab = "Quarterly dCPI", xlab = "Year", 
     main = "Fitted dCPI from ARMA Model")

lines(zoo(x = mod1$fitted, order.by = index(train)), 
      type = "l", lwd = 2, col = "red")

legend(x = "bottomleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lwd = 2)

## ----SARMA model--------------------------------------------------------------
mod2 <- forecast::auto.arima(y = train$dCPI, 
                             max.p = 4, max.q = 4, 
                             seasonal = T, ic = "aic", 
                             stepwise = F, approximation = F, trace = F)

mod2

checkresiduals(object = mod2, plot = F)

## ----plot fitted SARMA model and actual values, fig.align='center'------------
plot(train$dCPI, 
     type = "l", lwd = 2, col = "black", 
     ylab = "Quarterly dCPI", xlab = "Year",
     main = "Fitted dCPI from SARMA Model")

lines(zoo(x = mod2$fitted, order.by = index(train)), 
      type = "l", lwd = 2, col = "red")

legend(x = "bottomleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lwd = 2)

## ----ARMA model with seasonal dummies-----------------------------------------
mod3 <- forecast::auto.arima(y = train$dCPI, 
                             max.p = 4, max.q = 4,
                             seasonal = F, ic = "aic", 
                             stepwise = F, approximation = F, trace = F,
                             xreg = train[, 4:6])

mod3

checkresiduals(object = mod3, plot = F)

## ----plot fitted ARMA model with seasonal dummies and actual values, fig.align='center'----
plot(train$dCPI, 
     type = "l", lwd = 2, col = "black", 
     ylab = "Quarterly dCPI", xlab = "Year",
     main = "Fitted dCPI from ARMA Model with Seasonal Dummies")

lines(zoo(x = mod3$fitted, order.by = index(train)), 
      type = "l", lwd = 2, col = "red")

legend(x = "bottomleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lwd = 2)

## ----ARMAX model without seasonal dummies, fig.align='center'-----------------
mod4 <- forecast::auto.arima(y = train$dCPI, 
                             max.p = 4, max.q = 4,
                             seasonal = F, ic = "aic", 
                             stepwise = F, approximation = F, trace = F,
                             xreg = train[, 2:3])

mod4

checkresiduals(object = mod4, plot = F)

## ----plot fitted ARMAX model without seasonal dummies and actual values, fig.align='center'----
plot(train$dCPI, 
     type = "l", lwd = 2, col = "black", 
     ylab = "Quarterly dCPI", xlab = "Year",
     main = "Fitted dCPI from ARMAX Model without Seasonal Dummies")

lines(zoo(x = mod4$fitted, order.by = index(train)), 
      type = "l", lwd = 2, col = "red")

legend(x = "bottomleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lwd = 2)

## ----ARMAX model with seasonal dummies----------------------------------------
mod5 <- forecast::auto.arima(y = train$dCPI, 
                             max.p = 4, max.q = 4,
                             seasonal = F, ic = "aic", 
                             stepwise = F, approximation = F, trace = F,
                             xreg = train[, 2:6])

mod5

checkresiduals(object = mod5, plot = F)

## ----plot fitted ARMAX model with seasonal dummies and actual values, fig.align='center'----
plot(train$dCPI, 
     type = "l", lwd = 2, col = "black", 
     ylab = "Quarterly dCPI", xlab = "Year",
     main = "Fitted dCPI from ARMAX Model with Seasonal Dummies")

lines(zoo(x = mod5$fitted, order.by = index(train)), 
      type = "l", lwd = 2, col = "red")

legend(x = "bottomleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lwd = 2)

## ----SARMAX model-------------------------------------------------------------
mod6 <- forecast::auto.arima(y = train$dCPI, 
                             max.p = 4, max.q = 4,
                             seasonal = T, ic = "aic", 
                             stepwise = F, approximation = F, trace = F,
                             xreg = train[, 2:3])

mod6

checkresiduals(object = mod6, plot = F)

## ----plot fitted SARMAX model and actual values, fig.align='center'-----------
plot(train$dCPI, 
     type = "l", lwd = 2, col = "black", 
     ylab = "Quarterly dCPI", xlab = "Year",
     main = "Fitted dCPI from SARMAX Model")

lines(zoo(x = mod6$fitted, order.by = index(train)), 
      type = "l", lwd = 2, col = "red")

legend(x = "bottomleft", legend = c("Actual", "Fitted"), col = c("black", "red"), lwd = 2)

## ----forecast using ARMA model, fig.align='center'----------------------------
f_mod1 <- forecast(object = mod1, h = 5, level = 95)

merge(`Actual dCPI` = test$dCPI, 
      as.zoo(cbind(`Point Forecast` = f_mod1$mean, `Lo 95` = f_mod1$lower, `Hi 95` = f_mod1$upper)))

f_mod1.level <- zoo(cbind(fCPI = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod1$mean),
                          fCPI_lower = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod1$lower),
                          fCPI_upper = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod1$upper)),
                    order.by = index(test))

plot(merge(dat$CPI[index(dat) >= "2018 Q1"], f_mod1.level), 
     type = "l", col = c("black", "red", "gray", "gray"), lwd = 2,
     ylab = "Quarterly CPI", xlab = "Year", main = "Forecasted CPI Using ARMA Model",
     plot.type = "single")

legend(x = "topleft", 
       legend = c("Actual CPI 2018Q1 to 2022Q1", "Forecasted CPI 2021Q1 to 2022Q2", "Lower and Upper Bound"), 
       col = c("black", "red", "gray"), lwd = 2)

## ----forecast using SARMA model, fig.align='center'---------------------------
f_mod2 <- forecast(object = mod2, h = 5, level = 95)

merge(`Actual dCPI` = test$dCPI, 
      as.zoo(cbind(`Point Forecast` = f_mod2$mean, `Lo 95` = f_mod2$lower, `Hi 95` = f_mod2$upper)))

f_mod2.level <- zoo(cbind(fCPI = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod2$mean),
                          fCPI_lower = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod2$lower),
                          fCPI_upper = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod2$upper)),
                    order.by = index(test))

plot(merge(dat$CPI[index(dat) >= "2018 Q1"], f_mod2.level), 
     type = "l", col = c("black", "red", "gray", "gray"), lwd = 2,
     ylab = "Quarterly CPI", xlab = "Year", main = "Forecasted CPI Using SARMA Model",
     plot.type = "single")

legend(x = "topleft", 
       legend = c("Actual CPI 2018Q1 to 2022Q1", "Forecasted CPI 2021Q1 to 2022Q2", "Lower and Upper Bound"), 
       col = c("black", "red", "gray"), lwd = 2)

## ----forecast using ARMA model with seasonal dummies, fig.align='center'------
f_mod3 <- forecast(object = mod3, h = 5, level = 95, xreg = test[, 4:6])

merge(`Actual dCPI` = test$dCPI, 
      as.zoo(cbind(`Point Forecast` = f_mod3$mean, `Lo 95` = f_mod3$lower, `Hi 95` = f_mod3$upper)))

f_mod3.level <- zoo(cbind(fCPI = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod3$mean),
                          fCPI_lower = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod3$lower),
                          fCPI_upper = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod3$upper)),
                    order.by = index(test))

plot(merge(dat$CPI[index(dat) >= "2018 Q1"], f_mod3.level), 
     type = "l", col = c("black", "red", "gray", "gray"), lwd = 2,
     ylab = "Quarterly CPI", xlab = "Year", main = "Forecasted CPI Using ARMA Model with Seasonal Dummies",
     plot.type = "single")

legend(x = "topleft", 
       legend = c("Actual CPI 2018Q1 to 2022Q1", "Forecasted CPI 2021Q1 to 2022Q2", "Lower and Upper Bound"), 
       col = c("black", "red", "gray"), lwd = 2)

## ----forecast using ARMAX model without seasonal dummies, fig.align='center'----
f_mod4 <- forecast(object = mod4, h = 5, level = 95, xreg = test[, 2:3])

merge(`Actual dCPI` = test$dCPI, 
      as.zoo(cbind(`Point Forecast` = f_mod4$mean, `Lo 95` = f_mod4$lower, `Hi 95` = f_mod4$upper)))

f_mod4.level <- zoo(cbind(fCPI = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod4$mean),
                          fCPI_lower = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod4$lower),
                          fCPI_upper = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod4$upper)),
                    order.by = index(test))

plot(merge(dat$CPI[index(dat) >= "2018 Q1"], f_mod4.level), 
     type = "l", col = c("black", "red", "gray", "gray"), lwd = 2,
     ylab = "Quarterly CPI", xlab = "Year", main = "Forecasted CPI Using ARMAX Model without Seasonal Dummies",
     plot.type = "single")

legend(x = "topleft", 
       legend = c("Actual CPI 2018Q1 to 2022Q1", "Forecasted CPI 2021Q1 to 2022Q2", "Lower and Upper Bound"), 
       col = c("black", "red", "gray"), lwd = 2)

## ----forecast using ARMAX model with seasonal dummies, fig.align='center'-----
f_mod5 <- forecast(object = mod5, h = 5, level = 95, xreg = test[, 2:6])

merge(`Actual dCPI` = test$dCPI, 
      as.zoo(cbind(`Point Forecast` = f_mod5$mean, `Lo 95` = f_mod5$lower, `Hi 95` = f_mod5$upper)))

f_mod5.level <- zoo(cbind(fCPI = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod5$mean),
                          fCPI_lower = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod5$lower),
                          fCPI_upper = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod5$upper)),
                    order.by = index(test))

plot(merge(dat$CPI[index(dat) >= "2018 Q1"], f_mod5.level), 
     type = "l", col = c("black", "red", "gray", "gray"), lwd = 2,
     ylab = "Quarterly CPI", xlab = "Year", main = "Forecasted CPI Using ARMAX Model with Seasonal Dummies",
     plot.type = "single")

legend(x = "topleft", 
       legend = c("Actual CPI 2018Q1 to 2022Q1", "Forecasted CPI 2021Q1 to 2022Q2", "Lower and Upper Bound"), 
       col = c("black", "red", "gray"), lwd = 2)

## ----forecast using SARMAX model, fig.align='center'--------------------------
f_mod6 <- forecast(object = mod6, h = 5, level = 95, xreg = test[, 2:3])

merge(`Actual dCPI` = test$dCPI, 
      as.zoo(cbind(`Point Forecast` = f_mod6$mean, `Lo 95` = f_mod6$lower, `Hi 95` = f_mod6$upper)))

f_mod6.level <- zoo(cbind(fCPI = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod6$mean),
                          fCPI_lower = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod6$lower),
                          fCPI_upper = as.vector(dat$CPI[index(dat) == "2020 Q4",]) + cumsum(f_mod6$upper)),
                    order.by = index(test))

plot(merge(dat$CPI[index(dat) >= "2018 Q1"], f_mod6.level), 
     type = "l", col = c("black", "red", "gray", "gray"), lwd = 2,
     ylab = "Quarterly CPI", xlab = "Year", main = "Forecasted CPI Using SARMAX Model",
     plot.type = "single")

legend(x = "topleft", 
       legend = c("Actual CPI 2018Q1 to 2022Q1", "Forecasted CPI 2021Q1 to 2022Q2", "Lower and Upper Bound"), 
       col = c("black", "red", "gray"), lwd = 2)

## ----merge point forecasts to data frame--------------------------------------
ptfor <- cbind(model1 = f_mod1.level$fCPI, 
               model2 = f_mod2.level$fCPI,
               model3 = f_mod3.level$fCPI, 
               model4 = f_mod4.level$fCPI,
               model5 = f_mod5.level$fCPI, 
               model6 = f_mod6.level$fCPI)

ptfor

## ----forecasting performance of models----------------------------------------
results <- foreach (i = 1:6) %do% {
  accuracy(object = as.ts(ptfor[,i]), x = dat$CPI[index(dat) >= "2021 Q1"])
}

names(results) <- colnames(ptfor)

results

