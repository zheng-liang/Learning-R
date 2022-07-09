## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages, message=FALSE, warning=FALSE------------------------------
library(dplyr)
library(doParallel) # For parallel computation in foreach loops
library(PerformanceAnalytics) # For portfolio performance and risk analysis
library(PortfolioAnalytics) # For portfolio optimization and analysis
library(quantmod) # For obtaining historical prices from Yahoo Finance

## ----msft price---------------------------------------------------------------
startdate <- "2010-01-01"
enddate <- "2022-05-31"

MSFT <- quantmod::getSymbols(Symbols = "MSFT", # Indicate a symbol that corresponds to stock in Yahoo Finance
                             src = "yahoo", # Indicate to search stock prices from Yahoo Finance
                             from = startdate, # Indicate a start date
                             to = enddate, # Indicate an end date
                             periodicity = "monthly", # Indicate the periodicity of prices to retrieve, can also be "daily" or "weekly"
                             auto.assign = F) # Indicate FALSE so that results are assigned to variable we indicate

head(MSFT, n = 4); tail(MSFT, n = 4)

colSums(is.na(MSFT)) # To check for NA values

## ----msft returns-------------------------------------------------------------
MSFT_returns <- PerformanceAnalytics::Return.calculate(prices = MSFT[,6], # Use the 6th column of MSFT (Adjusted Price)
                                                       method = "discrete" # Calculate the arithmetic/discrete returns, use "log" for log/continuous returns
                                                       )[-1,] # Remove first row since first price observation cannot be used to calculate return

head(MSFT_returns, 4)

## ----stddev msft--------------------------------------------------------------
MSFT_stddev <- PerformanceAnalytics::StdDev(R = MSFT_returns)

MSFT_stddev

## ----distribution of msft returns, fig.align='center'-------------------------
PerformanceAnalytics::chart.Histogram(MSFT_returns, 
                                      main = "Distribution of MSFT Daily Returns",
                                      methods = c("add.density", "add.normal"))

PerformanceAnalytics::skewness(MSFT_returns, method = "sample")

PerformanceAnalytics::kurtosis(MSFT_returns, method = "sample")

## ----semid msft---------------------------------------------------------------
MSFT_semidev <- PerformanceAnalytics::SemiDeviation(MSFT_returns)

MSFT_semidev

## ----dd msft------------------------------------------------------------------
MSFT_downdev <- PerformanceAnalytics::DownsideDeviation(MSFT_returns, MAR = 0.03/12) # divide by 252 to change to daily periodicity

MSFT_downdev

## ----plot VaR, fig.align='center'---------------------------------------------
PerformanceAnalytics::chart.Histogram(MSFT_returns, 
                                      main = "Distribution of MSFT Monthly Returns with VaR",
                                      methods = c("add.risk")) # Indicate to show VaR and Modified VaR

## ----HVaR msft----------------------------------------------------------------
MSFT_hvar <- PerformanceAnalytics::VaR(MSFT_returns,
                                       p = 0.95, # confidence level of 95%
                                       method = "historical") # use historical VaR

MSFT_hvar

## ----PVaR msft----------------------------------------------------------------
MSFT_pvar <- PerformanceAnalytics::VaR(MSFT_returns,
                                       p = 0.95, 
                                       method = "gaussian") # gaussian (normal) distribution

MSFT_pvar

## ----modVaR msft--------------------------------------------------------------
MSFT_modvar <- PerformanceAnalytics::VaR(MSFT_returns, 
                                         p = 0.95, 
                                         method = "modified") # use modified VaR

MSFT_modvar

## ----historical CVaR msft-----------------------------------------------------
MSFT_histcvar <- PerformanceAnalytics::CVaR(MSFT_returns, 
                                            p = 0.95, 
                                            method = "historical")

MSFT_histcvar

## ----parametric CVaR msft-----------------------------------------------------
MSFT_parcvar <- PerformanceAnalytics::CVaR(MSFT_returns,
                                           p = 0.95, 
                                           method = "gaussian")

MSFT_parcvar

## ----modified CVaR msft-------------------------------------------------------
MSFT_modcvar <- PerformanceAnalytics::CVaR(MSFT_returns, 
                                           p = 0.95, 
                                           method = "modified")

MSFT_modcvar

## ----summary of risk measures-------------------------------------------------
c(Std_Dev = MSFT_stddev, Semi_Dev = MSFT_semidev, Downside_Dev = MSFT_downdev)

c(H_VaR = MSFT_hvar, P_VaR = MSFT_pvar, Mod_VaR = MSFT_modvar,
  H_CVaR = MSFT_histcvar, P_CVaR = MSFT_parcvar, Mod_CVaR = MSFT_modcvar)

## ----portfolio price data-----------------------------------------------------
tickers <- c("AAPL", "MSFT", "JNJ", "PG", "MA", "HD", "PFE", "MRK", "COST", "NVDA")

price_data <- NULL

for (ticker in tickers) {
  price_data <- cbind(price_data,
                      quantmod::getSymbols(ticker, 
                                           src = "yahoo", 
                                           from = startdate, to = enddate, 
                                           periodicity = "monthly", 
                                           auto.assign = F)[,6])
}

colnames(price_data) <- tickers

## ----head tail of price data--------------------------------------------------
head(price_data, n = 4); tail(price_data, n = 4)

nrow(price_data)

colSums(is.na(price_data))

## ----individual security return-----------------------------------------------
returns <- na.omit(PerformanceAnalytics::Return.calculate(price_data, method = "discrete"))

head(returns)

nrow(returns)

## ----initial portfolio spec---------------------------------------------------
init_spec <- PortfolioAnalytics::portfolio.spec(assets = colnames(returns))

# Sum of weights constrained to 1 but require some "wiggle" room for random portfolio generation
init_spec <- add.constraint(portfolio = init_spec, 
                            type = "weight_sum", 
                            min_sum = .99, max_sum = 1.01)

# Weight of each asset is constrained to min of 0 and max of 20% of portfolio
init_spec <- add.constraint(portfolio = init_spec, 
                            type = "box", 
                            min = 0, max = 0.20)

## ----random portfolios--------------------------------------------------------
set.seed(1)

randport <- random_portfolios(portfolio = init_spec, 
                              permutations = 25000, 
                              rp_method = "sample",
                              eliminate = T) # Eliminate portfolios that do not meet requirements

dim(randport) # 24999 random portfolios found

## ----random portfolios returns, warning=FALSE, message=FALSE------------------
# Important to make sure that we do not use all cores for computation. Check with detectCores(logical = F)
mycluster <- makeCluster(6) # Number of cores to use. I have 8, but may not be suitable for everyone.

registerDoParallel(mycluster) # Register parallel computing

total_time <- system.time(
output1 <- foreach(i = 1:nrow(randport), .combine = "cbind") %dopar% {
  
  rp_return <- PerformanceAnalytics::Return.portfolio(R = returns, 
                                                      weights = randport[i,], 
                                                      geometric = T,
                                                      rebalance_on = "quarters")
  
  }
) # system.time to measure time take to run the loop

stopCluster(mycluster) # End parallel computing

total_time

dim(output1)

## ----minimize standard deviation----------------------------------------------
stddev_search <- PerformanceAnalytics::StdDev(R = output1, portfolio_method = "single")

min(stddev_search)

weight_MinVar <- randport[which.min(stddev_search),]

weight_MinVar

## ----minimize semi-deviation--------------------------------------------------
semidev_search <- PerformanceAnalytics::SemiDeviation(R = output1)

min(semidev_search)

weight_MinSemiDev <- randport[which.min(semidev_search),]

weight_MinSemiDev

## ----minimize downside deviation----------------------------------------------
downdev_search <- PerformanceAnalytics::DownsideDeviation(R = output1, MAR = 0.03/12)

min(downdev_search)

weight_MinDD <- randport[which.min(downdev_search),]

weight_MinDD

## ----minimize historical VaR--------------------------------------------------
hvar_search <- PerformanceAnalytics::VaR(R = output1, p = 0.95, method = "historical")

max(hvar_search)

weight_MinHVaR <- randport[which.max(hvar_search),]

weight_MinHVaR

## ----minimize historical CVaR-------------------------------------------------
hcvar_search <- PerformanceAnalytics::CVaR(R = output1, p = 0.95, method = "historical")

max(hcvar_search)

weight_MinHCVaR <- randport[which.max(hcvar_search),]

weight_MinHCVaR

## ----weights of minimum risk portfolios---------------------------------------
port_weights <- rbind(Min_Var = weight_MinVar, 
                      Min_SemiDev = weight_MinSemiDev, 
                      Min_DownDev = weight_MinDD, 
                      Min_HVaR = weight_MinHVaR, 
                      Min_HCVaR = weight_MinHCVaR)

port_weights

## ----extract monthly returns of min risk portfolios---------------------------
return_comp <- cbind(output1[, which.min(stddev_search)],
                     output1[, which.min(semidev_search)],
                     output1[, which.min(downdev_search)],
                     output1[, which.max(hvar_search)],
                     output1[, which.max(hcvar_search)]) %>%
  `colnames<-`(c("Min_Var", "Min_SemiDev", "Min_DownDev", "Min_HVaR", "Min_HCVaR"))

head(return_comp)

## ----benchmark return---------------------------------------------------------
benchmark <- quantmod::getSymbols(Symbols = "SPY",
                                  src = "yahoo", 
                                  from = startdate, to = enddate, 
                                  periodicity = "monthly", 
                                  auto.assign = F)[,6] %>%
  `colnames<-`("SPY")

benchmark_return <- na.omit(PerformanceAnalytics::Return.calculate(prices = benchmark, method = "discrete"))

head(benchmark_return); dim(benchmark_return)

## ----plot performance chart of minimum risk portfolios, fig.align='center'----
PerformanceAnalytics::chart.CumReturns(R = cbind(return_comp, benchmark_return), 
                                       geometric = T, # Use geometric chaining for portfolio cumulative returns over time
                                       legend.loc = "topleft",
                                       main = "Cumulative Returns of Minimum Risk Portfolios")

PerformanceAnalytics::chart.Drawdown(R = cbind(return_comp, benchmark_return), 
                                     geometric = T,
                                     legend.loc = "bottom",
                                     main = "Drawdown of Minimum Risk Portfolios")

## ----stats and annualized metrics of minimum risk portfolios------------------
table.AnnualizedReturns(R = cbind(return_comp, benchmark_return), scale = 12, Rf = 0.02/12, geometric = T, digits = 4)

table.Stats(R = cbind(return_comp, benchmark_return), digits = 4)

table.CAPM(Ra = return_comp, Rb = benchmark_return, scale = 12, Rf = 0.02/12, digits = 4)

table.DownsideRisk(R = cbind(return_comp, benchmark_return), scale = 12, Rf = 0.02/12, MAR = 0.03/12, digits = 4)

## ----subset of randport with target return------------------------------------
monthly_meanret <- PerformanceAnalytics::Mean.arithmetic(x = output1)

target_return <- monthly_meanret > 0.199/12 & monthly_meanret < 0.201/12

subrp_return <- output1[, target_return == "TRUE"]

dim(subrp_return)

## ----subset randport weights--------------------------------------------------
sub_randport <- randport[target_return == "TRUE",]

## ----mean variance portfolio--------------------------------------------------
meanvar_search <- PerformanceAnalytics::StdDev(R = subrp_return, portfolio_method = "single")

min(meanvar_search)

weight_MeanVar <- sub_randport[which.min(meanvar_search),]

weight_MeanVar

## ----mean semidev portfolio---------------------------------------------------
meansemidev_search <- PerformanceAnalytics::SemiDeviation(R = subrp_return)

min(meansemidev_search)

weight_MeanSemiDev <- sub_randport[which.min(meansemidev_search),]

weight_MeanSemiDev

## ----mean downdev portfolio---------------------------------------------------
meandowndev_search <- PerformanceAnalytics::DownsideDeviation(R = subrp_return, MAR = 0.03/12)

min(meandowndev_search)

weight_MeanDownDev <- sub_randport[which.min(meandowndev_search),]

weight_MeanDownDev

## ----mean VaR portfolio-------------------------------------------------------
meanVaR_search <- PerformanceAnalytics::VaR(R = subrp_return, p = 0.95, method = "historical")

max(meanVaR_search)

weight_MeanVaR <- sub_randport[which.max(meanVaR_search),]

weight_MeanVaR

## ----mean CVaR portfolio------------------------------------------------------
meanCVaR_search <- PerformanceAnalytics::CVaR(R = subrp_return, p = 0.95, method = "historical")

max(meanCVaR_search)

weight_MeanCVaR <- sub_randport[which.max(meanCVaR_search),]

weight_MeanCVaR

## ----weights of mean risk portfolios------------------------------------------
weight_comp <- rbind(Mean_Var = weight_MeanVar,
                     Mean_SemiDev = weight_MeanSemiDev,
                     Mean_DownDev = weight_MeanDownDev,
                     Mean_VaR = weight_MeanVaR,
                     Mean_CVaR = weight_MeanCVaR)

weight_comp

## ----extract monthly returns of optimal portfolios----------------------------
return_comp <- cbind(subrp_return[, which.min(meanvar_search)],
                     subrp_return[, which.min(meansemidev_search)],
                     subrp_return[, which.min(meandowndev_search)],
                     subrp_return[, which.max(meanVaR_search)],
                     subrp_return[, which.max(meanCVaR_search)]) %>%
  `colnames<-`(c("Mean_Var", "Mean_SemiDev", "Mean_DownDev", "Mean_VaR", "Mean_CVaR"))

dim(return_comp)

## ----plot performance chart of optimal portfolios, fig.align='center'---------
PerformanceAnalytics::chart.CumReturns(R = cbind(return_comp, benchmark_return), 
                                       geometric = T, # Use geometric chaining for portfolio cumulative returns over time
                                       legend.loc = "topleft",
                                       main = "Cumulative Returns of Optimal Portfolios")

PerformanceAnalytics::chart.Drawdown(R = cbind(return_comp, benchmark_return), 
                                     geometric = T,
                                     legend.loc = "bottom",
                                     main = "Drawdown of Optimal Portfolios")

## ----stats and annualized metrics of optimal portfolios-----------------------
table.AnnualizedReturns(R = cbind(return_comp, benchmark_return), scale = 12, Rf = 0.02/12, geometric = T, digits = 4)

table.Stats(R = cbind(return_comp, benchmark_return), digits = 4)

table.CAPM(Ra = return_comp, Rb = benchmark_return, scale = 12, Rf = 0.02/12, digits = 4)

table.DownsideRisk(R = cbind(return_comp, benchmark_return), scale = 12, Rf = 0.02/12, MAR = 0.03/12, digits = 4)

