Forecasting Singapore’s Consumer Price Index Using Different Time Series
Models
================

-   [1 Introduction](#1-introduction)
-   [2 Packages Required](#2-packages-required)
-   [3 Importing Data Using API](#3-importing-data-using-api)
-   [4 Exploratory Data Analysis](#4-exploratory-data-analysis)
    -   [4.1 Consumer Price Index](#41-consumer-price-index)
    -   [4.2 Domestic Supply Price
        Index](#42-domestic-supply-price-index)
    -   [4.3 Composite Leading Index](#43-composite-leading-index)
    -   [4.5 Multivariate Analysis](#45-multivariate-analysis)
-   [5 Splitting Data Into Training and Testing
    Sets.](#5-splitting-data-into-training-and-testing-sets)
-   [6 Unit Root Test for
    Stationarity](#6-unit-root-test-for-stationarity)
-   [7 Model Selection](#7-model-selection)
    -   [7.1 Autoregressive (Integrated) Moving Average
        Model](#71-autoregressive-integrated-moving-average-model)
    -   [7.2 Autoregressive Distributed Lag
        Models](#72-autoregressive-distributed-lag-models)
    -   [7.3 Vector Autoregressive
        Model](#73-vector-autoregressive-model)
-   [8 Granger Causality Test](#8-granger-causality-test)
-   [9 Johansen Test for
    Cointegration](#9-johansen-test-for-cointegration)
    -   [9.1 Error Correction Model](#91-error-correction-model)
    -   [9.2 Vector Error Correction
        Model](#92-vector-error-correction-model)
-   [10 Post-Estimation Tests](#10-post-estimation-tests)
    -   [10.1 ARDL Model](#101-ardl-model)
    -   [10.2 VAR Model](#102-var-model)
    -   [10.3 VECM](#103-vecm)
-   [11 Forecasting and Evaluation](#11-forecasting-and-evaluation)
    -   [11.1 Forecast Performance of ARIMA
        Models](#111-forecast-performance-of-arima-models)
    -   [11.2 Forecast Performance of ARDL and ARDL-EC
        Models](#112-forecast-performance-of-ardl-and-ardl-ec-models)
    -   [11.3 Forecast Performance of VAR and VEC
        Models](#113-forecast-performance-of-var-and-vec-models)
-   [12 Conclusion](#12-conclusion)

## 1 Introduction

The purpose of this project is to introduce time series modelling and
forecasting in R using a practical example. It covers the common tests
that are done in time series analysis, such as unit root tests for
stationarity, Breusch-Godfrey test for serial correlation, and Granger
Causality test for determining whether variables are useful in
forecasting other variables. The main models used in this project are
the ARIMA, ARDL and VAR models, which will be discussed in detail in the
later sections. However, I would not be tackling the issue of
seasonality in this project as it will become a lengthy discussion.
Instead, I would do this in a future project as I get more familiar with
the treatment of seasonal effects on time series and compare the results
against the ones I have documented here.

I attempted to forecast Singapore’s Consumer Price Index (CPI) using the
Producer Price Index (PPI) and the Composite Leading Index (CLI). The
data used in this project was obtained from the Department of Statistics
(DOS) Singapore and can be accessed from the [SingStat Table
Builder](https://tablebuilder.singstat.gov.sg/). The data was imported
via an API, but it can also be downloaded as an Excel or CSV file.

## 2 Packages Required

``` r
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
```

    ## 
    ## bruceR (version 0.8.7)
    ## BRoadly Useful Convenient and Efficient R functions
    ## 
    ## Packages also loaded:
    ## √ dplyr      √ emmeans       √ ggplot2
    ## √ tidyr      √ effectsize    √ ggtext
    ## √ stringr    √ performance   √ cowplot
    ## √ forcats    √ lmerTest      √ see
    ## √ data.table
    ## 
    ## Main functions of `bruceR`:
    ## cc()             Describe()  TTEST()
    ## add()            Freq()      MANOVA()
    ## .mean()          Corr()      EMMEANS()
    ## set.wd()         Alpha()     PROCESS()
    ## import()         EFA()       model_summary()
    ## print_table()    CFA()       lavaan_summary()
    ## 
    ## https://psychbruce.github.io/bruceR/

``` r
library(corrplot) # For visualizing correlation between variables
library(dLagM) # For creating ARDL objects that can work with a forecast function
library(forecast) # For ARIMA models, forecasting and evaluation
library(lmtest) # For tests such as Breusch-Godfrey, Breusch-Pagan
library(lubridate) # For working with dates
library(tidyverse) # For ggplot2 and dplyr
library(urca) # For unit root tests
library(xts) # For converting data to and working with xts objects
library(vars) # For VAR models
```

## 3 Importing Data Using API

To import data using an API, we need to use the **`GET`** function in
the `httr` package. We need to specify a URL to send a request for data
retrieval. Websites that has an API usually has their own documentation
on how to structure the parameters of the URL. For DOS Singapore, the
documentation can be found
[here](https://tablebuilder.singstat.gov.sg/view-api/for-developers).

I have included the process in detail for retrieving the “[Consumer
Price Index, 2019 As Base
Year](https://tablebuilder.singstat.gov.sg/table/TS/M212881)” data. For
the rest of the variables, I would run the steps in a single R chunk for
each variable.

``` r
cpi_url <- "https://tablebuilder.singstat.gov.sg/api/table/tabledata/M212881?isTestApi=true&seriesNoORrowNo=1"

raw_cpi <- httr::GET(url = cpi_url)
```

It has returned a list object which we can further explore by calling
`raw_cpi`.

``` r
raw_cpi
```

    ## Response [https://tablebuilder.singstat.gov.sg/api/table/tabledata/M212881?isTestApi=true&seriesNoORrowNo=1]
    ##   Date: 2022-06-22 14:55
    ##   Status: 200
    ##   Content-Type: application/json; charset=utf-8
    ##   Size: 45.1 kB

``` r
names(raw_cpi)
```

    ##  [1] "url"         "status_code" "headers"     "all_headers" "cookies"    
    ##  [6] "content"     "date"        "times"       "request"     "handle"

``` r
head(raw_cpi$content)
```

    ## [1] 7b 22 44 61 74 61

The `Status: 200` tells us that the operation was successful. If it
returned the number 400 or 404, it would mean that there are errors in
our query or the resource could not be found. The `Content-Type` tells
us that the data takes on a JSON format, which is why the `jsonlite`
package was needed.

Using the **`names`** function, we can find out what is in the `raw_cpi`
list. The most important part we need is in the `content`, which is not
useful until we convert it to text in a JSON format. To do this, we need
the **`rawToChar`** in base R and **`fromJSON`** function in the
`jsonlite` package.

``` r
cpi <- jsonlite::fromJSON(rawToChar(raw_cpi$content))

names(cpi)
```

    ## [1] "Data"       "DataCount"  "StatusCode" "Message"

``` r
lapply(cpi, FUN = class)
```

    ## $Data
    ## [1] "list"
    ## 
    ## $DataCount
    ## [1] "integer"
    ## 
    ## $StatusCode
    ## [1] "integer"
    ## 
    ## $Message
    ## [1] "character"

Using the **`names`** function on the resulting `cpi` list, we can find
the elements in the list and we are interested in the `Data` element.
Using the **`lapply`** function and indicating **`FUN = class`**, it
returned the classes of the elements in the list. Let us dig deeper into
the `Data` element.

``` r
names(cpi$Data)
```

    ##  [1] "id"              "title"           "footnote"        "frequency"      
    ##  [5] "datasource"      "generatedBy"     "dataLastUpdated" "dateGenerated"  
    ##  [9] "offset"          "limit"           "sortBy"          "timeFilter"     
    ## [13] "between"         "search"          "row"

``` r
names(cpi$Data$row)
```

    ## [1] "seriesNo" "rowText"  "uoM"      "footnote" "columns"

``` r
head(lapply(cpi$Data$row$columns, dim))
```

    ## [[1]]
    ## [1] 736   2
    ## 
    ## [[2]]
    ## [1] 0 0
    ## 
    ## [[3]]
    ## [1] 0 0
    ## 
    ## [[4]]
    ## [1] 0 0
    ## 
    ## [[5]]
    ## [1] 0 0
    ## 
    ## [[6]]
    ## [1] 0 0

The `Data` list consists of the type of data and some of the values in
each element. The elements `id`, `title`, `frequency`, etc. are the
parameters or metadata from the API query. What we need is the `row`
element. The `seriesNo` and `rowText`represents the class and
sub-classes of items in the CPI basket, for example 1 refers to All
Items, 1.0 refers to Food, and 1.1 refers to Food Excl Food Serving
Services, etc. For our analysis, I am interested in the CPI - All Items,
and due to a parameter in our URL specifying to only return the values
under this category, we only have data for it as can be seen in the
`columns` element. Due to the size of the `columns` list, I had to use
the **`head`** function to simplify the output.

Working with list of lists can be troublesome, so it would a good idea
to convert it into a data frame (at least for now).

``` r
cpi_data <- as.data.frame(cpi$Data$row$columns[[1]])
```

Let us take a look at `cpi_data`.

``` r
stargazer(rbind(head(cpi_data, n = 8), tail(cpi_data, n = 8)),
          type = "text",
          title = "First and Last 8 Values in cpi data",
          summary = F)
```

    ## 
    ## First and Last 8 Values in cpi data
    ## ====================
    ##       key     value 
    ## --------------------
    ## 1   1961 Apr 24.187 
    ## 2   1961 Aug 24.517 
    ## 3   1961 Dec 24.487 
    ## 4   1961 Feb 24.565 
    ## 5   1961 Jan 24.542 
    ## 6   1961 Jul 24.276 
    ## 7   1961 Jun 24.223 
    ## 8   1961 Mar 24.585 
    ## 729 2021 May 101.883
    ## 730 2021 Nov 103.959
    ## 731 2021 Oct 102.95 
    ## 732 2021 Sep 102.657
    ## 733 2022 Apr 106.547
    ## 734 2022 Feb 105.379
    ## 735 2022 Jan 104.472
    ## 736 2022 Mar 106.691
    ## --------------------

We can see that the order of the data is based on the time observations
but on the character values of the dates. To reorder the observations,
we can use the functions in the **`lubridate`** and **`dplyr`**
packages.

``` r
cpi_data <- cpi_data %>% 
  dplyr::arrange(lubridate::ym(cpi_data$key))

stargazer(rbind(head(cpi_data, n = 8), tail(cpi_data, n = 8)),
          type = "text",
          title = "Adjusted First and Last 8 Values in cpi data",
          summary = F)
```

    ## 
    ## Adjusted First and Last 8 Values in cpi data
    ## ====================
    ##       key     value 
    ## --------------------
    ## 1   1961 Jan 24.542 
    ## 2   1961 Feb 24.565 
    ## 3   1961 Mar 24.585 
    ## 4   1961 Apr 24.187 
    ## 5   1961 May 24.053 
    ## 6   1961 Jun 24.223 
    ## 7   1961 Jul 24.276 
    ## 8   1961 Aug 24.517 
    ## 729 2021 Sep 102.657
    ## 730 2021 Oct 102.95 
    ## 731 2021 Nov 103.959
    ## 732 2021 Dec 104.439
    ## 733 2022 Jan 104.472
    ## 734 2022 Feb 105.379
    ## 735 2022 Mar 106.691
    ## 736 2022 Apr 106.547
    ## --------------------

I have simplified the steps for data retrieval and conversion into data
frame for the other variables of interest.

[Domestic Supply Price Index, By Commodity Section (1-Digit Level), Base
Year 2018 = 100](https://tablebuilder.singstat.gov.sg/table/TS/M212701):

``` r
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
```

    ## 
    ## Adjusted First and Last 8 Values in dspi data
    ## ====================
    ##       key     value 
    ## --------------------
    ## 1   1974 Jan 79.774 
    ## 2   1974 Feb 80.715 
    ## 3   1974 Mar 81.278 
    ## 4   1974 Apr  81.56 
    ## 5   1974 May 81.842 
    ## 6   1974 Jun 81.278 
    ## 7   1974 Jul  81.09 
    ## 8   1974 Aug  81.56 
    ## 573 2021 Sep 105.936
    ## 574 2021 Oct 109.692
    ## 575 2021 Nov 109.496
    ## 576 2021 Dec 108.262
    ## 577 2022 Jan 111.692
    ## 578 2022 Feb 115.835
    ## 579 2022 Mar 125.088
    ## 580 2022 Apr 126.759
    ## --------------------

[Composite Leading Index (2015 =
100)](https://tablebuilder.singstat.gov.sg/table/TS/M240421):

``` r
cli_url <- "https://tablebuilder.singstat.gov.sg/api/table/tabledata/M240421?isTestApi=true"

raw_cli <- httr::GET(url = cli_url)

cli <- jsonlite::fromJSON(rawToChar(raw_cli$content))

cli_data <- as.data.frame(cli$Data$row$columns[[1]])

# Data is ordered by the years and quarters so there was no need for reordering

stargazer(rbind(head(cli_data, n = 8), tail(cli_data, n = 8)),
          type = "text",
          title = "First and Last 8 Values in cli data",
          summary = F)
```

    ## 
    ## First and Last 8 Values in cli data
    ## =================
    ##       key   value
    ## -----------------
    ## 1   1978 1Q 25.7 
    ## 2   1978 2Q 26.5 
    ## 3   1978 3Q 27.2 
    ## 4   1978 4Q 26.6 
    ## 5   1979 1Q 27.8 
    ## 6   1979 2Q 27.4 
    ## 7   1979 3Q 27.4 
    ## 8   1979 4Q 28.3 
    ## 170 2020 2Q 100.6
    ## 171 2020 3Q 107.4
    ## 172 2020 4Q  111 
    ## 173 2021 1Q 110.9
    ## 174 2021 2Q 112.1
    ## 175 2021 3Q  112 
    ## 176 2021 4Q 112.9
    ## 177 2022 1Q 112.3
    ## -----------------

## 4 Exploratory Data Analysis

In this section, I provided more details about each variable and
generated univariate and multivariate plots to find patterns in and
interaction between the variables. To visualize the plots, rather than
using a data frame object, it is best to convert them into an `xts`
object. The conversion requires two arguments. First is the vector or
matrix to be converted to xts object and a vector of the date. We have
the dates in the data frame but they are not formatted as dates, so I
used the **`yearmon`** and **`yearqtr`** functions to format them to the
appropriate types.

``` r
cpi.xts <- xts(x = cpi_data$value, 
               order.by = as.yearmon(cpi_data$key, format = "%Y %b")) %>%
  `colnames<-`("CPI")

dspi.xts <- xts(x = dspi_data$value, 
                order.by = as.yearmon(dspi_data$key, format = "%Y %b")) %>%
  `colnames<-`("DSPI")

cli.xts <- xts(x = cli_data$value, 
               order.by = as.yearqtr(gsub("Q", "", cli_data$key), format = "%Y %q")) %>%
  `colnames<-`("CLI")
```

Since the values were stored as characters, I converted them to numeric
so that it can be properly analyzed. Unlike the normal data frame, we
cannot use **`as.numeric`** function to convert the variable type to
numeric. Instead, we need to use **`storage.mode`**.

``` r
storage.mode(cpi.xts) <- "numeric"

storage.mode(dspi.xts) <- "numeric"

storage.mode(cli.xts) <- "numeric"
```

### 4.1 Consumer Price Index

One of the widely-known and most watched indicator is the CPI as it can
be used to determine the general inflation in a country. Inflation
basically refers to the increase in general price levels of goods and
services. Deflation (or negative inflation) would mean that the general
price level of goods and services has fallen. The CPI measures the
average price level of a basket of goods and services consumed by
households, and the basket of goods is updated every five years in
Singapore to better reflect the changing consumption habits. Due to the
importance of the indicator, many economists in government or private
business settings attempt to forecast it to formulate economic policies
or business/investment decisions.

`cpi.xts` contained 736 monthly observations from January 1961 to April

1.  However, I would need to adjust this variable as the Composite
    Leading Index starts from 1978 and it is a quarterly series. For
    time series analysis to work, we need the variables to have same
    time interval.

``` r
cpi_q.xts <- xts::apply.quarterly(cpi.xts, FUN = mean)

# Changing the date index to an appropriate format
tclass(cpi_q.xts) <- "yearqtr"
```

``` r
#Subset cpi_q to match the cli data

cpi_q.xts <- cpi_q.xts["1978-01/2022-03"]
```

I have plotted the cleaned `cpi_q.xts`, which shows how the CPI has
changed over time. We can see that the series is non-stationary, which
could probably be solved by taking the first difference on the data.
More about this in the next section.

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot cpi-1.png" style="display: block; margin: auto;" />

I have also generated a boxplot of the range of CPI values of all years
grouped by the quarters. It is hard to say if there are any differences
between the quarters to indicate an effect of seasonality. However,
since the data obtained was not seasonally-adjusted, we might want to
consider adding seasonal dummies to our time series model. However,
since seasonality is not the main purpose of this project, we may treat
this as a factor for discussion in future research.

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/seasonality in cpi-1.png" style="display: block; margin: auto;" />

### 4.2 Domestic Supply Price Index

There are 3 different measurements of the PPI in Singapore, namely the
Domestic Supply Price Index, the Singapore Manufactured Products Price
Index and the Services Producer Price Index. Here are the descriptions
of each index:

| Index | Description                                                                                                                                                                                                                                                             |
|-------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| DSPI  | Monitors the price changes of locally manufactured goods and imports which are retained for use in the domestic economy. It gives an indication of the price trends of goods used in the domestic economy.                                                              |
| SMPPI | Measures the changes in the prices of goods produced by manufacturers in Singapore for sale in the domestic and international markets.                                                                                                                                  |
| SPPI  | Contains indices of different services: Accounting Services Price Index, Cargo Handling Price Index, Computer Consultancy and Information Services Price Index, Freight Forwarding Price Index, Sea Freight Transport Price Index, Warehousing and Storage Price Index. |

The index I am interested in is the DSPI since it measures the prices of
goods used in the domestic economy, including goods that were imported.
It may give a better measure of the PPI, since these are the costs
affecting the domestic manufacturers and would flow down the supply
chain and affect the domestic end-consumers. We would expect that if PPI
increases, CPI would increase as well.

Similar to `cpi.xts`, `dspi.xts` 580 monthly observations from January
1974 to April 2022. We would need to adjust the time interval to match
the CLI data.

``` r
dspi_q.xts <- xts::apply.quarterly(dspi.xts, FUN = mean)

tclass(dspi_q.xts) <- "yearqtr"

dspi_q.xts <- dspi_q.xts["1978-01/2022-03"]
```

Based on the plot of `dspi_q.xts`, the Singapore DSPI has ranged from a
minimum of about 80 to a maximum of about 130. This is unlike the CPI
chart that we saw earlier that had a clear upward trend.

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot dspi-1.png" style="display: block; margin: auto;" />

Looking at the boxplot of the `dspi_q.xts` grouped by quarters, we can
see some variation across quarters, which may indicate that seasonality
is present. Based on the boxplot, the DSPI is generally lower in fourth
quarter of each year than in other quarters.

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/seasonality in dspi-1.png" style="display: block; margin: auto;" />

### 4.3 Composite Leading Index

The CLI is a leading indicator to predict market expansions and
slowdowns. It aggregates the following nine indicators:

-   Total New Companies Formed
-   Money Supply (M2)
-   Stock Exchange of Singapore Indices
-   Business Expectations for Wholesale Trade
-   Business Expectations for Stock of Finished Goods (Manufacturing)
-   US Purchasing Managers’ Index (Manufacturing)
-   Total Non-oil Seaborne Cargo Handled
-   Domestic Liquidity Indicator
-   Total Non-oil Retained Imports

We could expect that if the CLI indicates a market upturn(downturn), CPI
would increase(decrease).

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot cli-1.png" style="display: block; margin: auto;" />

### 4.5 Multivariate Analysis

Before conducting a multivariate analysis, it would be better to merge
our data into a single object. To do this, we need the **`merge`**
function. However, as we can see there seems to be a problem, as there
are two rows for each quarter in the year. This is because CPI and DSPI
considered the quarter to be on March, June, September and December due
to the adjustment we did previously.

``` r
data <- merge(cpi_q.xts, dspi_q.xts, cli.xts)

stargazer(head(data, n = 8),
          type = "text",
          title = "Unadjusted data",
          rownames = T)
```

    ## 
    ## Unadjusted data
    ## ===============================
    ##             CPI    DSPI   CLI  
    ## -------------------------------
    ## X1978.Q1                 25.700
    ## X1978.Q1.1 44.057 88.732       
    ## X1978.Q2                 26.500
    ## X1978.Q2.1 44.160 89.923       
    ## X1978.Q3                 27.200
    ## X1978.Q3.1 44.975 91.018       
    ## X1978.Q4                 26.600
    ## X1978.Q4.1 45.098 91.458       
    ## -------------------------------

To resolve this, we can use the **`na.locf`** function to replace the
NAs with the last observation. We can do this on the column, and use the
**`na.omit`** function to remove rows with NAs in the CPI and DSPI
columns. This would remove the duplication of the quarters in each year.

``` r
data[, 3] <- na.locf(data[, 3])

data <- na.omit(data)

stargazer(head(data, n = 8),
          type = "text",
          title = "Adjusted data",
          rownames = T)
```

    ## 
    ## Adjusted data
    ## =============================
    ##          CPI    DSPI    CLI  
    ## -----------------------------
    ## 1978 Q1 44.057 88.732  25.700
    ## 1978 Q2 44.160 89.923  26.500
    ## 1978 Q3 44.975 91.018  27.200
    ## 1978 Q4 45.098 91.458  26.600
    ## 1979 Q1 45.052 93.869  27.800
    ## 1979 Q2 45.371 100.885 27.400
    ## 1979 Q3 46.919 107.525 27.400
    ## 1979 Q4 48.089 110.782 28.300
    ## -----------------------------

``` r
dim(data)
```

    ## [1] 177   3

So now we have 177 quarterly observations and 4 variables, which is
correct. I have plotted a scatterplot matrix of the variables in `data`.
We can see a clear pattern between CPI and CLI, but not so with DSPI.
However, this is because we could see a clear upward trend in CPI and
CLI when plotted on a chart, but DSPI had a ranging pattern. The
correlation matrix plot also shows how the variables are correlated.

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/scatplot data-1.png" style="display: block; margin: auto;" />

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/corrplot data-1.png" style="display: block; margin: auto;" />

## 5 Splitting Data Into Training and Testing Sets.

Before we continue with any serious analysis, it would be better to
split our data into training and testing sets. The training set will be
used to create our models, while the testing set will be used to
evaluate our models on out-of-sample prediction. Unlike cross-sectional
data, the splitting of the data can only happen at a certain observation
and not randomly selected to preserve the stochastic process of the
observations.

``` r
train <- data["/2019"]

test <- data["2020/"]
```

## 6 Unit Root Test for Stationarity

Now that we have a better understanding of our variables, we can proceed
with the actual time series analysis. However, the first thing to do
before modelling is to check that the variables are stationary.

Stationarity of variables is an important condition in most time series
models as regression with non-stationary variables can lead to spurious
results. Time series are said to be stationary if they have a constant
mean, constant variance and the covariance between observations
![h](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;h "h")
periods apart depends on
![h](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;h "h").
This type of stationarity is called covariance (weak) stationarity.

A common test for stationarity is the Augmented Dickey-Fuller (ADF) unit
root test. The null hypothesis claims that there is unit root in the
series and hence it is non-stationary, while the alternative hypothesis
claims that there is no unit root in the series. An alternative is the
Phillips-Perron (PP) unit root test, which has the same null and
alternative hypothesis as the ADF test.
Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test is a test to check for the
stationarity of time series variables. KPSS test is commonly used in
conjunction with the ADF or PP test. The the hypotheses of KPSS test is
opposite to that of ADF and PP test, with the null claiming that the
time series is stationary with deterministic trend and the alternative
claiming that it is not stationary.

If a series is non-stationary, it is common to take the first difference
(i.e. ![y_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y_t "y_t") -
![y\_{t-1}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y_%7Bt-1%7D "y_{t-1}"))
or to de-trend a trend stationary series. If a series is stationary
after taking the first difference, we now have an I(1) series or we can
say that the variable is integrated of order 1. The original series (or
at levels) is I(0), if stationary.

I performed the ADF and KPSS test on the four variables in the `data`
object, firstly on the variables at levels then at first differences,
using the **`ur.df`** and **`ur.kpss`** function in the **`urca`**
package.

``` r
# Indicate type = "trend" as there was trend in the series based on plot
# Indicate selectlags = "AIC" to select lags based on Akaike IC

train$CPI %>%
  ur.df(type = "trend", selectlags = "AIC") %>%
  summary()
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression trend 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.74847 -0.21189 -0.03358  0.16545  1.63360 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.347931   0.556296   2.423   0.0165 *  
    ## z.lag.1     -0.024060   0.011782  -2.042   0.0428 *  
    ## tt           0.007222   0.003853   1.874   0.0627 .  
    ## z.diff.lag   0.476250   0.068665   6.936 9.15e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4531 on 162 degrees of freedom
    ## Multiple R-squared:  0.2431, Adjusted R-squared:  0.2291 
    ## F-statistic: 17.34 on 3 and 162 DF,  p-value: 8.165e-10
    ## 
    ## 
    ## Value of test-statistic is: -2.042 7.6047 2.3171 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau3 -3.99 -3.43 -3.13
    ## phi2  6.22  4.75  4.07
    ## phi3  8.43  6.49  5.47

The **`summary`** function returns the full output of the ADF test, but
we only need the bottom section where the test statistics and the
critical values are. The first test statistic, for `tau3`, is higher
than the 5% critical value (-2.042 \> -3.43) but we need it to be lower
than the critical value to reject the null (ADF is a one-sided test,
where the alternative is \< 0). Therefore, the CPI series contains unit
root. The `phi3` value (third value in the test statistic) has the null
hypothesis that there is unit root and no time trend (joint test). The
`phi2` value (second value in the test statistic) has the null
hypothesis that there is unit root, no drift and no time trend (joint
test). We rejected `phi2` based on the 5% critical value, which means
that either one, two or all three coefficients are not 0.

``` r
# Indicate type = "tau" for trend

train$CPI %>%
  ur.kpss(type = "tau" , lags = "short") %>%
  summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: tau with 4 lags. 
    ## 
    ## Value of test-statistic is: 0.2947 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.119 0.146  0.176 0.216

The KPSS test rejected the null hypothesis (test statistic 0.2947 \> 5%
critical value 0.146), which means there is unit root in the CPI series.

``` r
# Indicate type = "drift" as DSPI as there is no particular long term trend

train$DSPI %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression drift 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -25.4510  -1.6328   0.0294   1.7575   9.6201 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.88039    2.45616   2.801  0.00571 ** 
    ## z.lag.1     -0.06626    0.02358  -2.810  0.00556 ** 
    ## z.diff.lag   0.29399    0.07460   3.941  0.00012 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.576 on 163 degrees of freedom
    ## Multiple R-squared:  0.1124, Adjusted R-squared:  0.1015 
    ## F-statistic: 10.32 on 2 and 163 DF,  p-value: 6.047e-05
    ## 
    ## 
    ## Value of test-statistic is: -2.8099 3.9513 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau2 -3.46 -2.88 -2.57
    ## phi1  6.52  4.63  3.81

``` r
# Indicate type = "mu" for drift

train$DSPI %>%
  ur.kpss(type = "mu", lags = "short") %>%
  summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 4 lags. 
    ## 
    ## Value of test-statistic is: 0.4165 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

For the DSPI series, we can see contradicting results where the ADF test
does not reject the null of unit root, but the KPSS test does not reject
the null of stationarity. This may happen due to insufficient
observations, but let us assume that there is unit root in this series.

``` r
# Indicate type = "trend"  since there was a clear trend in CLI plot

train$CLI %>%
  ur.df(type = "trend", selectlags = "AIC") %>%
  summary()
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression trend 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.7191 -0.7875  0.0243  1.0158  4.3514 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.71316    0.91401   5.157 7.26e-07 ***
    ## z.lag.1     -0.18384    0.03737  -4.919 2.12e-06 ***
    ## tt           0.09428    0.01947   4.842 2.99e-06 ***
    ## z.diff.lag   0.42112    0.07214   5.837 2.81e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.517 on 162 degrees of freedom
    ## Multiple R-squared:  0.2223, Adjusted R-squared:  0.2079 
    ## F-statistic: 15.44 on 3 and 162 DF,  p-value: 7.018e-09
    ## 
    ## 
    ## Value of test-statistic is: -4.9194 10.2782 12.1472 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau3 -3.99 -3.43 -3.13
    ## phi2  6.22  4.75  4.07
    ## phi3  8.43  6.49  5.47

``` r
train$CLI %>%
  ur.kpss(type = "tau", lags = "short") %>%
  summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: tau with 4 lags. 
    ## 
    ## Value of test-statistic is: 0.0876 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.119 0.146  0.176 0.216

We rejected the null in the ADF test, and do not reject the null in the
KPSS test. This indicated that CLI has a deterministic trend, which can
be removed to make the series stationary. However, for simplicity sake,
I would use the first difference (although there are arguments that this
is incorrect and leads to spurious analysis).

``` r
# By default, the diff function uses lag = 1 and differences = 1

diff.data <- diff(data) %>%
  `colnames<-`(c("dCPI", "dDSPI", "dCLI"))

head(diff.data) # First observation removed because we cannot take difference on it. 
```

    ##               dCPI     dDSPI dCLI
    ## 1978 Q1         NA        NA   NA
    ## 1978 Q2  0.1026667 1.1906667  0.8
    ## 1978 Q3  0.8150000 1.0956667  0.7
    ## 1978 Q4  0.1226667 0.4393333 -0.6
    ## 1979 Q1 -0.0460000 2.4113333  1.2
    ## 1979 Q2  0.3190000 7.0163333 -0.4

``` r
diff.data <- na.omit(diff.data)

# Split the train and test sets again

diff.train <- diff.data["/2019"]

diff.test <- diff.data["2020/"]
```

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot diff data-1.png" style="display: block; margin: auto;" />

The following R chunks performs the ADF test on the first-differenced
variables. Because we have taken the first difference, there is no need
to include trend option in ADF test.

``` r
# After differencing, there should be no deterministic trend in the plots, but it may contain drift terms (especially dealing with non-zero means)
# KPSS test null hypothesis is that variables are stationary with deterministic parts, so just indicate type = "mu"

diff.train$dCPI %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression drift 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.91937 -0.22442 -0.02093  0.17146  1.40266 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.14829    0.04435   3.343  0.00103 ** 
    ## z.lag.1     -0.44925    0.07963  -5.642 7.34e-08 ***
    ## z.diff.lag  -0.14575    0.07742  -1.883  0.06156 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4522 on 162 degrees of freedom
    ## Multiple R-squared:  0.2802, Adjusted R-squared:  0.2714 
    ## F-statistic: 31.54 on 2 and 162 DF,  p-value: 2.706e-12
    ## 
    ## 
    ## Value of test-statistic is: -5.6419 15.9215 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau2 -3.46 -2.88 -2.57
    ## phi1  6.52  4.63  3.81

``` r
diff.train$dCPI %>%
  ur.kpss(type = "mu", lags = "short") %>%
  summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 4 lags. 
    ## 
    ## Value of test-statistic is: 0.123 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
diff.train$dDSPI %>%
  ur.df(type = "none", selectlags = "AIC") %>%
  summary()
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression none 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 - 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -25.4100  -1.4989  -0.0343   2.0070  10.5239 
    ## 
    ## Coefficients:
    ##            Estimate Std. Error t value Pr(>|t|)    
    ## z.lag.1    -0.89788    0.09277  -9.679   <2e-16 ***
    ## z.diff.lag  0.21854    0.07642   2.860   0.0048 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.572 on 163 degrees of freedom
    ## Multiple R-squared:  0.3986, Adjusted R-squared:  0.3912 
    ## F-statistic: 54.02 on 2 and 163 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Value of test-statistic is: -9.6789 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau1 -2.58 -1.95 -1.62

``` r
diff.train$dDSPI %>%
  ur.kpss(type = "mu", lags = "short") %>%
  summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 4 lags. 
    ## 
    ## Value of test-statistic is: 0.092 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
diff.train$dCLI %>%
  ur.df(type = "drift", selectlags = "AIC") %>%
  summary()
```

    ## 
    ## ############################################### 
    ## # Augmented Dickey-Fuller Test Unit Root Test # 
    ## ############################################### 
    ## 
    ## Test regression drift 
    ## 
    ## 
    ## Call:
    ## lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.0845 -0.8252  0.0947  0.8918  4.6850 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.35903    0.13266   2.706  0.00753 ** 
    ## z.lag.1     -0.77113    0.09053  -8.518 1.06e-14 ***
    ## z.diff.lag   0.14386    0.07797   1.845  0.06685 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.61 on 162 degrees of freedom
    ## Multiple R-squared:  0.3495, Adjusted R-squared:  0.3415 
    ## F-statistic: 43.52 on 2 and 162 DF,  p-value: 7.474e-16
    ## 
    ## 
    ## Value of test-statistic is: -8.5177 36.2797 
    ## 
    ## Critical values for test statistics: 
    ##       1pct  5pct 10pct
    ## tau2 -3.46 -2.88 -2.57
    ## phi1  6.52  4.63  3.81

``` r
diff.train$dCLI %>%
  ur.kpss(type = "mu", lags = "short") %>%
  summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 4 lags. 
    ## 
    ## Value of test-statistic is: 0.0328 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

Since the ADF tau-statistic for all variables are less than the critical
tau-value, we can reject the null hypothesis and conclude that the
series are I(1) variables (stationary at first difference).

After taking the first difference, we can see that the interaction
between variables have changed in the scatterplot matrix and the
correlation matrix. This is why stationarity is an important condition
as variables may seem to have a relationship at levels but after taking
the difference, there may not be a relationship between variables as
seen in the scatter plot below.

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/scatplot diff train-1.png" style="display: block; margin: auto;" />

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/corrplot diff train-1.png" style="display: block; margin: auto;" />

## 7 Model Selection

In this section, I discussed the different possible time series models
that can be created from our data.

### 7.1 Autoregressive (Integrated) Moving Average Model

The AR(I)MA model consists of two parts, an autoregressive (AR) model
and a moving average (MA) model. Integrated refers to the number of
differencing needed to achieve stationary time series.

An AR(p) model means that
![y_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y_t "y_t")
depends on p lags of
![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y").
In other words, we believe that up to the
![p](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;p "p")
historical value of
![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y")
can explain
![y_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y_t "y_t").
The function is written as:

![
y_t = \\alpha + \\sum\_{i = 1}^p \\alpha_i y\_{t-i} + u_t
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Ay_t%20%3D%20%5Calpha%20%2B%20%5Csum_%7Bi%20%3D%201%7D%5Ep%20%5Calpha_i%20y_%7Bt-i%7D%20%2B%20u_t%0A "
y_t = \alpha + \sum_{i = 1}^p \alpha_i y_{t-i} + u_t
")

A MA(q) model means that
![y_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y_t "y_t")
depends on q lags of its error terms. In other words, we believe that up
to
![q](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;q "q")
historical value of
![u](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;u "u")
(the error term) can explain
![y_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y_t "y_t").
The function is written as:

![
y_t = \\alpha + u_t + \\sum\_{j=1}^q \\beta_j u\_{t-j}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Ay_t%20%3D%20%5Calpha%20%2B%20u_t%20%2B%20%5Csum_%7Bj%3D1%7D%5Eq%20%5Cbeta_j%20u_%7Bt-j%7D%0A "
y_t = \alpha + u_t + \sum_{j=1}^q \beta_j u_{t-j}
")

An ARMA(p, q) model the combination of AR(p) and MA(q) models. The
function is written as:

![
y_t = \\alpha + \\sum\_{i = 1}^p \\alpha_i y\_{t-i} + u_t + \\sum\_{j=1}^q \\beta_j u\_{t-j}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Ay_t%20%3D%20%5Calpha%20%2B%20%5Csum_%7Bi%20%3D%201%7D%5Ep%20%5Calpha_i%20y_%7Bt-i%7D%20%2B%20u_t%20%2B%20%5Csum_%7Bj%3D1%7D%5Eq%20%5Cbeta_j%20u_%7Bt-j%7D%0A "
y_t = \alpha + \sum_{i = 1}^p \alpha_i y_{t-i} + u_t + \sum_{j=1}^q \beta_j u_{t-j}
")

This can be written simply as ARIMA(p, d, q). In Section 6, I had tested
that the CPI series is an I(1) series, which means it is stationary at
first difference. Therefore, we have an ARIMA(p, 1, q) model. The p and
q lags are determined in various ways. Firstly, it is common to use the
autocorrelation and partial autocorrelation function to determine the
lag order. The functions **`Acf`** and **`Pacf`** in the **`forecast`**
package can plot the ACF and PACF.

``` r
par(mfrow = c(2,1), mar = c(3, 3, 4, 2))

forecast::Acf(x = diff.train$dCPI, main = "ACF of dCPI")

forecast::Pacf(x = diff.train$dCPI, main = "PACF of dCPI")
```

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/acf pacf-1.png" style="display: block; margin: auto;" />

We can gauge/estimate the q lags of the error term using the ACF where
spikes are above the blue dotted line and similarly, p lags of the
dependent variable using the PACF. Usually, the first spike in the ACF
plot refers to lag 0, but the **`forecast`** package removes and we can
use the first spike as lag 1 instead. In this case, we might want to try
to fit a ARIMA(1, 1, 4) model using the **`Arima`** function in
**`forecast`** package.

``` r
# order refers to the (p,d,q) of the ARIMA model
# d = 0 as we are using the differenced data
# method for estimation is to use maximum likelihood

arima1 <- forecast::Arima(diff.train$dCPI, 
                          order = c(1, 0, 4),
                          method = "ML")

summary(arima1)
```

    ## Series: diff.train$dCPI 
    ## ARIMA(1,0,4) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ma1      ma2      ma3     ma4    mean
    ##       0.7090  -0.3086  -0.0016  -0.0469  0.0463  0.3328
    ## s.e.  0.3641   0.3764   0.1639   0.1440  0.0993  0.0809
    ## 
    ## sigma^2 = 0.2071:  log likelihood = -102.57
    ## AIC=219.15   AICc=219.85   BIC=240.98
    ## 
    ## Training set error measures:
    ##                        ME      RMSE       MAE       MPE     MAPE      MASE
    ## Training set 0.0008717552 0.4468077 0.3000226 -498.1928 712.1803 0.6590941
    ##                      ACF1
    ## Training set 0.0004936192

Another method that gets us the same results would be to use the data at
levels but indicate `c(1, 1, 4)` in the `order` argument. The benefit of
using this method is that the fitted values obtained from the model is
automatically corrected to values at levels, instead of first
difference. However, other functions used in further sections are based
on differenced data as input since we cannot specify a differencing
argument in the functions. Therefore, for better comparison, I have
decided to use the differenced data here as well.

The output shows the coefficients of the lagged variables in the ARIMA
model. It should be noted that the mean value is the mean of the series
(not the sample mean, but calculated with the log-likelihood). To
convert to the constant value, we multiply the mean by
![(1-0.7090)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%281-0.7090%29 "(1-0.7090)"),
which is basically coefficient of the AR terms.

We can write our model as:

![
y_t = 0.097 + 0.7090y\_{t-1} - 0.3086u\_{t-1} - 0.0016u\_{t-2} - 0.0469u\_{t-3} + 0.0463u\_{t-4}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Ay_t%20%3D%200.097%20%2B%200.7090y_%7Bt-1%7D%20-%200.3086u_%7Bt-1%7D%20-%200.0016u_%7Bt-2%7D%20-%200.0469u_%7Bt-3%7D%20%2B%200.0463u_%7Bt-4%7D%0A "
y_t = 0.097 + 0.7090y_{t-1} - 0.3086u_{t-1} - 0.0016u_{t-2} - 0.0469u_{t-3} + 0.0463u_{t-4}
")

How well does our manually selected model compare against a model
selected automatically using the lowest Akaike Information Criterion? We
can estimate such a model using the **`auto.arima`** function. We have
many arguments that we should include: `seasonal` should be set to False
since I am not estimating a seasonal ARIMA, `stepwise` and
`approximation` should be set to False to obtain a more accurate model
selection, `ic` set to AIC, and `trace` set to False to suppress the
running of the models by the automatic selection.

``` r
arima2 <- forecast::auto.arima(diff.train$dCPI, 
                               seasonal = F, 
                               ic = "aic", 
                               stepwise = F, 
                               approximation = F,
                               trace = F,
                               method = "ML")

summary(arima2)
```

    ## Series: diff.train$dCPI 
    ## ARIMA(2,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##           ar1     ar2     ma1    mean
    ##       -0.3984  0.4993  0.8490  0.3351
    ## s.e.   0.0922  0.0675  0.0793  0.0699
    ## 
    ## sigma^2 = 0.2001:  log likelihood = -100.83
    ## AIC=211.66   AICc=212.03   BIC=227.25
    ## 
    ## Training set error measures:
    ##                        ME      RMSE       MAE       MPE   MAPE      MASE
    ## Training set 0.0004816811 0.4419613 0.3014915 -538.2033 780.46 0.6623209
    ##                     ACF1
    ## Training set -0.01233133

We can see that the model automatically selected using the lowest AIC is
the different from the one I had manually coded into the **`Arima`**
function. The `arima2` model had a lower AIC and had a slightly higher
log-likelihood than the `arima1` model. I plotted the actual CPI values
and the fitted values based on the two ARIMA models to visualize how
well the model fits.

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot arima-1.png" style="display: block; margin: auto;" />

It seems that the `arima1` and `arima2` models approximately captures
the pattern (with slight differences), although it does not seem to
capture the magnitude of the movements in the CPI that well. ARIMA
models can give us a benchmark to compare other types of time series
models, either through in-sample prediction or out-of-sample forecast.

### 7.2 Autoregressive Distributed Lag Models

An ARDL model is similar to an ARMA model, except it used the lags of
the dependent variables and lags of other explanatory variables to
predict
![y_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y_t "y_t").
ARDL can be used on time series with combinations of I(0) and I(1).

The simplest ARDL model is one with an explanatory
(![x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x "x"))
and a dependent variable
(![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y"))
and the notation for this is ARDL(p, q). Unlike the ARIMA models, an
ARDL model allows us to capture the dynamic effects from
![x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x "x")
to
![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y").
We can write the model as such:

![
y_t = \\alpha + \\sum\_{i=1}^p \\alpha_i y\_{t-i} + \\sum\_{j=0}^q \\beta_i x\_{t-i} + \\epsilon_t
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0Ay_t%20%3D%20%5Calpha%20%2B%20%5Csum_%7Bi%3D1%7D%5Ep%20%5Calpha_i%20y_%7Bt-i%7D%20%2B%20%5Csum_%7Bj%3D0%7D%5Eq%20%5Cbeta_i%20x_%7Bt-i%7D%20%2B%20%5Cepsilon_t%0A "
y_t = \alpha + \sum_{i=1}^p \alpha_i y_{t-i} + \sum_{j=0}^q \beta_i x_{t-i} + \epsilon_t
")

In our case, we have more than 1 explanatory variables but the same
concept applies. We can use the **`auto_ardl`** function in the
**`ARDL`** package to automatically select an ARDL model that has the
lowest AIC based on a maximum number of lags for the search.

``` r
# max_order = 4 to indicate maximum lag in the search for all variables is 4
# possible to state a vector of length equal to no. of variables in max_order
# grid = T to to prevent stepwise search of models

ardl_lag <- ARDL::auto_ardl(formula = dCPI ~ dDSPI + dCLI,
                            data = as.zoo(diff.train),
                            max_order = 4, grid = T,
                            selection = "AIC")

# Obtain the best lag order, arranged by how variables were entered in the formula argument
ardl_lag$best_order
```

    ## [1] 4 1 2

``` r
# Save the coefficients of the model selected by auto_ardl
ardl <- ardl_lag$best_model

summary(ardl)
```

    ## 
    ## Time series regression with "zoo" data:
    ## Start = 1979 Q2, End = 2019 Q4
    ## 
    ## Call:
    ## dynlm::dynlm(formula = full_formula, data = data, start = start, 
    ##     end = end)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.96830 -0.19791  0.02277  0.18684  1.22282 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.096906   0.046950   2.064  0.04070 *  
    ## L(dCPI, 1)   0.257084   0.077079   3.335  0.00107 ** 
    ## L(dCPI, 2)   0.192362   0.074942   2.567  0.01122 *  
    ## L(dCPI, 3)   0.012879   0.075414   0.171  0.86463    
    ## L(dCPI, 4)   0.175960   0.069773   2.522  0.01270 *  
    ## dDSPI        0.039020   0.009285   4.203 4.47e-05 ***
    ## L(dDSPI, 1)  0.027543   0.009935   2.772  0.00626 ** 
    ## dCLI        -0.040132   0.021843  -1.837  0.06811 .  
    ## L(dCLI, 1)   0.056782   0.022624   2.510  0.01312 *  
    ## L(dCLI, 2)   0.032457   0.020753   1.564  0.11989    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3893 on 153 degrees of freedom
    ## Multiple R-squared:  0.467,  Adjusted R-squared:  0.4357 
    ## F-statistic:  14.9 on 9 and 153 DF,  p-value: < 2.2e-16

Unlike the ARIMA output earlier, the ARDL output does not state the
log-likelihood or the AIC values. This can be found using the
**`logLik`** and **`AIC`** functions in base R.

``` r
logLik(ardl)
```

    ## 'log Lik.' -72.3627 (df=11)

``` r
AIC(ardl)
```

    ## [1] 166.7254

The log-likelihood is much higher and AIC much lower than the `arima2`
model estimated in Section 9.1, which should indicate a better fit.

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot ardl-1.png" style="display: block; margin: auto;" />

We can see a significant improvement in the fit of the `ardl1` model as
compared to the `arima2` model in Section 9.1.

### 7.3 Vector Autoregressive Model

A VAR model allows us to model the influence of two or more time series
on one another, meaning that these variables are endogenous. In VAR
models, the variables
(e.g. ![x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x "x")
and
![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y"))
are simultaneously determined by the lags of these variables. A VAR(p)
model of two variables can be written as:

$$ y_t = beta\_{10} + beta\_{11} y\_{t-1} + + beta\_{1p} y\_{t-p} +
gamma\_{11} x\_{t-1} + + gamma\_{1p} x\_{t-p} + u\_{1t} \\

x_t = beta\_{20} + beta\_{21} y\_{t-1} + + beta\_{2p} y\_{t-p} +
gamma\_{21} x\_{t-1} + + gamma\_{2p} x\_{t-p} + u\_{2t} $$

We can see that with more lags and more variables included into the VAR
system, there would be more unknown parameters and lesser degrees of
freedom, which can affect estimation and statistical tests. However, if
too few lags were included, we may encounter the problem of
autocorrelated errors.

To build a VAR model, we first need to determine the optimal number of
lags which can be done by using the **`VARselect`** function in the
**`vars`** package.

``` r
var_lag <- vars::VARselect(y = diff.train,
                           lag.max = 4,
                           type = "const")

var_lag
```

    ## $selection
    ## AIC(n)  HQ(n)  SC(n) FPE(n) 
    ##      2      2      1      2 
    ## 
    ## $criteria
    ##               1        2        3        4
    ## AIC(n) 1.650474 1.573553 1.615502 1.616389
    ## HQ(n)  1.742942 1.735373 1.846673 1.916911
    ## SC(n)  1.878234 1.972135 2.184904 2.356612
    ## FPE(n) 5.209601 4.824523 5.032743 5.040008

Based on AIC, I would choose to use 2 lag for the VAR model, i.e. VAR(2)
model. We could use the SC (or BIC) selected lags for a parsimonious
model. The function **`VAR`** in the **`vars`** package allows us to
estimate this model.

``` r
var <- vars::VAR(y = diff.train,
                 p = 2,
                 type = "const")

stargazer(var$varresult, 
          type = "text", 
          title = "VAR Estimation Result for dCPI, dDSPI, dCLI", 
          digits = 3, 
          column.labels = c("dCPI", "dDSPI", "dCLI"),
          dep.var.labels.include = F)
```

    ## 
    ## VAR Estimation Result for dCPI, dDSPI, dCLI
    ## ============================================================
    ##                                     Dependent variable:     
    ##                                -----------------------------
    ##                                   dCPI      dDSPI     dCLI  
    ##                                   (1)        (2)      (3)   
    ## ------------------------------------------------------------
    ## dCPI.l1                         0.268***   -0.393    -0.051 
    ##                                 (0.080)    (0.694)  (0.301) 
    ##                                                             
    ## dDSPI.l1                        0.041***  0.342***   -0.039 
    ##                                 (0.010)    (0.088)  (0.038) 
    ##                                                             
    ## dCLI.l1                          0.039*    -0.079   0.346***
    ##                                 (0.023)    (0.196)  (0.085) 
    ##                                                             
    ## dCPI.l2                         0.261***    0.385    -0.381 
    ##                                 (0.075)    (0.652)  (0.283) 
    ##                                                             
    ## dDSPI.l2                         -0.004   -0.276*** -0.086**
    ##                                 (0.010)    (0.091)  (0.039) 
    ##                                                             
    ## dCLI.l2                          0.032     0.358*    -0.061 
    ##                                 (0.023)    (0.196)  (0.085) 
    ##                                                             
    ## const                           0.120***   -0.103   0.484***
    ##                                 (0.046)    (0.396)  (0.172) 
    ##                                                             
    ## ------------------------------------------------------------
    ## Observations                      165        165      165   
    ## R2                               0.382      0.133    0.200  
    ## Adjusted R2                      0.358      0.100    0.170  
    ## Residual Std. Error (df = 158)   0.414      3.589    1.558  
    ## F Statistic (df = 6; 158)      16.250***  4.030***  6.590***
    ## ============================================================
    ## Note:                            *p<0.1; **p<0.05; ***p<0.01

The output shows the three equations, one for each variable, with 2 lags
of each variable. Therefore, the model gets really big as more variables
and lags are added, which can become a problem. We can write it down as:

$$
$$

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot var dcpi-1.png" style="display: block; margin: auto;" />

An advantage of models like VAR is that we can also plot the predicted
values of other variables that are in the model.

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot var ddspi-1.png" style="display: block; margin: auto;" />

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot var dcli-1.png" style="display: block; margin: auto;" />

## 8 Granger Causality Test

Using the Granger Causality Test, we can statistically determine if a
series can be used predict values of another series better than solely
using its past values. Despite the name, it should not be used to
determine true cause-and-effect relationships between variables. It can
be a useful test to determine if variables should be added to a time
series model to improve forecasting accuracy.

In simple terms, the Granger Causality Test checks if the lags of
variable
![x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x "x")
can be used to improve the forecast of variable 2 than solely basing on
the lags of variable
![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y").
What the test does is to apply a Likelihood Ratio Test or a F-Test on
the two models below. The null hypothesis is that
![x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x "x")
does not granger-cause
![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y"),
against the alternative that
![x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x "x")
granger-causes
![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y").

![
\\begin{aligned}
y_t &= \\theta_0 + \\theta_1 y\_{t-1} + \\gamma_1 x\_{t-1} + e_t \\\\
y_t &= \\theta_0 + \\theta_1 y\_{t-1} + e_t
\\end{aligned}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Baligned%7D%0Ay_t%20%26%3D%20%5Ctheta_0%20%2B%20%5Ctheta_1%20y_%7Bt-1%7D%20%2B%20%5Cgamma_1%20x_%7Bt-1%7D%20%2B%20e_t%20%5C%5C%0Ay_t%20%26%3D%20%5Ctheta_0%20%2B%20%5Ctheta_1%20y_%7Bt-1%7D%20%2B%20e_t%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
y_t &= \theta_0 + \theta_1 y_{t-1} + \gamma_1 x_{t-1} + e_t \\
y_t &= \theta_0 + \theta_1 y_{t-1} + e_t
\end{aligned}
")

With two variables, we can test if
![x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x "x")
granger-causes
![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y"),
![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y")
granger-causes
![x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x "x")
or if there is bi-directional granger causality between
![x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x "x")
and
![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y").
The set up would start to look like a VAR model, and there is indeed a
package called **`bruceR`** that uses a VAR output to perform this test
using the function **`granger_test`** for bivariate models and
**`granger_causality`** for multivariate models.

``` r
granger_causality(varmodel = var)
```

    ## 
    ## Granger Causality Test (Multivariate)
    ## 
    ## F test and Wald χ² test based on VAR(2) model:
    ## ───────────────────────────────────────────────────────────
    ##                      F df1 df2     p     Chisq df     p    
    ## ───────────────────────────────────────────────────────────
    ##  -------------                                             
    ##  dCPI <= dDSPI    8.25   2 158 <.001 *** 16.50  2 <.001 ***
    ##  dCPI <= dCLI     3.87   2 158  .023 *    7.74  2  .021 *  
    ##  dCPI <= ALL      8.92   4 158 <.001 *** 35.69  4 <.001 ***
    ##  -------------                                             
    ##  dDSPI <= dCPI    0.23   2 158  .792      0.47  2  .792    
    ##  dDSPI <= dCLI    1.70   2 158  .185      3.41  2  .182    
    ##  dDSPI <= ALL     0.87   4 158  .484      3.48  4  .481    
    ##  -------------                                             
    ##  dCLI <= dCPI     1.25   2 158  .288      2.51  2  .286    
    ##  dCLI <= dDSPI    3.57   2 158  .030 *    7.14  2  .028 *  
    ##  dCLI <= ALL      3.76   4 158  .006 **  15.04  4  .005 ** 
    ## ───────────────────────────────────────────────────────────

The output provides both F-test and Likelihood Ratio Test values. Given
the significance level of 5%, `dDSPI` and `dCLI` granger-cause `dCPI`
and `dDSPI` granger-causes `dCLI`. None of the variables granger-cause
`dDSPI` and we did not find any bi-directional causality. It may be that
the wrong number of lags was chosen or simply that lags of other
variables does not help in predicting other variables. In such cases,
the forecasting power of this model may be affected since these
variables are inter-dependent on one another.

## 9 Johansen Test for Cointegration

Since we have I(1) variables, we can model the variables at levels if
they are found to have a cointegrating relationship. When variables are
cointegrated, they appear to have the same long-run patterns and the
difference between these two variables are stationary. In such cases, we
could estimate a Error Correction Model (ECM) or a Vector Error
Correction Model (VECM), which allows us to model long-run relationships
between variables. If these I(1) variables are not cointegrated, we can
only estimate a short-run model using the VAR or ARDL models.

To test for cointegration of multivariate time series, I have used the
Johansen Test using the **`ca.jo`** function in the **`urca`** package.
One of the arguments required is to specify the lag order of the level
series in the VAR model. This can be determined using the
**`VARselect`** function in the **`vars`** package.

``` r
# Use the data at levels, not at first difference
lagselect <- vars::VARselect(train, lag.max = 4, type = "const")

lagselect$selection
```

    ## AIC(n)  HQ(n)  SC(n) FPE(n) 
    ##      3      2      2      3

To be constant throughout the project, I would use the AIC to select the
number of lags. The `lagselect` output indicated we should use 3 lags
for the Johansen Test.

``` r
# Indicate type = "trace" for trace test, "eigen" for eigenvalue test

# Indicate ecdet = "none" assuming no intercept in the cointegration process, indicate "const" or "trend" to add constant or trend into the equation
# It is common to indicate no deterministic terms in the error correction term unless there are reasons to believe otherwise

# K is the number of lags from the lag selection using VARselect

jotest <- urca::ca.jo(x = train, 
                      type = "trace", 
                      ecdet = "none", 
                      K = 3)

summary(jotest)
```

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: trace statistic , with linear trend 
    ## 
    ## Eigenvalues (lambda):
    ## [1] 0.112648456 0.097619805 0.005873078
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##           test 10pct  5pct  1pct
    ## r <= 2 |  0.97  6.50  8.18 11.65
    ## r <= 1 | 17.92 15.66 17.95 23.52
    ## r = 0  | 37.64 28.71 31.52 37.22
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##             CPI.l3    DSPI.l3    CLI.l3
    ## CPI.l3   1.0000000  1.0000000  1.000000
    ## DSPI.l3  0.3407136 -0.2576423  1.229929
    ## CLI.l3  -0.5627592 -0.6376516 -5.011137
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##               CPI.l3     DSPI.l3       CLI.l3
    ## CPI.d  -0.0004640629 -0.03969217 2.872257e-05
    ## DSPI.d -0.2221608552 -0.07743235 7.697682e-04
    ## CLI.d  -0.0092783984  0.01652976 1.029779e-03

The test results are read starting from:

1.  r = 0: value under test column is more than value under 5pct column
    (statistically significant) and we have at least one cointegrating
    relationship.

2.  r \<= 1: value under test column is less than value under 5pct
    column (statistically insignificant) and we have at most one
    cointegrating relationship.

Since we have found evidence of cointegration through the Johansen Test,
we can estimate a ECM or VECM model.

### 9.1 Error Correction Model

Given the cointegration between variables, we can model the long-run
equilibrium variable
![y^\*](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y%5E%2A "y^*")
using the equilibrium of
![x^\*](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x%5E%2A "x^*")
at levels. To model the short-run model, we need to remember that the
variables are I(1), so we need to use a first-differenced equation, for
example
![\\Delta y_t = \\gamma_0 + \\gamma_1 \\Delta x_t + \\epsilon_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5CDelta%20y_t%20%3D%20%5Cgamma_0%20%2B%20%5Cgamma_1%20%5CDelta%20x_t%20%2B%20%5Cepsilon_t "\Delta y_t = \gamma_0 + \gamma_1 \Delta x_t + \epsilon_t").
But since there is a long-run association between
![y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y "y")
and
![x](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x "x"),
we could include some components into the short-run model so that it
takes into account this long-run association. What we can add into the
short-run model is the error correction term (ECT), which is the
one-period ago residual from the long-run model. The following are the
steps to modelling an ECM:

1.  Regress
    ![y^\*](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y%5E%2A "y^*")
    on
    ![x^\*](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x%5E%2A "x^*")
    (and their lags) and obtain the residuals
    ![v_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;v_t "v_t")

2.  Regress
    ![\\Delta y_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5CDelta%20y_t "\Delta y_t")
    on
    ![\\Delta x_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5CDelta%20x_t "\Delta x_t")
    and the lag of the residual in Step 1,
    ![v\_{t-1}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;v_%7Bt-1%7D "v_{t-1}")

If there is indeed cointegration, the coefficient of
![v\_{t-1}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;v_%7Bt-1%7D "v_{t-1}")
should be between -1 and 0. This is because, taking the long-run pattern
into account, if
![y\_{t-1}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y_%7Bt-1%7D "y_{t-1}")
was above/below
![y^\*](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y%5E%2A "y^*"),
then we should expect
![y_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y_t "y_t")
to decrease/increase, showing an adjustment to the equilibrium.

Let us first estimate a model in levels as per step one using
**`auto_ardl`**

``` r
ardl_lr_lag <- ARDL::auto_ardl(formula = CPI ~ DSPI + CLI,
                               data = as.zoo(train),
                               max_order = 4,
                               selection = "AIC",
                               grid = T)

ardl_lr <- ardl_lr_lag$best_model

summary(ardl_lr)
```

    ## 
    ## Time series regression with "zoo" data:
    ## Start = 1978 Q4, End = 2019 Q4
    ## 
    ## Call:
    ## dynlm::dynlm(formula = full_formula, data = data, start = start, 
    ##     end = end)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7297 -0.2122  0.0080  0.2073  1.1320 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.119847   0.442929  -0.271  0.78708    
    ## L(CPI, 1)    1.166220   0.077287  15.089  < 2e-16 ***
    ## L(CPI, 2)   -0.070034   0.117069  -0.598  0.55056    
    ## L(CPI, 3)   -0.126197   0.072143  -1.749  0.08223 .  
    ## DSPI         0.036974   0.009367   3.947  0.00012 ***
    ## L(DSPI, 1)  -0.006121   0.014886  -0.411  0.68151    
    ## L(DSPI, 2)  -0.018865   0.009706  -1.944  0.05376 .  
    ## CLI         -0.031103   0.020905  -1.488  0.13884    
    ## L(CLI, 1)    0.090332   0.033457   2.700  0.00771 ** 
    ## L(CLI, 2)   -0.039690   0.021399  -1.855  0.06553 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3793 on 155 degrees of freedom
    ## Multiple R-squared:  0.9994, Adjusted R-squared:  0.9994 
    ## F-statistic: 3.057e+04 on 9 and 155 DF,  p-value: < 2.2e-16

Instead of extracting the residuals, we can use the **`uecm`** and
**`recm`** functions to get the unrestricted and restricted form of the
ECM. The first model will show all the coefficients with the long-run
and short-run variables, while the second will only show the short-run
variables with the ECT.

``` r
un_ecm <- ARDL::uecm(ardl_lr)

summary(un_ecm)
```

    ## 
    ## Time series regression with "zoo" data:
    ## Start = 1978 Q4, End = 2019 Q4
    ## 
    ## Call:
    ## dynlm::dynlm(formula = full_formula, data = data, start = start, 
    ##     end = end)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7297 -0.2122  0.0080  0.2073  1.1320 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -0.119847   0.442929  -0.271  0.78708    
    ## L(CPI, 1)     -0.030011   0.010889  -2.756  0.00655 ** 
    ## L(DSPI, 1)     0.011989   0.003216   3.728  0.00027 ***
    ## L(CLI, 1)      0.019538   0.006821   2.865  0.00475 ** 
    ## d(L(CPI, 1))   0.196231   0.074311   2.641  0.00912 ** 
    ## d(L(CPI, 2))   0.126197   0.072143   1.749  0.08223 .  
    ## d(DSPI)        0.036974   0.009367   3.947  0.00012 ***
    ## d(L(DSPI, 1))  0.018865   0.009706   1.944  0.05376 .  
    ## d(CLI)        -0.031103   0.020905  -1.488  0.13884    
    ## d(L(CLI, 1))   0.039690   0.021399   1.855  0.06553 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3793 on 155 degrees of freedom
    ## Multiple R-squared:  0.4898, Adjusted R-squared:  0.4602 
    ## F-statistic: 16.54 on 9 and 155 DF,  p-value: < 2.2e-16

`L(CPI, 1)` is the lag of our dependent variable and its coefficient is
actually the ECT, which tells us how `CPI` would react given a shock to
`L(CPI, 1)`. `L(DSPI, 1)` and `L(CLI, 1)` is the long-run coefficient
(after some conversion) and the differenced variables (those starting
with `d...`) are the short-run coefficients.

``` r
# Indicated case = 3 to include constant in the model
r_ecm <- ARDL::recm(un_ecm, case = 3)

summary(r_ecm)
```

    ## 
    ## Time series regression with "zooreg" data:
    ## Start = 1978 Q4, End = 2019 Q4
    ## 
    ## Call:
    ## dynlm::dynlm(formula = full_formula, data = data, start = start, 
    ##     end = end)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7297 -0.2122  0.0080  0.2073  1.1320 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -0.119847   0.074168  -1.616  0.10812    
    ## d(L(CPI, 1))   0.196231   0.073783   2.660  0.00864 ** 
    ## d(L(CPI, 2))   0.126197   0.070983   1.778  0.07737 .  
    ## d(DSPI)        0.036974   0.008794   4.204 4.38e-05 ***
    ## d(L(DSPI, 1))  0.018865   0.009622   1.961  0.05170 .  
    ## d(CLI)        -0.031103   0.020452  -1.521  0.13032    
    ## d(L(CLI, 1))   0.039690   0.020853   1.903  0.05883 .  
    ## ect           -0.030011   0.007082  -4.238 3.84e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3768 on 157 degrees of freedom
    ##   (0 observations deleted due to missingness)
    ## Multiple R-squared:  0.4898, Adjusted R-squared:  0.4671 
    ## F-statistic: 21.53 on 7 and 157 DF,  p-value: < 2.2e-16

We can see that the `r_ecm` output is the same as the `un_ecm` output,
less the long-run variables. And as expected, the ECT is negative and is
between -1 and 0 (and statistically significant).

To obtain the long-run multiplier, we can use the **`multiplier`**
function.

``` r
stargazer(multipliers(un_ecm), # Can also be used on ardl_lr
          type = "text", 
          title = "Long Run Multipliers of var Model",
          summary = F)
```

    ## 
    ## Long Run Multipliers of var Model
    ## ====================================================
    ##      term     estimate std.error t.statistic p.value
    ## ----------------------------------------------------
    ## 1 (Intercept)  -3.993   15.733     -0.254     0.800 
    ## 2    DSPI      0.399     0.159      2.510     0.013 
    ## 3     CLI      0.651     0.043     15.274       0   
    ## ----------------------------------------------------

The equation of the error correction model is:

$$
$$

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot ardl ecm-1.png" style="display: block; margin: auto;" />

### 9.2 Vector Error Correction Model

The VECM is simply a VAR model with error correction terms. What has
been explained in Section 7.3 Vector Autoregressive Models and Section
9.1 Error Correction Model should be sufficient to give a glimpse of
what a VECM is about.

We can build a VECM using the **`urca`** package, using the functions
**`ca.jo`**, **`cajools`** and **`cajorls`**. Although we had seen
earlier in the Johansen Test that we used **`ca.jo`** to test the number
of cointegrating relationships, it can also provide the VECM using the
other functions mentioned.

``` r
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
```

| term     | estimate | std.error | statistic | p.value |
|:---------|---------:|----------:|----------:|--------:|
| ect1     |  -0.0005 |    0.0066 |   -0.0702 |  0.9442 |
| constant |   0.1526 |    0.4669 |    0.3270 |  0.7441 |
| CPI.dl1  |   0.2691 |    0.0817 |    3.2926 |  0.0012 |
| DSPI.dl1 |   0.0406 |    0.0102 |    3.9737 |  0.0001 |
| CLI.dl1  |   0.0391 |    0.0227 |    1.7260 |  0.0863 |
| CPI.dl2  |   0.2616 |    0.0766 |    3.4138 |  0.0008 |
| DSPI.dl2 |  -0.0038 |    0.0105 |   -0.3607 |  0.7188 |
| CLI.dl2  |   0.0314 |    0.0230 |    1.3683 |  0.1732 |

VECM Estimation Output for dCPI

``` r
kable(tidy(summary(vec_ecm$rlm)[[2]]), format = "pipe",
      caption = "VECM Estimation Output for dDSPI",
      digits = 4)
```

| term     | estimate | std.error | statistic | p.value |
|:---------|---------:|----------:|----------:|--------:|
| ect1     |  -0.2222 |    0.0546 |   -4.0690 |  0.0001 |
| constant |  15.5003 |    3.8532 |    4.0227 |  0.0001 |
| CPI.dl1  |   0.1272 |    0.6745 |    0.1886 |  0.8507 |
| DSPI.dl1 |   0.3030 |    0.0843 |    3.5919 |  0.0004 |
| CLI.dl1  |  -0.1062 |    0.1871 |   -0.5677 |  0.5710 |
| CPI.dl2  |   0.8377 |    0.6324 |    1.3247 |  0.1872 |
| DSPI.dl2 |  -0.2986 |    0.0867 |   -3.4457 |  0.0007 |
| CLI.dl2  |   0.2283 |    0.1895 |    1.2046 |  0.2302 |

VECM Estimation Output for dDSPI

``` r
kable(tidy(summary(vec_ecm$rlm)[[3]]), format = "pipe",
      caption = "VECM Estimation Output for dCLI",
      digits = 4)
```

| term     | estimate | std.error | statistic | p.value |
|:---------|---------:|----------:|----------:|--------:|
| ect1     |  -0.0093 |    0.0249 |   -0.3726 |  0.7100 |
| constant |   1.1357 |    1.7576 |    0.6461 |  0.5191 |
| CPI.dl1  |  -0.0291 |    0.3077 |   -0.0944 |  0.9249 |
| DSPI.dl1 |  -0.0405 |    0.0385 |   -1.0520 |  0.2944 |
| CLI.dl1  |   0.3452 |    0.0853 |    4.0448 |  0.0001 |
| CPI.dl2  |  -0.3617 |    0.2885 |   -1.2538 |  0.2118 |
| DSPI.dl2 |  -0.0865 |    0.0395 |   -2.1872 |  0.0302 |
| CLI.dl2  |  -0.0660 |    0.0864 |   -0.7638 |  0.4461 |

VECM Estimation Output for dCLI

The output shows the coefficients under the `estimate` column of each
variable under the `term` column. The 3 equations in the VECM are:

$$
$$

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot vecm cpi-1.png" style="display: block; margin: auto;" />

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot vecm dspi-1.png" style="display: block; margin: auto;" />

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot vecm cli-1.png" style="display: block; margin: auto;" />

We can also convert the VECM in differences to a VAR in levels using the
**`vec2var`** function, which is particularly useful when we are
forecasting since it gives us the output in levels.

``` r
var_ecm <- vec2var(get_vecm, r = 2)

var_ecm
```

    ## 
    ## Coefficient matrix of lagged endogenous variables:
    ## 
    ## A1:
    ##           CPI.l1     DSPI.l1     CLI.l1
    ## CPI   1.14854574  0.04091275 0.04770292
    ## DSPI -0.32920007  1.22816711 0.03501547
    ## CLI   0.01166646 -0.04382656 1.34696558
    ## 
    ## 
    ## A2:
    ##           CPI.l2     DSPI.l2       CLI.l2
    ## CPI  -0.02508445 -0.03874531 -0.007197675
    ## DSPI  0.67623129 -0.59052287  0.335495327
    ## CLI  -0.32528719 -0.04833322 -0.411441590
    ## 
    ## 
    ## A3:
    ##          CPI.l3     DSPI.l3      CLI.l3
    ## CPI  -0.1636175 0.007900839 -0.01493431
    ## DSPI -0.6466244 0.306612375 -0.19611287
    ## CLI   0.3208721 0.084739736  0.05915729
    ## 
    ## 
    ## Coefficient matrix of deterministic regressor(s).
    ## 
    ##        constant
    ## CPI   0.4005884
    ## DSPI 15.9839561
    ## CLI   1.0323983

## 10 Post-Estimation Tests

Two common problems encountered in time series analysis are serial
correlation of errors and heteroskedastic errors. These can affect our
analysis as estimators are no longer efficient and the usual test
statistics become unreliable, which can be important if inference is the
main purpose. The Breusch-Godfrey Test can be used to test for serial
correlation, while the Breusch-Pagan Test can be used to test for
heteroskedasticity. However, there is no single package that could
accommodate different models for these tests, so we will have to use
different functions from different packages for the models that are
accepted.

However, using Newey-West standard errors to obtain the corrected test
statistics regardless of whether serial correlation and/or
heteroskedasticity is present seems to be an acceptable method,
especially when we have large sample sizes.

### 10.1 ARDL Model

We can use the **`bgtest`** and **`bptest`** from the **`lmtest`**
package.

``` r
# Test with up to 4 lags of the residuals

lmtest::bgtest(ardl, order = 4)
```

    ## 
    ##  Breusch-Godfrey test for serial correlation of order up to 4
    ## 
    ## data:  ardl
    ## LM test = 10.169, df = 4, p-value = 0.03767

``` r
lmtest::bptest(ardl, studentize = T)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  ardl
    ## BP = 18.642, df = 9, p-value = 0.02842

There is serial correlation and heteroskedasticity in the ARDL
residuals, and we should use the Newey-West (Heteroskedasticity and
Autocorrelation Corrected) standard errors to obtain the correct test
statistics for inference.

### 10.2 VAR Model

The functions that can be applied to VAR models are **`serial.test`**
and the **`arch.test`** from the **`vars`** package. The **`arch.test`**
performs the ARCH test to detect if there is changing conditional
variance in a time series, unlike the Breusch-Pagan test that detects
for constant variance.

``` r
vars::serial.test(var, lags.bg = 4, type = "BG")
```

    ## 
    ##  Breusch-Godfrey LM test
    ## 
    ## data:  Residuals of VAR object var
    ## Chi-squared = 37.111, df = 36, p-value = 0.4176

``` r
vars::arch.test(var, lags.single = 4, lags.multi = 4, multivariate.only = F)
```

    ## $dCPI
    ## 
    ##  ARCH test (univariate)
    ## 
    ## data:  Residual of dCPI equation
    ## Chi-squared = 11.411, df = 4, p-value = 0.02232
    ## 
    ## 
    ## $dDSPI
    ## 
    ##  ARCH test (univariate)
    ## 
    ## data:  Residual of dDSPI equation
    ## Chi-squared = 1.9459, df = 4, p-value = 0.7457
    ## 
    ## 
    ## $dCLI
    ## 
    ##  ARCH test (univariate)
    ## 
    ## data:  Residual of dCLI equation
    ## Chi-squared = 3.2084, df = 4, p-value = 0.5236
    ## 
    ## 
    ## 
    ##  ARCH (multivariate)
    ## 
    ## data:  Residuals of VAR object var
    ## Chi-squared = 195.17, df = 144, p-value = 0.002925

From the tests, there are was no serial correlation detected at up to 4
lags of the error term, and only `dCPI` and the overall VAR models has
ARCH effects.

### 10.3 VECM

``` r
vars::serial.test(var_ecm, lags.bg = 4, type = "BG")
```

    ## 
    ##  Breusch-Godfrey LM test
    ## 
    ## data:  Residuals of VAR object var_ecm
    ## Chi-squared = 32.422, df = 36, p-value = 0.6395

``` r
vars::arch.test(var_ecm, lags.single = 4, lags.multi = 4, multivariate.only = F)
```

    ## $`resids of CPI`
    ## 
    ##  ARCH test (univariate)
    ## 
    ## data:  Residual of resids of CPI equation
    ## Chi-squared = 15.957, df = 4, p-value = 0.003077
    ## 
    ## 
    ## $`resids of DSPI`
    ## 
    ##  ARCH test (univariate)
    ## 
    ## data:  Residual of resids of DSPI equation
    ## Chi-squared = 1.4143, df = 4, p-value = 0.8417
    ## 
    ## 
    ## $`resids of CLI`
    ## 
    ##  ARCH test (univariate)
    ## 
    ## data:  Residual of resids of CLI equation
    ## Chi-squared = 3.571, df = 4, p-value = 0.4672
    ## 
    ## 
    ## 
    ##  ARCH (multivariate)
    ## 
    ## data:  Residuals of VAR object var_ecm
    ## Chi-squared = 209.8, df = 144, p-value = 0.0002854

Similar to the tests for the VAR model, there is no serial correlation
detected at up to 4 lags of the error terms and only the `CPI` single
equation and the multivariate model has ARCH effects.

## 11 Forecasting and Evaluation

For the forecast and evaluation, I would compare the data at first
difference in the `diff.test` dataset against the 8 period ahead
forecasted values. This would mean that the forecasted values are for
the periods 2020 Q1 to 2021 Q4. I then plotted the actual vs forecasted
values at levels to see how well the different models fit. However,
there are two points to keep in mind.

Firstly, the forecasted values will be in first-difference. Conversion
to levels will be needed, which can be obtained simply by adding the
cumulative forecasted values to the last observation used in the
`diff.train` dataset.

Secondly, models such as ARIMA and VAR uses endogenous variable but ARDL
models contains exogenous variables. This means that we would need data
on the exogenous variables in the ARDL models for the forecasting to
work. This can be done through the use of ARIMA models to predict
exogenous variables before inputting them into the ARDL forecast.
However, for simplicity, I would use the data in the `diff.test` set to
compare the performance of the ARDL and ARDL-EC models while ARIMA and
VAR/VECM would be compared to one another.

For the ARIMA models, we can generate forecast values using the
**`forecast`** function in the **`forecast`** package. For ARDL, we
would need to use the **`dLagM`** package and for ARCL-ECM we need to
use the **`ecm`** package as they have their own forecast functions. For
VAR and VECM models, we need to use the **`predict`** function from the
base R package.

### 11.1 Forecast Performance of ARIMA Models

``` r
arima1_h8 <- forecast(object = arima1, h = 8, level = 95)

stargazer(as.data.frame(arima1_h8), 
          type = "text",
          title = "Forecasted dCPI 2020Q1 to 2021Q4 Using arima1 Model", 
          summary = F)
```

    ## 
    ## Forecasted dCPI 2020Q1 to 2021Q4 Using arima1 Model
    ## =================================
    ##       Point Forecast Lo 95  Hi 95
    ## ---------------------------------
    ## 42 Q4     0.232      -0.660 1.124
    ## 43 Q1     0.281      -0.680 1.241
    ## 43 Q2     0.276      -0.717 1.269
    ## 43 Q3     0.296      -0.706 1.299
    ## 43 Q4     0.307      -0.705 1.319
    ## 44 Q1     0.314      -0.702 1.331
    ## 44 Q2     0.320      -0.699 1.339
    ## 44 Q3     0.324      -0.697 1.344
    ## ---------------------------------

``` r
stargazer(accuracy(object = arima1_h8, x = diff.test["2020/2021"]$dCPI), 
          type = "text", 
          title = "Accuracy Measures of arima1 Model for 8-Steps Ahead Forecasting")
```

    ## 
    ## Accuracy Measures of arima1 Model for 8-Steps Ahead Forecasting
    ## ============================================================
    ##               ME   RMSE   MAE    MPE     MAPE   MASE   ACF1 
    ## ------------------------------------------------------------
    ## Training set 0.001 0.447 0.300 -498.193 712.180 0.659 0.0005
    ## Test set     0.153 0.650 0.501 106.786  106.786 1.101       
    ## ------------------------------------------------------------

R simply refers to the row number of the observations and does not
correct the dates assigned to each observation after the use of lags in
the models as can be seen in the table of the forecasted `dCPI` values.
It would be important to take note of this point when plotting the
forecasted values.

The accuracy measures Root Mean Squared Error (RMSE), Mean Absolute
Error (MAE) and Mean Absolute Scaled Error (MASE) can be compared across
models and lower values would indicate better out-of-sample forecast.
The Mean Absolute Percentage Error (MAPE) can also be used, but may not
be useful in cases where we might have zeroes in data. For the
out-of-sample forecast, we are interested in the accuracy measures of
the `Test set` row.

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot f_arima1-1.png" style="display: block; margin: auto;" />

For model `arima2`, I suppressed the code to reduce the length of the
document since it is similar to what I did for model `arima1`.

    ## 
    ## Forecasted dCPI 2020Q1 to 2021Q4 Using arima2 Model
    ## =================================
    ##       Point Forecast Lo 95  Hi 95
    ## ---------------------------------
    ## 42 Q4     0.301      -0.576 1.177
    ## 43 Q1     0.296      -0.666 1.257
    ## 43 Q2     0.333      -0.668 1.335
    ## 43 Q3     0.316      -0.689 1.321
    ## 43 Q4     0.342      -0.669 1.353
    ## 44 Q1     0.323      -0.688 1.334
    ## 44 Q2     0.343      -0.669 1.356
    ## 44 Q3     0.326      -0.687 1.338
    ## ---------------------------------

    ## 
    ## Accuracy Measures of arima2 Model for 8-Steps Ahead Forecasting
    ## =============================================================
    ##                ME   RMSE   MAE    MPE     MAPE   MASE   ACF1 
    ## -------------------------------------------------------------
    ## Training set 0.0005 0.442 0.301 -538.203 780.460 0.662 -0.012
    ## Test set     0.124  0.651 0.493 115.255  115.255 1.083       
    ## -------------------------------------------------------------

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot f_arima2-1.png" style="display: block; margin: auto;" />

There seemed to be negligible differences between the models `arima1`
and `arima2`, so it might be better to go with the simpler `arima2` when
using ARIMA models for forecasting Singapore CPI.

### 11.2 Forecast Performance of ARDL and ARDL-EC Models

To forecast the ARDL and ARDL-EC models, we need values for the
exogenous variables. However, as there are lags in the model as well,
**`forecast`** and **`predict`** functions do not work well with the
underlying **`dynlm`** objects of these models. To resolve this, we have
to use the **`dLagM`** package function **`ardlDlm`** to fit the ARDL
model. The **`dLagM`** package also contains its own **`forecast`**
function.

``` r
ardl.new <- ardlDlm(formula = dCPI ~ dDSPI + dCLI, # Indicate variables for the ARDL model
                    data = data.frame(diff.train), 
                    p = 2, q = 4, # Include 2 lags for exogenous variables, 4 lags for dependent variable
                    remove = list(p = list(dDSPI = c(2)))) # Remove second lag for dDSPI so that model is same as auto_ardl

stargazer(ardl.new$model, type = "text",
          title = "ARDL Regression Using dLagM Package", 
          dep.var.labels.include = F, 
          column.labels = "CPI.t")
```

    ## 
    ## ARDL Regression Using dLagM Package
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                CPI.t           
    ## -----------------------------------------------
    ## dDSPI.t                      0.039***          
    ##                               (0.009)          
    ##                                                
    ## dDSPI.1                      0.028***          
    ##                               (0.010)          
    ##                                                
    ## dCLI.t                        -0.040*          
    ##                               (0.022)          
    ##                                                
    ## dCLI.1                        0.057**          
    ##                               (0.023)          
    ##                                                
    ## dCLI.2                         0.032           
    ##                               (0.021)          
    ##                                                
    ## dCPI.1                       0.257***          
    ##                               (0.077)          
    ##                                                
    ## dCPI.2                        0.192**          
    ##                               (0.075)          
    ##                                                
    ## dCPI.3                         0.013           
    ##                               (0.075)          
    ##                                                
    ## dCPI.4                        0.176**          
    ##                               (0.070)          
    ##                                                
    ## Constant                      0.097**          
    ##                               (0.047)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                    163            
    ## R2                             0.467           
    ## Adjusted R2                    0.436           
    ## Residual Std. Error      0.389 (df = 153)      
    ## F Statistic           14.895*** (df = 9; 153)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
x.new <- rbind(as.vector(diff.test["2020/2021", 2]), as.vector(diff.test["2020/2021", 3])) # function only accepts new data in matrix class

ardl_h8 <- dLagM::forecast(model = ardl.new, x = x.new, h = 8, interval = T, level = 0.95)

stargazer(ardl_h8$forecasts, 
          type = "text",
          title = "Forecasted dCPI 2020Q1 to 2021Q4 Using ardl Model", 
          summary = F)
```

    ## 
    ## Forecasted dCPI 2020Q1 to 2021Q4 Using ardl Model
    ## ========================
    ##   95% LB Forecast 95% UB
    ## ------------------------
    ## 1 -0.632  0.103   0.846 
    ## 2 -1.153  -0.336  0.452 
    ## 3 -1.389  -0.512  0.290 
    ## 4 -0.653  0.226   1.005 
    ## 5 -0.108  0.756   1.585 
    ## 6 -0.217  0.706   1.563 
    ## 7 -0.191  0.735   1.545 
    ## 8 -0.039  0.796   1.633 
    ## ------------------------

``` r
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
```

    ## 
    ## Accuracy Measures of ardl Model for 8-Steps Ahead Forecasting
    ## =====================================================
    ##               ME   RMSE   MAE    MPE     MAPE   MASE 
    ## -----------------------------------------------------
    ## Training set  -0   0.377 0.269 -560.441 772.280 0.713
    ## Test set     0.137 0.466 0.338  76.297  83.043  0.896
    ## -----------------------------------------------------

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot f_ardl-1.png" style="display: block; margin: auto;" />

For the ARDL-ECM, since we have found cointegration we could simply
obtain the forecast in levels since there is a long-run model `ardl_lr`.
Of course, there will still be a need to model and forecast it using the
**`dLagM`** package for it to work.

``` r
ardlecm.new <- dLagM::ardlDlm(formula = CPI ~ DSPI + CLI, data = data.frame(train), p = 2, q = 3)

stargazer(ardlecm.new$model, 
          type = "text",
          title = "ECM Regression Using dLagM Package", 
          dep.var.labels.include = F, 
          column.labels = "CPI.t")
```

    ## 
    ## ECM Regression Using dLagM Package
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                CPI.t           
    ## -----------------------------------------------
    ## DSPI.t                       0.037***          
    ##                               (0.009)          
    ##                                                
    ## DSPI.1                        -0.006           
    ##                               (0.015)          
    ##                                                
    ## DSPI.2                        -0.019*          
    ##                               (0.010)          
    ##                                                
    ## CLI.t                         -0.031           
    ##                               (0.021)          
    ##                                                
    ## CLI.1                        0.090***          
    ##                               (0.033)          
    ##                                                
    ## CLI.2                         -0.040*          
    ##                               (0.021)          
    ##                                                
    ## CPI.1                        1.166***          
    ##                               (0.077)          
    ##                                                
    ## CPI.2                         -0.070           
    ##                               (0.117)          
    ##                                                
    ## CPI.3                         -0.126*          
    ##                               (0.072)          
    ##                                                
    ## Constant                      -0.120           
    ##                               (0.443)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                    165            
    ## R2                             0.999           
    ## Adjusted R2                    0.999           
    ## Residual Std. Error      0.379 (df = 155)      
    ## F Statistic         30,570.540*** (df = 9; 155)
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
newdata <- rbind(as.vector(test["2020/2021", 2]), as.vector(test["2020/2021", 3]))

ecm_h8 <- dLagM::forecast(model = ardlecm.new, x = newdata, h = 8, interval = T, level = 0.95)

stargazer(ecm_h8$forecasts, 
          type = "text",
          title = "Forecasted dCPI 2020Q1 to 2021Q4 Using ecm Model", 
          summary = F)
```

    ## 
    ## Forecasted dCPI 2020Q1 to 2021Q4 Using ecm Model
    ## =========================
    ##   95% LB Forecast 95% UB 
    ## -------------------------
    ## 1 99.550 100.229  100.980
    ## 2 98.577  99.803  100.946
    ## 3 97.795  99.287  100.777
    ## 4 97.628  99.398  101.222
    ## 5 97.818  99.864  101.934
    ## 6 98.195 100.415  102.632
    ## 7 98.733 101.158  103.520
    ## 8 99.284 101.898  104.404
    ## -------------------------

``` r
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
```

    ## 
    ## Accuracy Measures of ecm Model for 8-Steps Ahead Forecasting
    ## =====================================================
    ##               ME   RMSE   MAE    MPE     MAPE   MASE 
    ## -----------------------------------------------------
    ## Training set  -0   0.368 0.269 -317.329 567.977 0.713
    ## Test set     0.236 0.491 0.394  71.472  78.533  1.046
    ## -----------------------------------------------------

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot ecm-1.png" style="display: block; margin: auto;" />

### 11.3 Forecast Performance of VAR and VEC Models

Forecasting VAR and VEC models is much simpler that for ARDL and ARDL-EC
models in R. It simply requires the **`predict`** function in base R.
However, we still need to create a list object of class `forecast` that
works with the **`accuracy`** function.

``` r
var_h8 <- predict(object = var, n.ahead = 8, ci = 0.95)

stargazer(var_h8$fcst, 
          type = "text", 
          title = c("Forecasted dCPI from 1Q20 to 4Q21", "Forecasted dDSPI from 1Q20 to 4Q21", "Forecasted dCLI from 1Q20 to 4Q21"))
```

    ## 
    ## Forecasted dCPI from 1Q20 to 4Q21
    ## ========================
    ## fcst  lower  upper  CI  
    ## ------------------------
    ## 0.090 -0.721 0.900 0.811
    ## 0.164 -0.763 1.091 0.927
    ## 0.207 -0.795 1.210 1.002
    ## 0.261 -0.760 1.283 1.022
    ## 0.294 -0.735 1.324 1.029
    ## 0.315 -0.716 1.346 1.031
    ## 0.323 -0.709 1.354 1.031
    ## 0.326 -0.706 1.357 1.032
    ## ------------------------
    ## 
    ## Forecasted dDSPI from 1Q20 to 4Q21
    ## =========================
    ## fcst   lower  upper  CI  
    ## -------------------------
    ## -0.360 -7.394 6.675 7.034
    ## -0.242 -7.630 7.147 7.388
    ## -0.118 -7.599 7.362 7.481
    ## 0.073  -7.474 7.621 7.547
    ## 0.128  -7.423 7.679 7.551
    ## 0.088  -7.465 7.641 7.553
    ## 0.047  -7.506 7.601 7.554
    ## 0.031  -7.523 7.584 7.554
    ## -------------------------
    ## 
    ## Forecasted dCLI from 1Q20 to 4Q21
    ## ========================
    ## fcst  lower  upper  CI  
    ## ------------------------
    ## 0.132 -2.921 3.185 3.053
    ## 0.623 -2.588 3.833 3.210
    ## 0.689 -2.645 4.024 3.334
    ## 0.637 -2.757 4.032 3.395
    ## 0.578 -2.829 3.985 3.407
    ## 0.520 -2.892 3.932 3.412
    ## 0.487 -2.927 3.901 3.414
    ## 0.476 -2.939 3.890 3.414
    ## ------------------------

``` r
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
```

    ## 
    ## Accuracy Measures of var Model for 8-Steps Ahead Forecasting for dCPI
    ## =====================================================
    ##               ME   RMSE   MAE    MPE     MAPE   MASE 
    ## -----------------------------------------------------
    ## Training set  -0   0.405 0.283 -647.127 853.314 0.751
    ## Test set     0.199 0.623 0.482  83.371  83.371  1.279
    ## -----------------------------------------------------

``` r
stargazer(accuracy(object = var_acc_list$dDSPI_acc, x = diff.test["2020/2021"]$dDSPI), 
          type = "text", 
          title = "Accuracy Measures of var Model for 8-Steps Ahead Forecasting for dDSPI")
```

    ## 
    ## Accuracy Measures of var Model for 8-Steps Ahead Forecasting for dDSPI
    ## ====================================================
    ##               ME   RMSE   MAE    MPE    MAPE   MASE 
    ## ----------------------------------------------------
    ## Training set  -0   3.512 2.331 25.063  242.126 0.913
    ## Test set     1.694 5.103 4.527 100.261 100.261 1.774
    ## ----------------------------------------------------

``` r
stargazer(accuracy(object = var_acc_list$dCLI_acc, x = diff.test["2020/2021"]$dCLI), 
          type = "text", 
          title = "Accuracy Measures of var Model for 8-Steps Ahead Forecasting for dCLI")
```

    ## 
    ## Accuracy Measures of var Model for 8-Steps Ahead Forecasting for dCLI
    ## ====================================================
    ##               ME   RMSE   MAE    MPE    MAPE   MASE 
    ## ----------------------------------------------------
    ## Training set  -0   1.524 1.144           Inf   0.950
    ## Test set     0.532 2.709 2.012 224.208 224.208 1.672
    ## ----------------------------------------------------

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot f_var-1.png" style="display: block; margin: auto;" />

For the VECM, R does not have a function that allows us to forecast the
VECM in differences. We have to use the VAR in levels that was converted
from the model `vec_ecm` using the **`vec2var`** function.

``` r
vecm_h8 <- predict(var_ecm, n.ahead = 8, ci = 0.95)

stargazer(vecm_h8$fcst, 
          type = "text", 
          title = c("Forecasted dCPI from 1Q20 to 4Q21", "Forecasted dDSPI from 1Q20 to 4Q21", "Forecasted dCLI from 1Q20 to 4Q21"))
```

    ## 
    ## Forecasted dCPI from 1Q20 to 4Q21
    ## ============================
    ## fcst    lower   upper   CI  
    ## ----------------------------
    ## 100.204 99.450 100.958 0.754
    ## 100.202 98.950 101.454 1.252
    ## 100.205 98.459 101.950 1.746
    ## 100.243 98.064 102.421 2.178
    ## 100.315 97.745 102.885 2.570
    ## 100.419 97.485 103.353 2.934
    ## 100.550 97.273 103.828 3.277
    ## 100.705 97.100 104.310 3.605
    ## ----------------------------
    ## 
    ## Forecasted dDSPI from 1Q20 to 4Q21
    ## ============================
    ## fcst   lower   upper    CI  
    ## ----------------------------
    ## 94.655 88.126 101.184 6.529 
    ## 93.509 83.187 103.832 10.323
    ## 92.629 80.457 104.801 12.172
    ## 92.065 78.615 105.516 13.450
    ## 91.728 77.111 106.345 14.617
    ## 91.537 75.881 107.193 15.656
    ## 91.461 74.921 108.001 16.540
    ## 91.482 74.178 108.786 17.304
    ## ----------------------------
    ## 
    ## Forecasted dCLI from 1Q20 to 4Q21
    ## =============================
    ## fcst     lower   upper   CI  
    ## -----------------------------
    ## 104.640 101.655 107.624 2.984
    ## 105.331 100.406 110.256 4.925
    ## 106.224 99.930  112.517 6.293
    ## 107.134 99.791  114.477 7.343
    ## 108.003 99.856  116.150 8.147
    ## 108.808 100.041 117.575 8.767
    ## 109.556 100.291 118.820 9.264
    ## 110.258 100.577 119.938 9.680
    ## -----------------------------

``` r
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
```

    ## 
    ## Accuracy Measures of vecm Model for 8-Steps Ahead Forecasting for dCPI
    ## =====================================================
    ##               ME   RMSE   MAE    MPE     MAPE   MASE 
    ## -----------------------------------------------------
    ## Training set   0   0.405 0.283 -648.788 853.811 0.751
    ## Test set     0.385 0.715 0.603  90.651  90.651  1.598
    ## -----------------------------------------------------

``` r
stargazer(accuracy(object = vecm_acc_list$diff_DSPI_acc, x = diff.test["2020/2021"]$dDSPI), 
          type = "text", 
          title = "Accuracy Measures of vecm Model for 8-Steps Ahead Forecasting for dDSPI")
```

    ## 
    ## Accuracy Measures of vecm Model for 8-Steps Ahead Forecasting for dDSPI
    ## ===================================================
    ##               ME   RMSE   MAE   MPE    MAPE   MASE 
    ## ---------------------------------------------------
    ## Training set  -0   3.340 2.253 17.571 231.927 0.882
    ## Test set     2.741 5.165 4.670 56.722 98.690  1.829
    ## ---------------------------------------------------

``` r
stargazer(accuracy(object = vecm_acc_list$diff_CLI_acc, x = diff.test["2020/2021"]$dCLI), 
          type = "text", 
          title = "Accuracy Measures of vecm Model for 8-Steps Ahead Forecasting for dCLI")
```

    ## 
    ## Accuracy Measures of vecm Model for 8-Steps Ahead Forecasting for dCLI
    ## =====================================================
    ##                ME   RMSE   MAE    MPE    MAPE   MASE 
    ## -----------------------------------------------------
    ## Training set   -0   1.524 1.144           Inf   0.951
    ## Test set     -0.206 3.489 2.503 307.039 307.039 2.080
    ## -----------------------------------------------------

<img src="Forecasting-Singapore-CPI-Using-Different-Time-Series-Models_files/figure-gfm/plot f_vecm-1.png" style="display: block; margin: auto;" />

## 12 Conclusion

In this project, I have detailed the 5 common time series models used in
forecasting that I have learnt. Through this project, we can experience
how using R for time series analysis feels like. It is not as simple as
using EViews or Stata as the packages have their own quirks and
functions can be repeated across packages but only serve models that are
built by those packages.

The ARIMA models gives us a quick and easy forecasting method as we are
just using the lags of CPI and its residuals, without the need of
exogenous variables. However, whether in-sample or out-of-sample
prediction, these models do not capture much of the magnitude of the
movements in CPI although they are able to capture its patterns.

The ARDL model was able to use exogenous variables to provide a better
fit to the data, both in-sample and out-of-sample. However, for
out-of-sample prediction, I had used actual data of the exogenous
variables, which cannot happen for real-world forecasting methods. The
correct method would be to use another model to forecast the exogenous
variables and use these forecasted values fit the out-of-sample
prediction. This means that the accuracy of the forecast depends on both
the CPI model and the models of other variables.

The VAR model can be used when all variables are endogenous and can be
used to explain one another. By using VAR models, we solved the problem
of ARDL models by allowing variables to be forecasted at the same time.
However, despite the good in-sample fit, the out-of-sample fit is
disappointing. This is likely due to the findings of the Granger
Causality Test, where DSPI and CLI can be used to improve the prediction
of CPI but other variables have lack of granger causality. This would be
a model misspecification and would lead to poor forecasting results.

The Johansen Test was used to determine if cointegration exists between
the variables and we found that there is a cointegrating relationship.
This was the reason for using the ECM and VECM. The ECM had much better
out-of-sample forecasts than the VECM, although this could be attributed
to using actual data for the exogenous variables. However, we can see
that the VECM was unable to capture the rising trend of the CPI in
levels through the forecast, which again could be due to model
misspecification as in the VAR model case.

For the case of forecasting Singapore CPI, ARIMA models seem to have the
best forecasting ability among the models given its simplicity. It would
be interesting to find out if using ARDL with forecasted exogenous
variables could still provide a better forecast accuracy than the ARIMA
models in the future. As for the VAR and VECM models, there would be a
need to re-specify the models or simply fit the models individually just
as in the ARDL case. I had also ignored the effects of seasonality in
this project, which I would discuss separately in the future as well.
