## Project Description

### 1 Portfolio Optimization and Back-testing

The purpose of this project was to apply what I had learnt in R onto portfolio optimization.  

The first part introduces the relevant packages and functions in R, which includes importing stock price data from Yahoo Finance,
calculating returns and important evaluation metrics.  

The second part involves single-period optimization and periodic optimization and back-testing.
For the single-period optimization, I created one portfolio that minimizes variance and another that maximizes returns.
For the back-testing optimization, I used the mean-variance portfolio, and solved the optimization using the random
portfolio method.  

Click the link to access the rendered HTML file: https://tinyurl.com/yfyxuzat

### 2 Effects of Risk Measures on Portfolio Composition and Performance

In this project, I attempted to determine the effects of the different risk measures on portfolio optimization and performance. The following risk measures have been used:  

- Variance and Standard Deviation
- Semi-Variance and Semi-Deviation
- Downside Deviation
- Value-at-Risk
- Conditional Value-at-Risk (or Expected Shortfall, Expected Tail Loss)

In the first part, I introduced the different measures on a single security and show how they differ. In the second part, I generated random portfolios and optimized two sets of portfolios:  

- Portfolios that minimize the different risk measures
- Portfolios that minimize the different risk measures for a given level of return

The portfolio was created using 10 large-cap U.S. equities, and benchmarked against the SPDR S&P 500 ETF (SPY).  

Click the link to access the rendered HTML file: https://tinyurl.com/4cdu4r47  

### 3 Random Selection, Expertise and Index Investing

The aim of this project is to find out if portfolios made of 15 randomly selected stocks from the S&P500 Large-Cap Index
can outperform stock-picking experts and index investing. The portfolios were optimized based on the mean-variance framework
developed by Harry Markowitz for simplicity, with the goal of maximizing the ex-post Sharpe Ratio during the optimization period.  

The project is meant to be a fun/thought experiment, and is not meant to disprove of index or active investing. It does not
make use of any statistical methods to prove that random selection, expertise or index investing is a better choice.  

Click the link to access the rendered HTML file: https://tinyurl.com/e5ed7mws

### 4 Returns and Risks: The Mean and The Median

The purpose of this project is to find out if using median returns in portfolio optimization could lead to better portfolio 
performance compared to mean returns. Due to the non-normal distribution of returns, the median may be a better optimization 
objective of expected returns than the mean.  

I found that portfolios maximizing the median returns led to better diversification than maximizing the mean returns. 
Furthermore, minimizing the median absolute deviation resulted in a portfolio that has the highest return among portfolios 
minimizing the different risk measures. However, this project does not include an optimal portfolio selection which maximizes 
return for a given level of risk, or minimizes risk for a given level of return. The project does include a few other flaws, 
such as having only a small number of random portfolios generated for testing and not being able to show conclusively that 
the median measure is better than the mean.  

Click the link to access the rendered HTML file: https://tinyurl.com/mryyeu8s

### 5 Testing a Trading Strategy Using a Rolling Window CAPM

This project aims to test if the Capital Asset Pricing Model (CAPM) estimated using a rolling window could allow an investor 
or trader to take advantage of inefficiencies in market pricing. I used a short-term modeling strategy with 5 trading days of 
historical data per estimation to capture the short-term pricing inefficiencies. It does not conclude if the strategy is effective 
for all stocks, but applying the CAPM trading strategy on Apple Inc. was able to yield short-term outperformance. Furthermore, the 
stock had outperformed the S&P 500 ETF over the years but it could be attributed to having chosen a stock that had outperformed 
the market consistently.  

Click the link to access the rendered HTML file: https://tinyurl.com/2p9d26jp
