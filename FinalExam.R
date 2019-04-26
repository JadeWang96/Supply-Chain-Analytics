# @ Jade Wang
# 04/22/2019

rm(list=ls())
library(forecast)
# Initialize data
sales <- c(40,38,36,37,39,39,42,42,40,37,36,38,
           40,42,43,42,39,40,42,62,40,38,37,41,
           44,43,39,39,39,37,39,31,38,40,41,42,
           38,39,53)
# Time series
TS_sales <- ts(sales, start = c(2016,1), frequency = 12)

# ARIMA
sales_model1 = ets(TS_sales)
sales_model2 = auto.arima(TS_sales)
sales_ets = forecast(sales_model1, h=6)
sales_arima = forecast(sales_model2, h=6)
sales_ets
sales_arima

# Plot the graphs
plot(sales_ets)
plot(sales_arima)