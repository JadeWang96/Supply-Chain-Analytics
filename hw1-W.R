#ISYE 6600 HW1
rm(list=ls())
library("TTR")
library("forecast")
setwd("D:\\RPI\\2019Spring\\Design of Manufacturing Systems and Supply Chains\\Homework\\HW1")

##Question2
#2A
my_data <- scan("hamburger2019.dat", skip = 1)
hamburger_ts <- ts(my_data)
#Moving Average with period of 3
#105-110
hamburger_tsSMA3 <- SMA(hamburger_ts, n=3) 
hamburger_tsSMA3[104]

#2B
#Moving Average with period of 4
#105-110
hamburger_ts <- ts(my_data)
hamburger_tsSMA4 <- SMA(hamburger_ts, n=4) 
hamburger_tsSMA4[104]

#2C
SSE_MA3 <- 0
for(i in 4:104)
{
  SSE_MA3 = SSE_MA3 + (hamburger_ts[i] - hamburger_tsSMA3[i-1])^2
}
SSE_MA3

SSE_MA4 <- 0
for(i in 5:104)
{
  SSE_MA4 = SSE_MA4 + (hamburger_ts[i] - hamburger_tsSMA4[i-1])^2
}
SSE_MA4

#2D
# Simple Exponential Smoothing
hamburger_ts <- ts(my_data)
hamburger_SimpleExpSmoothing <- HoltWinters(hamburger_ts, beta = FALSE, gamma = FALSE)
hamburger_SimpleExpSmoothing$SSE
hamburgerMSE = hamburger_SimpleExpSmoothing$SSE / 104
hamburgerMSE
hamburger_SimpleExpSmoothing$alpha
hamburger_SimpleExpSmoothing$fitted

#2E
##Simple Exponential Smoothing FORECASTING
hamburger_SimpleExpSmoothingf <- forecast:::forecast.HoltWinters(hamburger_SimpleExpSmoothing, 
                                                                h = 6, level = 95)
hamburger_SimpleExpSmoothingf

#2F
plot(hamburger_SimpleExpSmoothing)
plot(hamburger_SimpleExpSmoothingf)



##Question3
my_data2 <- scan("pizza2019.dat", skip = 1)
pizza_ts <- ts(my_data2)
#3A
##Exponential Smoothing with Trend
pizzaExpSmoothingTrend <- HoltWinters(pizza_ts, gamma = FALSE) 
pizzaExpSmoothingTrend$SSE
pizzaMSE = pizzaExpSmoothingTrend$SSE / 104
pizzaMSE
pizzaExpSmoothingTrend$alpha
pizzaExpSmoothingTrend$beta
pizzaExpSmoothingTrend$fitted

#3B
##Exponential Smoothing with a TREND FORECASTING
pizzaforecastExpSmoothingTrend <- forecast:::forecast.HoltWinters(pizzaExpSmoothingTrend,
                                                                  h = 6, level = 95)
pizzaforecastExpSmoothingTrend

#3C
plot(pizzaExpSmoothingTrend)
plot(pizzaforecastExpSmoothingTrend)


##Question4
my_data3 <- scan("icecream2019.dat", skip = 1)
#4A
plot(my_data3)
#4B
icecream_ts <- ts(my_data3, frequency = 4)
#winters holt method (trend and seaonsality)
icecream_ts
icecreamWinters <- HoltWinters(icecream_ts, seasonal = "multiplicative")
plot(icecreamWinters)
icecreamWinters$SSE
icecreamMSE = icecreamWinters$SSE / 20
icecreamMSE
icecreamWinters$alpha
icecreamWinters$beta
icecreamWinters$gamma
icecreamWinters$fitted

#4C
##WINTERS FORECASTING
icecreamForecast <- forecast:::forecast.HoltWinters(icecreamWinters, h=10)
icecreamForecast

#4D
plot(icecreamWinters)
plot(icecreamForecast)


