# Mileris Justinas 2020-01-15

# Google trends api gives popularity data of searching keyword.
# For this project two data sets were combined:
# 1) Apple highest stock price of the day (Kaggle dataset)
# 2) Google searching popularity of keyword 'iphone' (globaly searches in google website)
# Google trends api gives data of each sunday (in our case)
# Stock markets are open on working days, therefore stocks dataset consists only of working days data.
# Solution - in this project we combined google trend sunday data with monday (next day) highest stock price data.

# Stock prices (Apple) taken from : 
# https://www.kaggle.com/abdullahmu/big-five-stocks

# After preprocessing datasets data range are (2009-08-31 : 2019-08-19)

###################################################################################

library(ggplot2)
library(lubridate) 
library(gtrendsR)
library(dplyr)
library(tidyverse)
library("fpp")

set.seed(42)

###################################################################################
# UPLOAD DATA ###########################################################
###################################################################################

data <- read.csv(file = "/home/ubuntu/Desktop/time-series-ind/apple_stocs_and_gtrend_searches_iphone.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")

plot(data$date, data$stock_price, type = "l", col="red", main = "Apple stocks", xlab="Year", ylab="Stock price")
grid(nx=9, ny = NULL, col = "gray", lty = 4)



plot(data$date, data$search_popularity, type = "l", col="blue", main = "Global Google search popularity for keyword 'iphone'", xlab="Year", ylab="Search popularity")
grid(nx=9, ny = NULL, col = "gray", lty = 4)



str(data)
summary(data)

head(data, 3)
tail(data, 3)

# Both data sets on the same plot
# plot(data$date, data$stock_price, type = "l", col="red", main = "Apple stocks (red) + Google search 'iphone' (blue)", xlab="Year", ylab="Value")
# lines(data$date, data$search_popularity, type = "l", col="blue")



acf(data$stock_price)
pacf(data$stock_price)

spectrum(data$stock_price)

acf(data$search_popularity)
spectrum(data$search_popularity)



create_ts <- function(df) {
  df %>%
    # filter(date >= "2009-08-30",
    #        date <  "2019-08-27",
    #        name == "AAPL")
    # %>%
    with(ts(stock_price, start = c(2009, 3), end = c(2019, 3), frequency = 52))
}
ts_data_stocks <- create_ts(data)
ts_data_stocks
fit <- decompose(ts_data_stocks)
plot(fit)

#################################################################################
# ARIMA
fit2 <- auto.arima(ts_data_stocks, xreg=data$search_popularity, seasonal=TRUE, D=1)
fit2

fcast <- forecast(fit2, xreg=data$search_popularity)
plot(fcast, main="Apple stocks prediction")
abline(h=0, col="red")

fcast

ts_data_stocks

#################################################################################
#################################################################################
#################################################################################

ts_data_stocks_exp <- window(ts_data_stocks)
plot(ts_data_stocks_exp, ylab="Apple stock price", xlab="Year")

# Exponential smoothing
fit_exp_1 <- ses(ts_data_stocks_exp, alpha=0.1, h=10)
fit_exp_1$model
fit_exp_2 <- ses(ts_data_stocks_exp, alpha=0.7, h=10)
fit_exp_2$model

lines(fitted(fit_exp_1), col="blue")
lines(fitted(fit_exp_2), col="red")

# Forecast
lines(fit_exp_1$mean, col="blue")
lines(fit_exp_2$mean, col="red")
legend("topleft",lty=1, col=c(1,"blue","red"),
       c("data", expression(alpha == 0.1), expression(alpha == 0.7)), pch=1)

#################################################################################

# Double exponential smoothing
fit_exp_double_1 <- holt(ts_data_stocks_exp, alpha=0.5, beta=0.2, h=10)
fit_exp_double_2 <- holt(ts_data_stocks_exp, alpha=0.5, beta=0.2, damped=TRUE, h=10)

plot(ts_data_stocks_exp, ylab="Apple stock price", xlab="Year")
lines(fitted(fit_exp_double_1), col="green")
lines(fitted(fit_exp_double_2), col="red")
lines(fit_exp_double_1$mean, col="green")
lines(fit_exp_double_2$mean, col="red"
      )
legend("topleft", lty=1, col=c("black","green","red"),
       c("Data","Linear trend","Additive damped trend"))



plot(ts_data_stocks_exp, xlim=c(2017, 2019.5), ylab="Apple stock price", xlab="Year")
lines(fitted(fit_exp_double_1), col="green")
lines(fitted(fit_exp_double_2), col="red")
lines(fit_exp_double_1$mean, col="green")
lines(fit_exp_double_2$mean, col="red"
)
legend("topleft", lty=1, col=c("black","green","red"),
       c("Data","Linear trend","Additive damped trend"))

#################################################################################
#################################################################################
#################################################################################

# Naive
fit_naive <- forecast(ts_data_stocks, method="naive", h=50)
plot(fit_naive, ylab="Production")
# SNaive
fit_snaive <- snaive(ts_data_stocks, h=50)
plot(fit_snaive, ylab="Production")

# ARIMAX
accuracy(fcast)
# Exponential smoothing
accuracy(fit_exp_1)
accuracy(fit_exp_2)
accuracy(fit_exp_double_1)
accuracy(fit_exp_double_2)
# Naive
accuracy(fit_naive)
# SNaive
accuracy(fit_snaive)

#################################################################################
#################################################################################
#################################################################################

acf(residuals(fit2))
Box.test(residuals(fit2), lag=30, type="Ljung-Box")
adf.test(residuals(fit2))

acf(residuals(fit_exp_double_1))
Box.test(residuals(fit_exp_double_1), lag=30, type="Ljung-Box")
adf.test(residuals(fit_exp_double_1))

#################################################################################
#################################################################################
#################################################################################

m1 <- function(x, h){
  forecast(auto.arima(x, xreg=data$search_popularity, seasonal=TRUE, D=1),h=h)
}
res1 <- tsCV(ts_data_stocks, forecastfunction=m1, h=30)
plot(colMeans(res1^2, na.rm=TRUE), type="l", col="red", ylim=c(0, 100), ylab="Paklaida")



m2 <- function(x, h){
  forecast(ses(x, alpha=0.999, h=h),h=h)
}
res2 <- tsCV(ts_data_stocks, forecastfunction=m2, h=30)
plot(colMeans(res2^2, na.rm=TRUE), col="red", type="l", ylim=c(0, 600), ylab="Paklaida")
