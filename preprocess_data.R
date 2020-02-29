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

library(lubridate) 
library(gtrendsR)
library(dplyr)
library(tidyverse)

###################################################################################
# GOOGLE TRENDS DATA ##############################################################
###################################################################################
keyword <- "iphone"
# If requested for data range longer than 5y, google trends api gives data of each month
# If requested for data range less   than 5y, google trends api gives data of each week
# Therefore, it is needed to require for 2 times for ~5y to get 10y data of each week
trends_1_bunch <- gtrends(keyword, time = "2009-08-30 2014-08-30", onlyInterest = TRUE)
trends_2_bunch <- gtrends(keyword, time = "2014-08-31 2019-08-18", onlyInterest = TRUE)

data_gtrends <- rbind(trends_1_bunch$interest_over_time, trends_2_bunch$interest_over_time)
data_gtrends$date <- ymd(data_gtrends$date) + 1

# COLUMN RENAME
data_gtrends <- data_gtrends %>% 
  rename(
    search_popularity = hits
  )

filter_gtrend_data <- function(df, kw){
  df %>%
    filter(date    >= "2009-08-30",
           date    < "2019-08-20",
           keyword == kw)
}
data_gtrends <- filter_gtrend_data(data_gtrends, keyword)

###################################################################################
# STOCKS DATA UPLOAD AND PREPROCESS ###############################################
###################################################################################
data_stocks <- read.csv(file = "/home/ubuntu/Desktop/time-series/big_five_stocks.csv")

# DATA PREPROCESS
data_stocks[is.na(data_stocks)] <- 0 # Actually there is no empty data in this data set
data_stocks$date <- as.Date(data_stocks$X, format = "%Y-%m-%d")

# COLUMN RENAME
data_stocks <- data_stocks %>% 
  rename(
    stock_price = high
  )

keep_columns <- c("date", "name", "stock_price")
data_stocks <- data_stocks[keep_columns]

filter_stocks_data <- function(df){
  df %>%
    filter(date >= "2009-08-30",
           date <  "2019-08-27",
           name == "AAPL")
}
data_stocks <- filter_stocks_data(data_stocks)

###################################################################################
# COMBINE STOCKS AND GOOGLE TRENDS DATA ###########################################
###################################################################################

data_gtrends$stock_price=0
colSums(data_gtrends < 1)
for (i in c(1:length(data_gtrends[[1]]))) {
  tmpNumber <- ymd(data_gtrends[["date"]][i])
  index <- which(data_stocks[["date"]] == as.Date(tmpNumber), arr.ind=TRUE)
  if (length(index)>0 && is.numeric(index)) {
    data_gtrends$stock_price[i] <- data_stocks$stock_price[index]
  } else {
    tmpNumber <- ymd(data_gtrends[["date"]][i])+1
    index <- which(data_stocks[["date"]] == as.Date(tmpNumber), arr.ind=TRUE)
    if (length(index)>0 && is.numeric(index)) {
      data_gtrends$stock_price[i] <- data_stocks$stock_price[index]
    } else {
      print("In stocks data set 3 DAYS IN ROW ARE THE SAME")
      tmpNumber <- ymd(data_gtrends[["date"]][i])+2
      index <- which(data_stocks[["date"]] == as.Date(tmpNumber), arr.ind=TRUE)
      if (length(index)>0 && is.numeric(index)) {
        data_gtrends$stock_price[i] <- data_stocks$stock_price[index]
      } else {
        print("In stocks data set 4 DAYS IN ROW ARE THE SAME")
        tmpNumber <- ymd(data_gtrends[["date"]][i])+3
        index <- which(data_stocks[["date"]] == as.Date(tmpNumber), arr.ind=TRUE)
        data_gtrends$stock_price[i] <- data_stocks$stock_price[index]
      }
    }
  }
}

data_final <- data_gtrends

colSums(data_final < 1)
keep_final_columns <- c("date", "stock_price", "search_popularity")
data_final <- data_final[keep_final_columns]

write.csv(data_final, "/home/ubuntu/Desktop/time-series-ind/apple_stocs_and_gtrend_searches_iphone.csv", row.names = FALSE)
