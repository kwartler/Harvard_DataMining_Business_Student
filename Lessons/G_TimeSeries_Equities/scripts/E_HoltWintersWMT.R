#' Author: Ted Kwartler
#' Data: 10-26-2020
#' Purpose: Exponential Smoothing (Holt Winters) on WalMart Revenue Data

# Options
options(scipen=999)

# Wd
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# library
library(forecast)
library(lubridate)
library(ggplot2)
library(MLmetrics)
library(readr)

# Data
wmt <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/G_TimeSeries_Equities/data/WMT_Qtr_Rev.csv')

# Time formatting
wmt$date <- as.POSIXct(wmt$unixTime, origin = '1970-1-1')

head(wmt)

# Change to a time series
stYr  <- year(wmt$date[1])
stQtr <- quarter(wmt$date[1])
st    <- c(stYr, stQtr)
qtrTS <- ts(wmt$revMill, start = st, frequency = 4)
qtrTS

# Looks multiplicative
plot(qtrTS)

# Apply HW
# use hw() from forecast package to get prediction intervals, HoltWinters() is from stats package
# Book Authors use ets from forecast with model='MAA'
qtrHW <- HoltWinters(qtrTS, seasonal = "mult") 

# Example
plot(qtrHW)
predict(qtrHW, 4)

# Out of time partition
trainTS <- window(qtrTS, end = c(2014,4))
validationTS <- window(qtrTS, start = c(2015,1))

# Re-Fit HW with training dta
qtrHW <- HoltWinters(trainTS, seasonal = 'mult')
plot(qtrHW)

# Make predictions on validation
validForecasts <- predict(qtrHW, length(validationTS))

# Organize & Compare
(validationDF <- data.frame(idx = seq_along(validForecasts),
                            original = as.vector(validationTS),
                            fit = validForecasts))

# Examine visual fit
ggplot(validationDF) +
  geom_line(aes(x=idx, y=original), color = 'black', alpha = 0.5) +
  geom_line(aes(x=idx, y=fit), colour='red') + theme_bw()

# Get RMSE
RMSE(validationDF$original, validationDF$fit)
MAPE(validationDF$original, validationDF$fit)

# End