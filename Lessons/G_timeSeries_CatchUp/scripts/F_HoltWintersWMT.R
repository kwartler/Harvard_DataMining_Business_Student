#' Author: Ted Kwartler
#' Data: 10-26-2020
#' Purpose: Exponential Smoothing (Holt Winters) on WalMart Revenue Data

# Options
options(scipen=999)

# Wd
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/G_RF_TimeSeries/data")

# library
library(forecast)
library(lubridate)
library(ggplot2)
library(MLmetrics)

# Data
wmt <- read.csv('WMT_Qtr_Rev.csv')

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
#qtrHW2 <- hw(qtrTS,seasonal = "multiplicative")
#qtrHW3 <- ets(qtrTS, model='MAA')


# Example
plot(qtrHW)
predict(qtrHW, 4)

# Out of time partition
trainWMT <- wmt[1:108,]
trainTS  <- ts(trainWMT$revMill, start = st, frequency = 4)

validationWMT <- wmt[109:120,]
validationTS  <- ts(validationWMT$revMill, start = c(2015, 3), frequency = 4)

# Re-Fit HW with training dta
qtrHW <- HoltWinters(trainTS, seasonal = 'mult')
plot(qtrHW)

# Make predictions on validation
validForecasts <- predict(qtrHW, 12)

# Organize & Compare
(validationDF <- data.frame(idx = seq_along(validForecasts),
                            original = validationWMT$revMill,
                            fit = validForecasts))

# Examine visual fit
ggplot(validationDF) +
  geom_line(aes(x=idx, y=original)) +
  geom_line(aes(x=idx, y=fit), colour='red') + theme_bw()

# Get RMSE
RMSE(validationDF$original, validationDF$fit)
MAPE(validationDF$original, validationDF$fit)

# End