#' Author: Ted Kwartler
#' Data: 3-21-2022
#' Purpose: Naive Forecast for Nike
#' 

# Options
options(scipen=999)

# library
library(lubridate)
library(dygraphs)
library(forecast)

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/G_RF_TimeSeries/data")

# Data
stockQtrRev <- read.csv('nike_qtr_rev.csv')
stockQtrRev$date <- gsub(' 1:00','',stockQtrRev$date)
stockQtrRev$date <- mdy(stockQtrRev$date)

# Change to a time series
stYr  <- year(stockQtrRev$date[1])
stQtr <- quarter(stockQtrRev$date[1])
st    <- c(stYr, stQtr)
qtrTS <- ts(stockQtrRev$revMill, start = st, frequency = 4)
qtrTS

# Visualize
dygraph(qtrTS, main = 'Quarterly Rev for Nike') %>% 
  dyRangeSelector(height = 20, strokeColor = "") 

# Naive -  Mean forecast 12 quarters ahead
meanTS <- meanf(qtrTS, h=12) 
plot(meanTS)
dev.off()


# Compare to actual mean avg
mean(stockQtrRev$revMill)
tail(meanTS$fitted)

# Naive - Drift "random walk forecast"; just get the trend
driftTS <- rwf(qtrTS, drift=T, h=12)
plot(driftTS)

# Naive - take the last number and repeat it
naiveTS <- naive(qtrTS, h=12)
plot(naiveTS)

# Naive - seasonal, repeat the last periodic pattern
seasonalNaive <- snaive(qtrTS, h=12)
plot(seasonalNaive)

# End

