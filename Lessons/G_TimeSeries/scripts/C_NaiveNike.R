#' Author: Ted Kwartler
#' Data: 10-29-2023
#' Purpose: Naive Forecast for Nike
#' 

# Options
options(scipen=999)

# library
library(lubridate)
library(dygraphs)
library(forecast)

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Data
stockQtrRev <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/G_TimeSeries/data/nike_qtr_rev.csv')
stockQtrRev$date <- gsub(' 1:00','',stockQtrRev$date)
stockQtrRev$date <- mdy(stockQtrRev$date)

head(stockQtrRev)
tail(stockQtrRev)

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
mean(stockQtrRev$revMill, na.rm = T)
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

# Of course shocks to the system are problematic for naive methods
fakeTS <- window(qtrTS, end = c(2020,4))
seasonalNaive <- snaive(fakeTS, h=12)
plot(seasonalNaive)

# End

