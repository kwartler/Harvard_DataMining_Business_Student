#' Author: Ted Kwartler
#' Data: 6-6-2018
#' Purpose: Naive Forecast for Nike
#' 

# Options
options(scipen=999)

# library
library(jsonlite)
library(lubridate)
library(dygraphs)
library(forecast)

stock <-'NKE'

# Construct the JSON URL and get the data
stockURL <- paste0('https://www.gurufocus.com/modules/chart/interactive_chart_json_morn.php?symbol=',
                   'NYSE:',stock,'&fp=q&ser=Revenue')
stockQtrRev <- fromJSON(stockURL)

# Adjust time from Unix epoch
unixTime <-  stockQtrRev[[2]][,1]/1000
revDate  <- as.POSIXct(unixTime, origin = '1970-1-1')

# Organize the list into a DF, keep in mind this site is inconsistent with milllions and billions
qtrDF <- data.frame(date = revDate, revMill = stockQtrRev[[2]][,2])

head(qtrDF)

# Change to a time series
stYr  <- year(qtrDF$date[1])
stQtr <- quarter(qtrDF$date[1])
st    <- c(stYr, stQtr)
qtrTS <- ts(qtrDF$revMill, start = st, frequency = 4)

# Visualize
dygraph(qtrTS, main = paste('Quarterly Rev for', stock, '1000M = 1B')) %>% 
  dyRangeSelector(height = 20, strokeColor = "") 

# Naive -  Mean forecast 12 quarters ahead
meanTS <- meanf(qtrTS, h=12) 
plot(meanTS)

# Compare to actual mean avg
mean(qtrDF$revMill)

# Naive - Drift "random walk forecast"
driftTS <- rwf(qtrTS, drift=T, h=12)
plot(driftTS)

# Naive - true
naiveTS <- naive(qtrTS, h=12)
plot(naiveTS)

# Naive - seasonal
seasonalNaive <- snaive(qtrTS, h=12)
plot(seasonalNaive)


# End

