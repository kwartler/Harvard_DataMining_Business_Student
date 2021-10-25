#' Author: Ted Kwartler
#' Data: 10-26-2020
#' Purpose: Naive Forecast for Nike
#' 

# Options
options(scipen=999)

# library
library(jsonlite)
library(lubridate)
library(dygraphs)
library(forecast)
library(ggseas)

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
qtrTS

# Visualize
dygraph(qtrTS, main = paste('Quarterly Rev for', stock, '1000M = 1B')) %>% 
  dyRangeSelector(height = 20, strokeColor = "") 

# Now decompose the ts
nkeDecomp <- decompose(qtrTS)

# See the trend 
plot(nkeT <- nkeDecomp$trend)

# See the seasonal
plot(nkeS <- nkeDecomp$seasonal)

# See the random; does this really look random?!
plot(nkeR <- nkeDecomp$random)


# 
nkeS + nkeT #9423.7842 Expected 2020 Q2
qtrTS - (nkeS + nkeT)   #Actual outcome - Expected in that quarter -3110.784159 $3B

# End


