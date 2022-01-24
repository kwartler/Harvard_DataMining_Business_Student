#' Author: Ted Kwartler
#' Data: 10-25-2021
#' Purpose: Decompose the impact of the pandemic
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

# Trend+Seasonal and actual
# turn the ts back into a DF
nkeDF <- tsdf(qtrTS)
ggplot(nkeDF) +
  geom_line(aes(x=x, y=y), colour='black') +
  geom_line(aes(x=x, y=(nkeS+nkeT)), colour ='red') +
  theme_bw() + theme(legend.position="none")

nkeDF$seasonal <- nkeS
nkeDF$trend    <- nkeT
nkeDF$SeasonalTrendDecompose <- nkeS + nkeT
badQtr <- subset(nkeDF, nkeDF$x==2020.25 )

# Impact to Rev from Covid from seasonally adjusted forecast
badQtr$seasonal + badQtr$trend #9423.7842 Expected 2020 Q2
badQtr$y - (badQtr$seasonal + badQtr$trend)#$3.110784B, in Q2 the seasonality is ~-25
# End
