#' Author: Ted Kwartler
#' Data: 5-17-2018
#' Purpose: Grab some time series revenue data and visualize it
#' 

# Options
options(scipen=999)

# library
library(jsonlite)
library(lubridate)
library(dygraphs)

# Choose a single stock ticker
# Drug Stores : CVS (CVS),      RAD (Rite Aid),           WAG (Walgreen's)
# Electronics : BBY (Best Buy), GME (Gamestop),           HGG (HH Greg)
# Grocery     : SWY (Safeway),  KR (Kroger),              GAP (A & P)
# Home Goods  : PIR (Pier 1),   WSM (Williams & Sonoma),  BBBY (Bed Bath & Beyond)
# Home Improve: HD (Home Depot), LOW (Lowe's)
stock <-'AMZN'

# Open a browser with this URL; NOTE THE EXCHANGE "NAS" or "NYSE"
(paste0('https://www.gurufocus.com/financials/', stock))

# Open a browser with this URL to see the chart of data
(paste0('https://www.gurufocus.com/chart/', stock))

# Construct the JSON URL and get the data
# Be sure to change the exchange if needed NYSE or NAS
stockURL <- paste0('https://www.gurufocus.com/modules/chart/interactive_chart_json_morn.php?symbol=',
                   'NAS', #Change to the correct exchange in between quotes
                   ':',stock,'&fp=q&ser=Revenue')
stockURL
stockQtrRev <- fromJSON(stockURL)

# Adjust time from Unix epoch
unixTime <-  stockQtrRev[[2]][,1]/1000
revDate <- as.POSIXct(unixTime, origin = '1970-1-1')

# Organize the list into a DF, keep in mind this site is inconsistent with milllions and billions
qtrDF <- data.frame(date = revDate, revMill = stockQtrRev[[2]][,2])

head(qtrDF)

# Change to a time series
stYr <- year(qtrDF$date[1])
stQtr <- quarter(qtrDF$date[1])
st<- c(stYr, stQtr)
qtrTS <- ts(qtrDF$revMill, start = st, frequency = 4)

# Visualize like site
dygraph(qtrTS, main = paste('Quarterly Rev for', stock)) %>% 
  dyRangeSelector(height = 20, strokeColor = "") 

# End