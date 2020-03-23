#' Author: Ted Kwartler
#' Data: 3-23-2020
#' Purpose: Grab some time series revenue data and visualize it
#' 

# Options
options(scipen=999)

# library
library(jsonlite)
library(lubridate)
library(xts)
library(ggplot2)

# Choose a single stock ticker
stock <-'AMZN' #CVS 

# Open a browser with this URL to see the chart of data
(paste0('https://ycharts.com/companies/',stock,'/revenues'))

# Get the raw data under the chart
(stockURL <- paste0('https://ycharts.com/charts/fund_data.json?securities=include%3Atrue%2Cid%3A',stock,'%2C%2C&calcs=include%3Atrue%2Cid%3Arevenues%2C%2C'))

# Send R to go get it
stockQtrRev <- fromJSON(stockURL)

# Review
(rawData <- stockQtrRev$chart_data[[1]]$raw_data)

# Easier data type
qtrDF <- data.frame(unixTime = rawData[[1]][,1],
                    revBill  = rawData[[1]][,2])

head(qtrDF)

# Change to date
qtrDF$date  <- as.POSIXct(qtrDF$unixTime, origin = '1970-1-1')
tail(qtrDF)

# Change to a time series
stYr  <- year(qtrDF$date[1])
stQtr <- quarter(qtrDF$date[1])
st    <- c(stYr, stQtr)

# Base R time series
qtrTS2 <- ts(qtrDF$revBill, start = st, frequency = 4)

# Basic ggplot2
p <- ggplot(qtrDF, aes(x=date, y=revBill)) +
  geom_line() + 
  xlab("")
p

# End