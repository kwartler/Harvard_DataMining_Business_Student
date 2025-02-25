#' Author: Ted Kwartler
#' Data: 10-28-2023
#' Purpose: Grab some time series revenue data and visualize it
#' https://ycharts.com/companies/AMZN/revenues

# Options
options(scipen=999)

# library
library(lubridate)
library(xts)
library(ggplot2)
library(ggthemes)

# read in Data
qtrDF <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/G_TimeSeries/data/AMZN_Qtr_Rev.csv')
tail(qtrDF)

# Change to date
qtrDF$date  <- as_datetime(qtrDF$unixTime)
tail(qtrDF)

# Change to a time series
stYr  <- year(qtrDF$date[1])
stQtr <- quarter(qtrDF$date[1])
st    <- c(stYr, stQtr)

# Base R time series
qtrTS2 <- ts(qtrDF$revMill, start = st, frequency = 4)
qtrTS2

# Basic ggplot2
p <- ggplot(qtrDF, aes(x=date, y=revMill)) +
  geom_line() + 
  xlab("") + 
  theme_hc() + 
  ggtitle('amzn qtr rev')
p

# End
