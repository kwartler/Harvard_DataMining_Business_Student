#' Author: Ted Kwartler
#' Date: 10-29-2023
#' Purpose: Forecasting Basics
#'

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# libs
library(dygraphs)
library(lubridate)

# Data
df <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/G_TimeSeries/data/Amtrak.csv')

# Examine
head(df, 10)
tail(df,2)
class(df)
class(df$Month)

# Create a time series obj
riders <- ts(df$Ridership, 
             start     = c(1991, 1), 
             end       = c(2004, 3), 
             frequency = 12)

riders
class(riders)

# Level
riderLev <- mean(df$Ridership)

# Basic Viz
plot(riders, xlab = 'Months', ylab = "monthly Ridership in 000s")
abline(a = riderLev, b = 0, col='red')

# Dynamic to see cyclicality
dygraph(riders, main = "Amtrack Ridership") %>% 
  dyRangeSelector()

## Dealing with Dates; be very careful, dont assume month, day, yr!
# Make a date obj
df$Month
cleanDate <- dmy(df$Month)
tail(df$Month, 10)
tail(cleanDate, 10)

# Chk class
class(cleanDate)

# Extract as standalone data vecs
df$day   <- day(cleanDate)
df$month <- month(cleanDate)
df$yr    <- year(cleanDate)

head(df)

# Calculate date differences w/difftime()
Sys.Date()
cleanDate[1]
difftime(Sys.Date(), cleanDate[1])
difftime(Sys.Date(), cleanDate[1], units = 'weeks')

# Nest difftime to engineer new data vectors
df$daysPassed   <- as.numeric(difftime(Sys.Date(), cleanDate))
df$monthsPassed <- as.numeric(difftime(Sys.Date(), cleanDate, units="weeks")) /4
df$yrsPassed    <- as.numeric(difftime(Sys.Date(), cleanDate, unit="weeks"))/52.25
head(df)

# End
