#' Author: Ted Kwartler
#' Date: 6-18-2018
#' Purpose: Forecasting Basics
#'

# WD
setwd("/cloud/project/Lessons/G_TimeSeries_Equities/data")

# libs
library(dygraphs)

# Data
df <- read.csv('Amtrak.csv')

# Examine
head(df, 10)
class(df$Month)

# Create a time series obj
riders <- ts(df$Ridership, 
             start = c(1991, 1), 
             end = c(2004, 3), 
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

# Bonus: Dealing with Dates
library(lubridate)

# Make a date obj
df$Month[1]
cleanDate <- dmy(df$Month)
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
