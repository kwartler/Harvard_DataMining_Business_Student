#' Author: Ted Kwartler
#' Date: 3-23-2020
#' Purpose: Forecasting Basics
#'

# WD
setwd("/cloud/project/Lessons/G_TimeSeries/data")

# libs
library(dygraphs)

# Data
df <- read.csv('Amtrak.csv')

# Examine
head(df, 10)
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

# Bonus: Dealing with Dates
library(lubridate)

# Make a date obj
df$Month[1]
wrongData <- mdy(df$Month) ##HINT: WRONG!

# Now Check
tail(df$Month, 10) #why would it just be the first 12 days of a month?
tail(wrongData, 10)

# Recreate correctly
df$Month[1:13]
cleanData <- dmy(df$Month)
tail(df$Month, 10)
tail(cleanData, 10)

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
