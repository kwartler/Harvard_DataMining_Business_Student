#' Author: Ted Kwartler
#' Data: Mar 23 2026
#' Purpose: Decompose Amazon Quarterly Revenue
#' Notes: Students may like this ppt: https://robjhyndman.com/eindhoven/1-3-Seasonality.pdf

# Options
options(scipen=999)

# library
library(forecast)
library(lubridate)
library(ggseas) #seasonal adjustments with ggplot too!
library(timetk)
library(dplyr)
library(feasts)
library(tstibble)

# Data
amzn <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/H_TimeSeries/data/AMZN_Qtr_Rev.csv')
head(amzn)

# Time formatting
amzn$date <- as.POSIXct(amzn$unixTime, origin = '1970-1-1')
head(amzn)

# Change to a time series
stYr  <- year(amzn$date[1])
stQtr <- quarter(amzn$date[1])
st    <- c(stYr, stQtr)
qtrTS <- ts(amzn$revMill, start = st, frequency = 4)
qtrTS

# Now decompose the ts
amznDecomp <- decompose(qtrTS)

# See the trend 
plot(amznT <- amznDecomp$trend)

# See the seasonal
plot(amznS <- amznDecomp$seasonal)

# See the random; does this really look random?!
plot(amznR <- amznDecomp$random)

# turn the ts back into a DF/Tibble
amznDF <- tk_tbl(qtrTS, preserve_index = TRUE, rename_index = "date")
head(amznDF)

# Plot the actual and the trend line on top
ggplot(amznDF) +
  geom_line(aes(x=date, y=value), colour='black', alpha = 0.5) +
  geom_line(aes(x=date, y=amznT), colour='blue') +
  theme_bw() + theme(legend.position="none")

# Plot the actual and add two sections of the time series decomposition.  
# Notice the seasonality is off a lot particularly in the early periods.
# This is because this is a multiplicative series
ggplot(amznDF) +
  geom_line(aes(x=date, y=value), colour='black', alpha = 0.5) +
  geom_line(aes(x=date, y=(amznS+amznT)), colour ='red') +
  theme_bw() + theme(legend.position="none")

# Since the decomposition looked non-random we can try multiplicative decomposition
decompositionMulti <- decompose(qtrTS, type = "multiplicative")
plot(decompositionMulti)


# Extract individual components
trendMult     <- decompositionMulti$trend
head(trendMult) #x = date, y=value, component is section of multiplicative component
seasonalMult  <- decompositionMulti$seasonal
remainderMult <- decompositionMulti$random

# decompose(): additive method is default
# End
