#' Author: Ted Kwartler
#' Data: 6-6-2018
#' Purpose: Decompose Amazon Quarterly Revenue
#' Notes: Students may like this ppt: https://robjhyndman.com/eindhoven/1-3-Seasonality.pdf

# Options
options(scipen=999)

# Wd
setwd("/cloud/project/Lessons/G_TimeSeries_Equities/data")

# library
library(forecast)
library(lubridate)
library(ggseas)

# Data
amzn <- read.csv('AMZN_Qtr_Rev.csv')

# Time formatting
amzn$date <- as.POSIXct(amzn$unixTime, origin = '1970-1-1')

head(amzn)

# Change to a time series
stYr  <- year(amzn$date[1])
stQtr <- quarter(amzn$date[1])
st    <- c(stYr, stQtr)
qtrTS <- ts(amzn$revMill, start = st, frequency = 4)

# Now decompose the ts
amznDecomp <- decompose(qtrTS)

# See the trend 
plot(amznT <- amznDecomp$trend)

# See the seasonal
plot(amznS <- amznDecomp$seasonal)

# See the random
plot(amznR <- amznDecomp$random)

# What is Amazon's revenue without the impact of the holiday shopping season?
noSeasons <- qtrTS - amznS
plot(noSeasons)

# turn the ts back into a DF
amznGG <- tsdf(qtrTS)

# Trend and actual
ggplot(amznGG) +
  geom_line(aes(x=x, y=y), colour='black') +
  geom_line(aes(x=x, y=amznT), colour='blue') +
  theme_bw() + theme(legend.position="none")

# Trend+Seasonal and actual
ggplot(amznGG) +
  geom_line(aes(x=x, y=y), colour='black') +
  geom_line(aes(x=x, y=(amznS+amznT)), colour ='red') +
  theme_bw() + theme(legend.position="none")

# ggseas & ggplot which uses seas, is a form of ARIMA forecasting usually for improved results - captures additive nature
ggsdc(amznGG, aes(x = x, y = y), method = "seas") + geom_line() + theme_bw()
ggsdc(amznGG, aes(x = x, y = y), method = "decompose") + geom_line() + theme_bw()

# Getting a forecast can be done in a naive manner from the components
# Drift the trend component ahead
driftF <- rwf(amznT, drift=T, h=4)

# Naive Seasonal the seasonal component
seasonalF <- snaive(amznS, h=4)

# Then add them together (additive de-seasoning)
naiveComponents  <- as.numeric(seasonalF$mean) + as.numeric(driftF$mean)

revenue <- c(amzn$revMill, naiveComponents)
newDF <- data.frame(indx = seq_along(revenue), 
                    rev = revenue, 
                    type = c(rep('actual', 89), rep('forecast',4)))

# Naive Additive
ggplot(newDF, aes(x=indx, y=rev)) + geom_line() +  geom_point(aes(colour=type)) + theme_bw()

# Zoom in
ggplot(tail(newDF,12), aes(x=indx, y=rev)) + geom_line() +  geom_point(aes(colour=type)) + theme_bw()
# End
