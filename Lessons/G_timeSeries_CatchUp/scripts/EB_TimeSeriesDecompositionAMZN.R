#' Author: Ted Kwartler
#' Data: 10-26-2020
#' Purpose: Decompose Amazon Quarterly Revenue
#' Notes: Students may like this ppt: https://robjhyndman.com/eindhoven/1-3-Seasonality.pdf

# Options
options(scipen=999)

# Wd
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/G_timeSeries_CatchUp/data")

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
qtrTS

# Now decompose the ts
amznDecomp <- decompose(qtrTS)

# See the trend 
plot(amznT <- amznDecomp$trend)

# See the seasonal
plot(amznS <- amznDecomp$seasonal)

# See the random; does this really look random?!
plot(amznR <- amznDecomp$random)

# What is Amazon's revenue without the impact of the holiday shopping season?
noSeasons <- qtrTS - amznS
plot(noSeasons)

# turn the ts back into a DF
amznDF <- tsdf(qtrTS)

# Trend and actual
ggplot(amznDF) +
  geom_line(aes(x=x, y=y), colour='black') +
  geom_line(aes(x=x, y=amznT), colour='blue') +
  theme_bw() + theme(legend.position="none")

# Trend+Seasonal and actual
ggplot(amznDF) +
  geom_line(aes(x=x, y=y), colour='black') +
  geom_line(aes(x=x, y=(amznS+amznT)), colour ='red') +
  theme_bw() + theme(legend.position="none")

# ggseas & ggplot which uses seas, is a form of ARIMA forecasting usually for improved results - captures additive nature
# Does the "random" irregular look more like noise?
ggsdc(amznDF, aes(x = x, y = y), method = "seas") + geom_line() + theme_bw()
# Compare
ggsdc(amznDF, aes(x = x, y = y), method = "decompose") + geom_line() + theme_bw()


# End
