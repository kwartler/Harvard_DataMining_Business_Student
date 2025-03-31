#' Author: Ted Kwartler
#' Data: 10-29-2023
#' Purpose: Decompose Amazon Quarterly Revenue
#' Notes: Students may like this ppt: https://robjhyndman.com/eindhoven/1-3-Seasonality.pdf

# Options
options(scipen=999)

# Wd
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# library
library(forecast)
library(lubridate)
library(ggseas) #seasonal adjustments with ggplot too!

# Data
amzn <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/G_TimeSeries/data/AMZN_Qtr_Rev.csv')
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

# turn the ts back into a DF
amznDF <- tsdf(qtrTS)
head(amznDF)

# Plot the actual and the trend line on top
ggplot(amznDF) +
  geom_line(aes(x=x, y=y), colour='black', alpha = 0.5) +
  geom_line(aes(x=x, y=amznT), colour='blue') +
  theme_bw() + theme(legend.position="none")

# Plot the actual and add two sections of the time series decomposition.  
# Notice the seasonality is off a lot particularly in the early periods.
# This is because this is a multiplicative series
ggplot(amznDF) +
  geom_line(aes(x=x, y=y), colour='black', alpha = 0.5) +
  geom_line(aes(x=x, y=(amznS+amznT)), colour ='red') +
  theme_bw() + theme(legend.position="none")

# ggseas & ggplot which uses seasonal differences, is a form of ARIMA forecasting usually for improved results - captures additive nature
# Does the "random" irregular look more like noise?
ggsdc(amznDF, aes(x = x, y = y), method = "seas") + geom_line() + theme_bw()
# Compare
ggsdc(amznDF, aes(x = x, y = y), method = "decompose") + geom_line() + theme_bw()

# To extract the multiplicative values we can use
decompositionMulti <- ggsdc(amznDF, aes(x = x, y = y), method = "seas")

# Extract individual components
trendMult     <- subset(decompositionMulti$data,
                        decompositionMulti$data$component=='trend')
head(trendMult) #x = date, y=value, component is section of multiplicative component
seasonalMult  <- subset(decompositionMulti$data,
                        decompositionMulti$data$component=='seasonal')
remainderMult <- subset(decompositionMulti$data,
                        decompositionMulti$data$component=='irregular')

# decompose(): additive method
# ggseas::ggsdc(): multiplicative method using ARIMA (autoregressive (AR), differencing (I), and moving average (MA) for the components.  AR - past values, I - removes trend, MA - reduces noise



# End
