#' Author: Ted Kwartler
#' Data: 7-8-2018
#' Purpose: Quick Auto.Arima
#' 

# Options
options(scipen=999)

# Libs
library(forecast)
library(tseries)
library(MLmetrics)

# Data
amzn <- read.csv('AMZN_Qtr_Rev.csv')

# Time formatting
amzn$date <- as.POSIXct(amzn$unixTime, origin = '1970-1-1')

head(amzn)

# Change to a time series
stYr <- year(amzn$date[1])
stQtr <- quarter(amzn$date[1])
st<- c(stYr, stQtr)
qtrTS <- ts(amzn$revMill, start = st, frequency = 4)
plot(qtrTS)

# Visually it is not stationary but let's check
# Augmented Dickey-Fuller (ADF)
adf.test(amzn$revMill, alternative = "stationary")
# .99 does not reject the null hypothesis of non-stationarity, confirming our previous visual inspection

# Differencing is the method to remove trend/seasonality
# Difference is calculated by subtracting one period's values from the previous period's 
countD1 <- diff(qtrTS, differences = 1)
adf.test(countD1, alternative = "stationary")

countD2 <- diff(qtrTS, differences = 2)
adf.test(countD2, alternative = "stationary")

# Time aware partition
trainTS <- window(qtrTS, start = c(1996, 1), 
                   end  = c(2016, 2))
validTS <- window(qtrTS, start = c(2016, 3),
                   end = c(2018, 1))

# auto.arima should take care of the differencing while also determing p and d params
fit <- auto.arima(trainTS)
pred <- forecast(fit, 12) #2yrs validation and 1yr forward

# KPI
RMSE(as.numeric(trainTS), fit$fitted)
MAPE(as.numeric(trainTS), fit$fitted)

# Plot: Blue forecast validation and future, green: fit, red: validation actuals
plot(pred)
lines(validTS, col='red',lwd = 2)
lines(fit$fitted, col='green', lwd=2)

# End


