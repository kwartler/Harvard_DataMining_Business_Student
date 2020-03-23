#' Author: Ted Kwartler
#' Data: 6-6-2018
#' Purpose: Exponential Smoothing (Holt Winters) on WalMart Revenue Data

# Libraries
library(forecast)

# Data Input
cvsRev <- read.csv('CVSrevenue.csv')

# Time formatting
cvsRev$date <- as.POSIXct(cvsRev$unixTime, origin = '1970-1-1')

# Change to a time series
stYr <- year(cvsRev$date[1])
stQtr <- quarter(cvsRev$date[1])
st<- c(stYr, stQtr)
qtrTS <- ts(cvsRev$revMill, start = st, frequency = 4)

# Add quarterly dummies, takes care of dropping a season
quarts <- seasonaldummy(qtrTS)
head(quarts,10)

# Trend can be captured just by an index
trendIdx <- seq_along(cvsRev$revMill)

# Add an event; CVS bought Caremark
acquisition <- c(rep(0,76),rep(1,120-76))

# Organize
modelMat <- as.data.frame(cbind(y = log(cvsRev$revMill),trendIdx, (trendIdx^2),quarts, acquisition))
fit <-lm(y ~., modelMat)
summary(fit)

# Viz
pred <- exp(predict(fit))
plot(pred, col='blue')
lines(pred, col='blue')
lines(cvsRev$revMill, col='red')

# Although they have higher rev, the model demonstrates the revenue underperformance prior (blue spikes above red) or perhaps the acquisition let CVS smooth out their revenue

# End
                 