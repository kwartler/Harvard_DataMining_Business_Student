#' Author: Ted Kwartler
#' Data: 10-29-2023
#' Extra TS data practice
#' Load and convert the CVS data to TS
#' Using TSD understand the impact of Covid on retail sales at CVS. Compare the shape of this revenue to NIKE and reflect on the impact of covid for the two companies.

# Library
library(lubridate)
library(forecast)

cvsDF <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/G_TimeSeries_Equities/challenge!/CVS_quarterlyRev.csv')
head(cvsDF)
plot(cvsDF$Value, type = 'l')

# Convert to a date
cvsDF$Date <- mdy(cvsDF$Date)


# Create a time series
stYr  <- year(cvsDF$Date[1])
stQtr <- quarter(cvsDF$Date[1])
st    <- c(stYr, stQtr)
qtrTS <- ts(cvsDF$Value, start = st, frequency = 4)
qtrTS

# Apply Time Series Decomposition as part of exploratory work
cvsDecompose <- decompose(qtrTS)

# Plot it
plot(cvsDecompose)

# Now use window() to partition the data and build a Holt Winters model, ending BEFORE 2020. 
# Optionally/Additionally you could create a naive drift modelfor comparison.
trainTS      <- window(qtrTS, end = c(2019,4))
validationTS <- window(qtrTS, start = c(2020,1))

# Make a HW model using the training periods
qtrHW <- HoltWinters(trainTS, seasonal = 'mult')

# Plot it and see how well it did
plot(qtrHW)

# Make predictions for each of the validation periods
validForecasts <- predict(qtrHW, length(validationTS))

# Organize & Compare
validationDF <- data.frame(idx      = seq_along(validForecasts),
                           original = as.vector(validationTS),
                           fit      = validForecasts)

# Examine visual fit
ggplot(validationDF) +
  geom_line(aes(x=idx, y=original), color = 'black', alpha = 0.5) +
  geom_line(aes(x=idx, y=fit), colour='red') + theme_bw()

# Get RMSE
RMSE(validationDF$original, validationDF$fit)
MAPE(validationDF$original, validationDF$fit)

# Why did the HW forecast over predict revenue consistently?
# The impact of the virus diminished revenue, though not as bad as other companies.  Since HW has a trend component, the impacted and diminished trend has a lasting effect on this forecast since the entire training does not have any virus impacted quarters.

# Now that we "like" HW since it was only 6% off, we should refit it on all the data and make a forecast
fullHW <- HoltWinters(qtrTS, seasonal = 'mult')

# Make predictions for the next 2 yrs (8quarters)
twoYrForecasts <- predict(fullHW, 8)

# Compare it to the original partitioned model, predict the validation set and the 8 new quarters
validationAndTwoYrForecasts <- predict(qtrHW, length(validationTS)+8)

# When you compare the two you see the full HW has lower amounts because the change in trend from covid is included
twoYrForecasts
tail(validationAndTwoYrForecasts, 8)


#' See if you can understand the impact of TSA screenings due to coronavirus using TSD
#' Using HoltWinters make a prediction for the next days, evaluate the model using out of time sampling
#' Once satisfied with HW (or another from the book) rerun with all the data making a prediction for the next month and compare to the data here:
#' https://www.tsa.gov/coronavirus/passenger-throughput
#' 

# Here is some code to get you started
tsaDF <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/G_TimeSeries_Equities/data/TSA_screened_passengers.csv')
head(tsaDF)
plot(tsaDF$TSAscreenings, type = 'l')

# Convert to a TS for the TSA_screenings, then partition with window(), then model, evaluate, and predict!
# End