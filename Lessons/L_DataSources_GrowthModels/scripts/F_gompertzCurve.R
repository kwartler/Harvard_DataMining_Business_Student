#' Author: Ted Kwartler
#' Data: Nov 27, 2022
#' Purpose: Gompertz Curve Examples
#' 

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")
options(scipen=999)

# libs
library(readr)
library(growthmodels)
library(easynls) #easy non linear modeling
library(lubridate)

# Get some data
colorTV <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/L_DataSources_GrowthModels/data/colorTV_adoption.csv')

# EDA
head(colorTV)

# Modify bc gompertz doesn't care about seasonality or actual time series
colorTV$Year <- 1:nrow(colorTV)

# Inputs to the model
colorGrowth <- gompertz(t     = colorTV$Year, #
                        alpha = 100, #maximum of the curve i.e. 100%
                        beta  = 2.75, #maximum displacement i.e. stretched out
                        k     = .15) # max growth rate ie steepest slope
plot(colorTV$PercentAdoptions, type = 'l')
lines(colorGrowth, col='red')

# Not great so let's have the computer optimize the parameters using OLS; check docs
?nlsfit
model2 <- nlsfit(data  = colorTV, #1st col is "explanatory" ie just time passing yr1,2, 3...
                 model = 10, #10 = "y~a*exp(-b*exp(-c*x)" gompertz; others in docs
                 start = c(a = 100, b = 2, c = .1)) #iterations of the inputs
model2

colorGrowth <- gompertz(t      = 1:nrow(colorTV),
                        alpha  = model2$Parameters[1,], #maximum of the curve ie 99.3279%
                        beta   = model2$Parameters[2,], #maximum displacement 
                        k      = model2$Parameters[3,]) #max growth rate

plot(colorTV$PercentAdoptions, type = 'l')
lines(colorGrowth, col='red')

# What about another data set from before (this is a bad practice to load data again and should be in another script)
rm(list = ls())
cvs <- read_csv("https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/G_TimeSeries_Equities/data/nike_qtr_rev.csv")
cvs$date <- mdy(gsub(' 1:00','', cvs$date))
cvs$yr   <- year(cvs$date)

# Sum to year
cvsAnnual <- aggregate(revMill ~ yr, cvs, sum)

# Drop incomplete last year
cvsAnnual <- cvsAnnual[-nrow(cvsAnnual),]

# Obtain the parmeters
model3 <- nlsfit(data.frame(yr=1:nrow(cvsAnnual),revB=cvsAnnual$revMill), 
                 model = 10, start = c(a = 50000, b = 10, c = .1))

cvsGrowth <- gompertz(t      = 1:nrow(cvsAnnual),
                       alpha  = model3$Parameters[1,], 
                       beta   = model3$Parameters[2,], 
                       k      = model3$Parameters[3,])
plot(cvsAnnual$revMill, type = 'l')
lines(cvsGrowth, col='red')

## Alpha is the  asymptote or upper limit
# This parametric model believes that without disruption (supply chain, recession, pandemic, acquisition) the upper limit of CVS revenue is
model3$Parameters[1,]


# End
