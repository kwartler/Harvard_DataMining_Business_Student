#' Author: Ted Kwartler
#' Data: 5-3-2020
#' Purpose: Gompertz Curve Examples
#' 

# Set the working directory
setwd("/cloud/project/Lessons/M_Optional_GrowthModels/data")
options(scipen=999)

# libs
library(growthmodels)
library(easynls)

# Get some data
colorTV <- read.csv('colorTV_adoption.csv')

# EDA
head(colorTV)

# Modify bc gompertz doesn't care about seasonality or actual time series
colorTV$Year <- 1:nrow(colorTV)

# Inputs to the model
colorGrowth <- gompertz(t     = 1:nrow(colorTV),
                        alpha = 100, #maximum of the curve i.e. 100%
                        beta  = 2.75, #maximum displacement i.e. stretched out
                        k     = .15) # max growth rate ie steepest slope
plot(colorTV$PercentAdoptions, type = 'l')
lines(colorGrowth, col='red')

# Not great so let's have the computer optimize the parameters using OLS
model2 <- nlsfit(colorTV, model = 10, start = c(a = 100, b = 2, c = .1))
model2

colorGrowth <- gompertz(t      = 1:26,
                        alpha  = model2$Parameters[1,], 
                        beta   = model2$Parameters[2,], 
                        k      = model2$Parameters[3,])

plot(colorTV$PercentAdoptions, type = 'l')
lines(colorGrowth, col='red')

# What about another phenom
amzn <- read.csv("AMZN_Ann_Rev.csv")
amzn$revB <- amzn$revMill/1000
model3 <- nlsfit(data.frame(yr=1:22,revB=amzn$revB), 
                 model = 10, start = c(a = 180, b = 10, c = .1))

amznGrowth <- gompertz(t      = 1:22,
                       alpha  = model3$Parameters[1,], 
                       beta   = model3$Parameters[2,], 
                       k      = model3$Parameters[3,])
plot(amzn$revB, type = 'l')
points(amznGrowth, col='red')

## Alpha is the  asymptote or upper limit
# since amzn revenue doesn't appear to be slowing down it is quite high 
model3$Parameters[1,]


# End
