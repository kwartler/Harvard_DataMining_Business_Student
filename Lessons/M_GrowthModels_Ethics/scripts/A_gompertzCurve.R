#' Author: Ted Kwartler
#' Data: May 2, 2022
#' Purpose: Gompertz Curve Examples
#' 

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/M_GrowthModels_Ethics/data")
options(scipen=999)

# libs
library(growthmodels)
library(easynls) #easy non linear modeling

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
model2 <- nlsfit(data  = colorTV, 
                 model = 10, #10 = "y~a*exp(-b*exp(-c*x)" gompertz; others in docs
                 start = c(a = 100, b = 2, c = .1)) #iterations of the inputs
model2

colorGrowth <- gompertz(t      = 1:nrow(colorTV),
                        alpha  = model2$Parameters[1,], 
                        beta   = model2$Parameters[2,], 
                        k      = model2$Parameters[3,])

plot(colorTV$PercentAdoptions, type = 'l')
lines(colorGrowth, col='red')

# What about another product
amzn <- read.csv("AMZN_Ann_Rev.csv")
amzn$revB <- amzn$revMill/1000
model3 <- nlsfit(data.frame(yr=1:22,revB=amzn$revB), 
                 model = 10, start = c(a = 180, b = 10, c = .1))

amznGrowth <- gompertz(t      = 1:22,
                       alpha  = model3$Parameters[1,], 
                       beta   = model3$Parameters[2,], 
                       k      = model3$Parameters[3,])
plot(amzn$revB, type = 'l')
lines(amznGrowth, col='red')

## Alpha is the  asymptote or upper limit
# since amzn revenue doesn't appear to be slowing down it is quite high; obviously marco-factors like the pandemic change this type of forecasting so rolling forward and refitting can be useful.
model3$Parameters[1,]


# End