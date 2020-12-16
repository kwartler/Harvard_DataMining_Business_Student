#' Author: Ted Kwartler
#' Data: 5-3-2020
#' Purpose: Gompertz Curve for HD TV shipments
#' Worth reading in 2021 classes 
#' https://holtmeier.de/how-big-is-my-dog-going-to-get-a-regression-analysis-with-r/

# Set the working directory
setwd("~/Documents/Harvard_DataMining_Business_Admin/lessons/M_Optional_GrowthModels/data")
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
colorGrowth <- growthmodels::gompertz(t = 1:nrow(colorTV),
                        alpha = 100, #maximum of the curve i.e. 100%
                        beta = 2.75, #maximum displacement i.e. stretched out
                        k = .15) # max growth rate ie steepest slope
plot(colorTV$PercentAdoptions, type = 'l')
lines(colorGrowth, col='red')

# Better to optimize than hunt and peck
model2 <- nlsfit(colorTV, model = 10, start = c(a = 100, b = 2, c = .1))
model2

class(model2)

colorGrowth <- growthmodels::gompertz(t = 1:26,
                                      alpha = model2$Parameters[1,], 
                                      beta = model2$Parameters[2,], 
                                      k = model2$Parameters[3,]) 
plot(colorTV$PercentAdoptions, type = 'l')
lines(colorGrowth, col='red')


# Total 285million TVs in 120m US homes
#https://hypertextbook.com/facts/2007/TamaraTamazashvili.shtml
#https://www.tvtechnology.com/news/number-of-u-s-tv-homes-grows-to-120-6-million-says-nielsen
#https://qz.com/321572/why-you-might-replace-your-tv-with-one-of-these/

#285m US TVs
#33.35m Canadian TVs
#318 total

# HDTV
hdTV <- read.csv("final_HDTV_shipments.csv")
hdTV$percent <- cumsum(hdTV$HDTV_shipments_M) /318

hdGrowth <- growthmodels::gompertz(t = 1:11,
                                      alpha = 100, 
                                      beta = model2$Parameters[2,], 
                                      k = model2$Parameters[3,]) 
plot(colorGrowth, type = 'l')
points((hdTV$percent*100), col='red')

tk <- data.frame(hdTV$timeN,hdTV$percent)
model3 <- nlsfit(tk[1:5,], 
                 model = 10, start = c(a = 1, b = .5, c = .01))
model3

x<- read.csv("AMZN_Ann_Rev.csv")
x$revBillions <- x$revMill/1000

colorGrowth <- growthmodels::gompertz(t = 1:40,
                                      alpha = 523, 
                                      beta = 13, 
                                      k = .11) 
plot(colorGrowth, type = 'l')
points(x$revBillions)

y <- data.frame(date = 1:22,
           rev=x$revBillions)
model3 <- nlsfit(y, 
                 model = 10, start = c(a = 150, b = 10, c = .1))
colorGrowth <- growthmodels::gompertz(t = 1:40,
                                      alpha = 177, 
                                      beta = model3$Parameters[2,], 
                                      k = model3$Parameters[3,]) 
plot(colorGrowth, type = 'l')
points(x$revBillions)
plot(x$revBillions)
lines(colorGrowth)

     