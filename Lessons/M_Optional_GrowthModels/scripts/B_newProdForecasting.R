#' Author: Ted Kwartler
#' Data: 5-3-2020
#' Purpose: Diffusion Modeling for Forecasting 
#' 

# Set the working directory
setwd("/cloud/project/Lessons/M_Optional_GrowthModels/data")
options(scipen=999)

# libs
library(diffusion)

#### Example to see the curves w/full data
tsIbm
computerBass <- diffusion(tsIbm[, 3], type = "bass")
plot(computerBass)
####

# Data
hdTV <- read.csv("HDTV_shipments.csv")
hdTV

# Biz Understanding 
#285m US TVs
#33.35m Canadian TVs
#318m total
#color tv data was 26yrs to 98%

# Fit a bass model
# bassParam <- c(p = .008,q = .421, m = 318) #from an online example
bassParam <- c(.005,.84,318) #p,q for color TV
fitBass   <- diffusion(hdTV[, 2], type = "bass",w = bassParam)

# plot
plot(fitBass)

# Predict
hdTVpreds <- predict(fitBass, h = (26-nrow(hdTV)))

# Examine
hdTVpreds$frc

# Organize
finalDF <- data.frame(yr=1999:2025, 
                      cumulativeSales = c(hdTV[,3], hdTVpreds$frc[,1]),
                      totalSales      = c(hdTV[,2], hdTVpreds$frc[,2]))

finalDF

plot(x=finalDF$yr,
     y=finalDF$totalSales, type ='l', col='darkgreen')
points(x=finalDF$yr[1:4], 
       y=finalDF$totalSales[1:4], col='red')


plot(x=finalDF$yr,
     y=finalDF$cumulativeSales, type ='l', col='darkgreen')
points(x=finalDF$yr[1:4], 
       y=finalDF$cumulativeSales[1:4], col='red')

# Gompertz
#For the Gompertz curve, vector w needs to be in the form of ("a", "b", "m"). Where "a" is the x-axis displacement coefficient, "b" determines the growth rate and "m" sets, similarly to Bass model, the market potential (saturation point).
# from nlsfit(alpha=99.32796, beta=3.785561, k=0.2043271) for color TV
gompParams <- c(a=3.785561, b=0.2043271, m=318) #notice the mkt at 100 or 318 doesn't matter
fitGomp <- diffusion(hdTV[, 2], type = "gompertz", w =gompParams)

# Predict
hdTVgomPreds <- predict(fitGomp, h = (26-nrow(hdTV)))

# Examine
hdTVgomPreds$frc

# Organize
gompDF <- data.frame(yr=1999:2025, 
                      cumulativeSales = c(hdTV[,3], hdTVgomPreds$frc[,1]))

lines(x=gompDF$yr, y =gompDF$cumulativeSales, col='blue')

# End