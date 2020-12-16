#' Author: Ted Kwartler
#' Data: 5-3-2020
#' Purpose: Diffusion Modeling for Forecasting 
#' 

# Set the working directory
setwd("~/Documents/Harvard_DataMining_Business_Admin/lessons/N_Optional_GrowthModels/data")
options(scipen=999)

# libs
library(diffusion)

# Data
hdTV <- read.csv("HDTV_shipments.csv")

# Biz Understanding 
#285m US TVs
#33.35m Canadian TVs
#318m total
#color tv data was 26yrs to 98%

# Fit a bass model
# bassParam <- c(p = .008,q = .421, m = 318) #from textbook
bassParam <- c(.005,.84,318) #p,q for color TV
fitBass   <- diffusion(hdTV[, 2], type = "bass",w = bassParam)

# Predict
hdTVpreds <- predict(fitBass, h = (26-nrow(hdTV)))

# Examine
hdTVpreds$frc

# Organize
finalDF <- data.frame(yr=1999:2025, 
                      cumulativeSales = c(hdTV[,3], hdTVpreds$frc[,1]),
                      totalSales      = c(hdTV[,2], hdTVpreds$frc[,2]))

finalDF

plot(finalDF$cumulativeSales, type ='l', col='darkgreen')
points(finalDF$cumulativeSales[1:4], col='red')

# Gompertz
#For the Gompertz curve, vector w needs to be in the form of ("a", "b", "m"). Where "a" is the x-axis displacement coefficient, "b" determines the growth rate and "m" sets, similarly to Bass model, the market potential (saturation point).
# from nlsfit(alpha=99.32796, beta=3.785561, k=0.2043271) for color TV
gompParams <- c(a=3.785561, b=0.2043271, m=318)
fitGomp <- diffusion(hdTV[, 2], type = "gompertz", w =gompParams)

# Predict
hdTVgomPreds <- predict(fitGomp, h = (26-nrow(hdTV)))

# Examine
hdTVgomPreds$frc

# Organize
gompDF <- data.frame(yr=1999:2025, 
                      cumulativeSales = c(hdTV[,3], hdTVgomPreds$frc[,1]))

lines(gompDF$cumulativeSales, col='blue')

# End