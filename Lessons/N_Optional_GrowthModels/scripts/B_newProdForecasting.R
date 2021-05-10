#' Author: Ted Kwartler
#' Data: 5-10-2021
#' Purpose: Diffusion Modeling for Forecasting 
#' 

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/N_Optional_GrowthModels/data")
options(scipen=999)

# libs
library(diffusion)

# Data
hdTV <- read.csv("HDTV_shipments.csv")
hdTV

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
hdTV

# Visuals
plot(c(hdTV$HDTV.Shipments, hdTVpreds$frc[,2]), type = 'l', 
     main = 'Annual Shipments Bass')
points(hdTV$HDTV.Shipments, col = 'red')

plot(c(hdTV$Cumulative.HDTV.Shipments, hdTVpreds$frc[,1]), type = 'l', 
     main = 'Total Cumulative Market (saturation) Bass')
points(hdTV$Cumulative.HDTV.Shipments, col = 'red')

# Gompertz
#For the Gompertz curve, vector w needs to be in the form of ("a", "b", "m"). Where "a" is the x-axis displacement coefficient, "b" determines the growth rate and "m" sets, similarly to Bass model, the market potential (saturation point).
# from nlsfit(alpha=99.32796, beta=3.785561, k=0.2043271) for color TV
gompParams <- c(a=3.785561, b=0.2043271, m=318)
fitGomp <- diffusion(hdTV[, 2], type = "gompertz", w =gompParams)

# Predict
hdTVgomPreds <- predict(fitGomp, h = (26-nrow(hdTV)))

# Examine
hdTVgomPreds$frc

# Visuals
plot(c(hdTV$HDTV.Shipments, hdTVgomPreds$frc[,2]), type = 'l', 
     main = 'Annual Shipments Gompertz')
points(hdTV$HDTV.Shipments, col = 'red')

plot(c(hdTV$Cumulative.HDTV.Shipments, hdTVgomPreds$frc[,1]), type = 'l', 
     main = 'Total Cumulative Market (saturation) Gompertz')
points(hdTV$Cumulative.HDTV.Shipments, col = 'red')

# Plot not as good a fit but the high point is similar so there is expected variance in adoption given some mkt saturation point.  Planning would probably be something in between unless there is a definite reason to pick one over the other.


# End