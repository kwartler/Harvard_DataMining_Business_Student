#' Author: Ted Kwartler
#' Data: 5-3-2020
#' Purpose: Bass Diffusion Modeling  
#' 

# Set the working directory
setwd("~/Documents/Harvard_DataMining_Business_Admin/lessons/M_Optional_GrowthModels/data")
options(scipen=999)

# libs
library(diffusion)

# data
cell <- read.csv('cellAdoption.csv')

# Pretend we have no idea and only 5yrs of growth and an estimated percentage
plot(cell[,2], type='l')

#vector w needs to be provided for the Bass curve in the order of "p", "q", "m", where "p" is thecoefficient of innovation, "q" is the coefficient of imitation and "m" is the market size coefficient.
#(0.008, 0.421, 100)
fitbass <- diffusion(cell[1:5, 2], type = "bass",w = c(.008,.421,100))
x <- predict(fitbass, h = 5)
x$frc

# End