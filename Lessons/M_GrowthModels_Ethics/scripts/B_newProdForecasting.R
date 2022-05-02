#' Author: Ted Kwartler
#' Data: May 2, 2022
#' Purpose: Diffusion Modeling for Forecasting 
#' 

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/M_GrowthModels_Ethics/data")
options(scipen=999)

# libs
library(diffusion)
library(ggplot2)
library(tidyr)
library(ggthemes)

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
bassParam <- c(.005,.84,318) #p,q for color TV; these values are often found online
fitBass   <- diffusion(hdTV[, 2], type = "bass",w = bassParam)

# Predict
hdTVpreds <- predict(fitBass, h = (26-(nrow(hdTV)+1))) #account for yr0 and first 4 yrs in data

# Examine
hdTVpreds$fit
hdTVpreds$frc
hdTV

# Total Market adoption by year
plot(hdTVpreds$frc[,1], type = 'l', main = 'total market')

# TV Sets sold among innovators
plot(hdTVpreds$frc[,3],  type = 'l', main = 'Imitators', col = 'blue')

# TV Sets sold among imitators
plot(hdTVpreds$frc[,4],  type = 'l', main = 'Imitators', col = 'red')

# total by yr
tmpAnnualSales <- data.frame(yr = 1:length(hdTVpreds$frc[,3]), 
                             inno = hdTVpreds$frc[,3], 
                             imit = hdTVpreds$frc[,4])

tmpAnnualSales <- tidyr::pivot_longer(tmpAnnualSales, -yr)
ggplot(data = tmpAnnualSales, aes(x = yr, y = value, fill = name)) + geom_col() + theme_gdocs()


# End