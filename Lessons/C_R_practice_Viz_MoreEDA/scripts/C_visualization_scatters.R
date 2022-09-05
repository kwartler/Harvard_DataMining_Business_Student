#' Author: Ted Kwartler
#' Date: Sept 5 2022
#' Purpose: R bar visual ggplot scatter & bubble examples
#' Good resource: https://r-graphics.org/

# wd
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/C_R_practice_Viz_MoreEDA/data")

# libs
library(ggplot2)
library(ggthemes)
library(readr)
library(lubridate)
library(qcc)

# Load
possiblePurchase <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/C_R_practice_Viz_MoreEDA/data/MarthasVineyardCondo.csv')
possiblePurchase <- as.data.frame(possiblePurchase)
names(possiblePurchase) <- make.names(names(possiblePurchase))

# Since we aren't doing temporal exploration we won't clean up

# Simple Scatter: relationship between two variables
ggplot(data = possiblePurchase, aes(x=NightOccupied, y=))



# End