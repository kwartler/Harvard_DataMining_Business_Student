#' Author: Ted Kwartler
#' Date: Oct 21, 2022
#' Purpose: Simple Moving Avg Example
#'

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)

# Get Chipotle
getSymbols("CMG") 
CMG <- CMG['2018-03-01/']

# Plot
plot(CMG$CMG.Close)

# Calculate the moving Avgs
CMGma3  <- SMA(CMG$CMG.Close, 3)
CMGma10 <- SMA(CMG$CMG.Close, 10)
CMGma30 <- SMA(CMG$CMG.Close, 30)
CMGma100 <- SMA(CMG$CMG.Close, 100)
CMGma250 <- SMA(CMG$CMG.Close, 250)

# Plot different MA windows
plot(CMG$CMG.Close)
lines(CMGma3, col='red')

plot(CMG$CMG.Close)
lines(CMGma10, col='red')

plot(CMG$CMG.Close)
lines(CMGma30, col='red')

plot(CMG$CMG.Close)
lines(CMGma100, col='red')

plot(CMG$CMG.Close)
lines(CMGma250, col='red')

# End

