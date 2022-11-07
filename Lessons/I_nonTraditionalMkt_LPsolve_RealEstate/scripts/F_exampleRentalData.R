#' Author: Ted Kwartler
#' Date: Nov 7, 2022
#' Purpose: Quick Examination of real rental history data
#' 

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(readr)
library(lubridate)

# Get the data
condo <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/I_nonTraditionalMkt_LPsolve_RealEstate/data/exampleRental.csv')
condo <- as.data.frame(condo)
condo$Month <- my(condo$Month)
condo$monthName <- month(condo$Month )

# Examine & adjust for dates
head(condo)
tail(condo)
summary(condo)

# Do we seasonality?
plot(condo$NightOccupied, type = 'l')
aggregate(NightOccupied ~ monthName, condo, mean)
aggregate(NetOperatingIncome ~ monthName, condo, mean)

# Obv there is a relationship between nights occupied and net income
plot(condo$NightOccupied, condo$NetOperatingIncome)
lines(lowess(condo$NightOccupied, condo$NetOperatingIncome ), col = "blue")

### Monthly Occupancy by Variable Cost, notice the decreasing unit cost
varCost <- condo$variableCosts/condo$NightOccupied
plot(condo$NightOccupied,varCost, main = 'Var Cost *per night* decreases the more rented nights')
lines(lowess(condo$NightOccupied, varCost ), col = "blue")

### Monthly Costs, but the total costs keep going up
totalCosts <- condo$fixedCosts + condo$variableCosts
plot(condo$NightOccupied,totalCosts, main = 'totalCosts increase the more rented nights')
lines(lowess(condo$NightOccupied, totalCosts ), col = "blue")

### Although I added into the DF already let's manually calc OER
# OER Examination $OperatingExpenseRatio should equal this new example column $oerNew
condo$oerNew <- (condo$fixedCosts + condo$variableCosts)  / (condo$NightOccupied*condo$Avg.Price.Per.Night)
head(condo)

# Since this is one property we can calculate the OER with weighted mean to account for the seasonality
mean(condo$oerNew) #1.300 = unprofitable! but this weights all months the same even though some have only 2 nights!
weighted.mean(condo$oerNew, condo$NightOccupied/sum(condo$NightOccupied)) #0.8660444 profitable

# Suppose the condo is $79K
# Cap Rate is 
wgtAvgNetOpIncome <- weighted.mean(condo$NetOperatingIncome, condo$NightOccupied/sum(condo$NightOccupied)) #950.72
wgtAvgNetOpIncome / 79000 #0.01203452 basically break even to asking price.

# This prop was sold for $64K, its a studio apt in a motel
wgtAvgNetOpIncome / 64000#0.01485511 basically break even but slightly better

# End





