#' Author: Ted Kwartler
#' Date: 7-5-2018
#' Purpose: oldCar Toyota Corolla Backward step Variable Regression
#'
# Options
options(scipen=999)

# Libs
library(vtreat)

# SetWD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Data
cars <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/E_Regressions/data/oldCar.csv')

# Partitioning 20% test set
splitPercent <- round(nrow(cars) %*% .8)

set.seed(2017)
idx      <- sample(1:nrow(cars), splitPercent)
trainSet <- cars[idx, ]
testSet  <- cars[-idx, ]

#PreProcessing
dataPlan     <- designTreatmentsN(cars, names(cars)[2:13], 'Price')
treatedTrain <- prepare(dataPlan, trainSet)
treatedTest  <- prepare(dataPlan, testSet)

# Fit
fit <- lm(Price ~ ., treatedTrain)

# Step
backFit <- step(fit,direction = 'backward', trace = 5)
summary(backFit)


# End