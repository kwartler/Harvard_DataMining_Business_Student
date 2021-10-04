#' Author: Ted Kwartler
#' Date: 10-3-2021
#' Purpose: Regressions
#' 

# Libs
options(scipen = 999)

# Setwd
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/E_LogReg/data")

# Library
library(ModelMetrics)

# Data
houses <-read.csv('BostonHousing.csv')
houses <- houses[order(houses$MEDV), ]
houses$realValue <- houses$MEDV*1000
houses$MEDV <- NULL #original variable
houses$CAT..MEDV <-NULL #not used categorical Y var


# Partitioning; get 10% test set
splitPercent <- round(nrow(houses) %*% .9)

set.seed(1234)
idx      <- sample(1:nrow(houses), splitPercent)
trainSet <- houses[idx, ]
testSet  <- houses[-idx, ]

# Visualize a relationship; do you see a trend
plot(houses$RM, houses$realValue)

# Let's make up a model; medianValue = 0 + 1*rooms
# This means for every room it adds 1 (actually 10,000) to the median value
abline(0,1000, col='red') #intercept, then slope

# Fit a model (univariate) with no intercept
# The equation of this model is the Y ~ the variable RM and with +0 we are forcing there to be NO beta-naught
fit <- lm(realValue ~ RM + 0, trainSet)

# Examine
fit

# Add the function line
abline(a = 0, #intercept
       b = coefficients(fit), col='red') #slope for every room in an house it adds 3.65 to the median value

# Fit a model with the intercept by removing the +0 in the formula, representing the steady state of median values
fit2 <- lm(realValue ~ RM, trainSet)

# Examine
fit2

# Add the function line
abline(a = coefficients(fit2)[1], #intercept
       b = coefficients(fit2)[2] , col='blue', lwd = 5) #slope

# Get RMSE -- training
manualPreds <- trainSet$RM*1000 #slope is 1 so beta =1 X the actual value
preds1      <- predict(fit, trainSet) 
preds2      <- predict(fit2, trainSet)
rmse(trainSet$realValue, manualPreds)
rmse(trainSet$realValue, preds1)
rmse(trainSet$realValue, preds2)

# Get RMSE -- validation
manualPredsTest <- testSet$RM*1000 #slope is 1 so beta =1 X the actual value
preds1Test      <- predict(fit, testSet) 
preds2Test      <- predict(fit2, testSet)
rmse(testSet$realValue, manualPredsTest)
rmse(testSet$realValue, preds1Test)
rmse(testSet$realValue, preds2Test)




# End