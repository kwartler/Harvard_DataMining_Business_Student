#' Author: Ted Kwartler
#' Date: 10-10-2023
#' Purpose: Regressions
#' 

# Libs & options
library(MLmetrics)
options(scipen = 999)

# Setwd
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Data
houses <-read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/E_Regressions/data/BostonHousing.csv')
houses <- houses[order(houses$MEDV), ]
houses$realValue <- houses$MEDV*10000
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
abline(0,10000, col='red') #intercept, then slope

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

# Get some predictions on the training set
manualPreds <- trainSet$RM*10000 #slope is 1 so beta =1 X the actual value
preds1      <- predict(fit, trainSet) 
preds2      <- predict(fit2, trainSet)

# Examine predictions since this is one of the first times we did predict() & compare to the actual values
data.frame(predicted = head(preds1),actual = head(trainSet$realValue))

# Get sum of squared errors manually for "heuristic model" 
manualErr <- (trainSet$realValue - manualPreds)^2
sqrt(mean(manualErr))

# Get RMSE for "heuristic model" 
RMSE(y_pred = manualPreds, y_true = trainSet$realValue)

# Get sum of squared errors manually for "ok model" 
fitErr    <- (trainSet$realValue - preds1)^2
sqrt(mean(fitErr))

# Get RMSE for "ok model" 
RMSE(y_pred = preds1, y_true = trainSet$realValue)

# Get sum of squared errors manually for "best model" 
fit2Err   <- (trainSet$realValue - preds2)^2 
sqrt(mean(fit2Err))

# Get RMSE for "best model" 
RMSE(y_pred = preds2, y_true = trainSet$realValue)

# Now let's do the test set, remember to look for consistency <5-10% change is ideal
manualPredsTest <- testSet$RM*10000 # Again beta = 1 X actual values
preds1Test      <- predict(fit, testSet)
preds2Test      <- predict(fit2, testSet)

# RMSE for each "model"
RMSE(y_pred = manualPredsTest, y_true = testSet$realValue)
RMSE(y_pred = preds1Test, y_true = testSet$realValue)
RMSE(y_pred = preds2Test, y_true = testSet$realValue)

# MAPE for each "model"
MAPE(y_pred = manualPredsTest, y_true = testSet$realValue)
MAPE(y_pred = preds1Test, y_true = testSet$realValue)
MAPE(y_pred = preds2Test, y_true = testSet$realValue)

# End