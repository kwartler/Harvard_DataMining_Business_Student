#' Author: Ted Kwartler
#' Date: 9-27-20
#' Purpose: Regressions
#' 

# Libs
options(scipen = 999)

# Setwd
setwd("~/Documents/Harvard_DataMining_Business_Student/Lessons/D_regression/data")

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
       b = coefficients(fit2)[2] , col='blue') #slope

# Get some predictions on the training set
manualPreds <- trainSet$RM*1000 #slope is 1 so beta =1 X the actual value
preds1      <- predict(fit, trainSet) 
preds2      <- predict(fit2, trainSet)

# Examine predictions since this is one of the first times we did predict() & compare to the actual values
cbind(head(preds1),head(trainSet$realValue))

# Get sum of squared errors
manualErr <- (trainSet$realValue - manualPreds)^2
fitErr    <- (trainSet$realValue - preds1)^2
fit2Err   <- (trainSet$realValue - preds2)^2 

sqrt(mean(manualErr))
sqrt(mean(fitErr))
sqrt(mean(fit2Err))

# Now validation
manualPreds <- testSet$RM*1000 # Again beta = 1 X actual values
preds1      <- predict(fit, testSet)
preds2      <- predict(fit2, testSet)

# Get sum of squared errors
manualErr <- (testSet$realValue - manualPreds)^2
fitErr    <- (testSet$realValue - preds1)^2
fit2Err   <- (testSet$realValue - preds2)^2 

sqrt(mean(manualErr))
sqrt(mean(fitErr))
sqrt(mean(fit2Err))

# End