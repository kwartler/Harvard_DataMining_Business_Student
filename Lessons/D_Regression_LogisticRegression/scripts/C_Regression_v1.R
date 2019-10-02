#' Author: Ted Kwartler
#' Date: 9-24-2019
#' Purpose: Regressions
#' 

# Libs

# Setwd
setwd("/cloud/project/Lessons/D_Regression_LogisticRegression/data")

# Data
houses <-read.csv('BostonHousing.csv')
houses$CAT..MEDV <-NULL #not used categorical Y var

# Partitioning; get 10% test set
splitPercent <- round(nrow(houses) %*% .9)

set.seed(1234)
idx      <- sample(1:nrow(houses), splitPercent)
trainSet <- houses[idx, ]
testSet  <- houses[-idx, ]

# Visualize a relationship; do you see a trend
houses <- houses[order(houses$MEDV), ]
plot(houses$RM, houses$MEDV, xlim=c(0,10), ylim = c(-35, 50))

# Let's make up a model; medianValue = 0 + 1*rooms
# This means for every room it adds 1 to the median value
abline(0,1, col='red') #intercept, then slope

# Fit a model (univariate) with no intercept
# The equation of this model is the Y ~ the variable RM and with +0 we are forcing there to be NO beta-naught
fit <- lm(MEDV ~ RM + 0, trainSet)

# Examine
fit

# Add the function line
abline(a = 0, #intercept
       b = coefficients(fit), col='red') #slope for every room in an house it adds 3.65 to the median value

# Fit a model with the intercept by removing the +0 in the formula, representing the steady state of median values
fit2 <- lm(MEDV ~ RM, trainSet)

# Examine
fit2

# Add the function line
abline(a = coefficients(fit2)[1], #intercept
       b = coefficients(fit2)[2] , col='blue') #slope

# Get some predictions on the training set
manualPreds <- trainSet$RM #slope is 1 so beta =1 X the actual value
preds1      <- predict(fit, trainSet) 
preds2      <- predict(fit2, trainSet)

# Examine predictions since this is one of the first times we did predict() & compare to the actual values
head(preds1)
head(trainSet$MEDV)

# Get sum of squared errors
manualErr <- (trainSet$MEDV - manualPreds)^2
fitErr    <- (trainSet$MEDV - preds1)^2
fit2Err   <- (trainSet$MEDV - preds2)^2 

sum(manualErr)
sum(fitErr)
sum(fit2Err)

# Now validation
manualPreds <- testSet$RM # Again beta = 1 X actual values
preds1      <- predict(fit, testSet)
preds2      <- predict(fit2, testSet)

# Get sum of squared errors
manualErr <- (testSet$MEDV - manualPreds)^2
fitErr    <- (testSet$MEDV - preds1)^2
fit2Err   <- (testSet$MEDV - preds2)^2 

sum(manualErr)
sum(fitErr)
sum(fit2Err)

# End