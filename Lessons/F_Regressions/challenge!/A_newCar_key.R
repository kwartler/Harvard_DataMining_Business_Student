#' Author: Ted Kwartler
#' Date: May 28, 2023
#' Purpose: Toyota Corolla Regression example
#'

# Libs
library(vtreat)
library(dplyr)
library(MLmetrics)

# Options
options(scipen=999)

# Data
cars <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/E_Regressions/data/newCars.csv')

# drop geo and text for this example
cars$state       <- NULL
cars$city        <- NULL
cars$allFeatures <- NULL
cars$monthlyPayment <- NULL # Target Leakage!


#### SAMPLE
# Get 10% for variable treatment
set.seed(2023)
varTreatIDX <- sample(1:nrow(cars), nrow(cars)*.1)
varTreatData <- cars[varTreatIDX,]
leftoverData <- cars[-varTreatIDX,]

# Use the leftover data to create a training and validation partition (80/20)
splitPercent <- round(nrow(leftoverData) %*% .8)
set.seed(1234)
idx      <- sample(1:nrow(leftoverData), splitPercent)
trainSet <- leftoverData[idx, ]
testSet  <- leftoverData[-idx, ]

### EXPLORE; you would do more than this in reality
summary(trainSet)

# Build a kernel density plot of the trainSet$listPrice
plot(density(trainSet$listPrice))

# Get the column names of our data frame
names(trainSet)

### MODIFY, variable treatment
informativeFeatureNames <- names(trainSet)[5:24]
outcomeVariableName     <- names(trainSet)[25] # Or simply "listPrice"

# Preprocessing & Automated Engineering
# id & constant variable removal, dummy $Fuel_Type
dataPlan     <- designTreatmentsN(varTreatData,
                                  informativeFeatureNames,
                                  outcomeVariableName)

# Apply the plan to the trainSet and testSet
treatedTrain <- prepare(dataPlan, trainSet)
treatedTest  <- prepare(dataPlan, testSet)

### MODEL
# Since we are comfortable with the data, we can concisely write a lm equation that includes all variables using period instead of an equation
fit <- lm(listPrice ~ ., treatedTrain)
summary(fit)

### THIS SECTION IS SHOWN FOR EXAMPLE WITHOUT STUDENT INPUT
# Drop uninformative vars
# Primarily these are nearly univariate (single value)
# Hybrid is captured in the mileage stats so its multi-colinear!

# First get the variable and corresponding p-values
pVals <- data.frame(varNames = names(na.omit(coef(fit))),
                    pValues = round(summary(fit)$coefficients[,4],4),
                    row.names = NULL)
pVals

# Determine which variable names to keep with p values less than .1
keeps <- subset(pVals$varNames, pVals$pValues<0.1)

# Remove unwanted columns in a new training set
treatedTrainParsimony <- treatedTrain[,names(treatedTrain) %in% keeps]

# Append the y-variable so you can re-model the parismonious data
treatedTrainParsimony$listPrice <- treatedTrain$listPrice

# Refit a model
fit2 <- lm(listPrice ~ ., treatedTrainParsimony)
summary(fit2)


# Get Training Set Predictions
# Warning can be ignored but for those interested:
# https://stackoverflow.com/questions/26558631/predict-lm-in-a-loop-warning-prediction-from-a-rank-deficient-fit-may-be-mis
trainingPreds <- predict(fit2, treatedTrainParsimony)

#Organize training set preds
trainingResults <-data.frame(actuals        = treatedTrainParsimony$listPrice,
                             predicted      = trainingPreds,
                             residualErrors = treatedTrainParsimony$listPrice-trainingPreds )
head(trainingResults)

# What is the RMSE?
trainRMSE <- RMSE(y_pred = trainingResults$predicted,
                  y_true =  trainingResults$actuals)
trainRMSE


# Since we haven't looked at the test set, we *could* go back and adjust the model.
# Let's continue to the test set evaluation
testPreds   <- predict(fit2, treatedTest)

#Organize training set preds
testResults <- data.frame(actuals   = testSet$listPrice,
                          predicted = testPreds)
head(testResults)

# KPI
testRMSE <- RMSE(y_pred = testResults$predicted,
                 y_true = testResults$actuals)
testRMSE

# Side by Side
trainRMSE
testRMSE


# End

