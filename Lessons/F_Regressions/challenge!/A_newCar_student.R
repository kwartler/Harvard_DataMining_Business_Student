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
varTreatIDX <- sample(1:nrow(____), nrow(____)*.1)
varTreatData <- ____[varTreatIDX,]
leftoverData <- ____[-varTreatIDX,]

# Use the leftover data to create a training and validation partition (80/20)
splitPercent <- round(nrow(leftoverData) %*% .8)
set.seed(1234)
idx      <- sample(1:nrow(leftoverData), splitPercent)
trainSet <- leftoverData[idx, ]
testSet  <- leftoverData[-idx, ]

### EXPLORE; you would do more than this in reality
summary(trainSet)

# Build a kernel density plot of the trainSet$listPrice
____(_______(trainSet$_________))

# Get the column names of our data frame
names(trainSet)

### MODIFY, variable treatment
informativeFeatureNames <- names(________)[5:24]
outcomeVariableName     <- _____(________)[25] # Or simply "listPrice"

# Preprocessing & Automated Engineering
# id & constant variable removal, dummy $Fuel_Type
dataPlan     <- design___________(varTreatData,
                                  informativeFeatureNames,
                                  ___________________)

# Apply the plan to the trainSet and testSet
treatedTrain <- _______(________, trainSet)
treatedTest  <- _______(dataPlan, _______)

### MODEL
# Since we are comfortable with the data, we can concisely write a lm equation that includes all variables using period instead of an equation long with listPrice as the dependent variable
fit <- lm(_________ ~ ., ____________)
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
fit2 <- __(_________ ~ ., treatedTrainParsimony)
summary(____)


# Get Training Set Predictions
# Warning can be ignored but for those interested:
# https://stackoverflow.com/questions/26558631/predict-lm-in-a-loop-warning-prediction-from-a-rank-deficient-fit-may-be-mis
trainingPreds <- _______(____, treatedTrainParsimony)

#Organize training set preds
trainingResults <-data.frame(actuals        = treatedTrainParsimony$listPrice,
                             predicted      = trainingPreds,
                             residualErrors = treatedTrainParsimony$listPrice-trainingPreds )
head(trainingResults)

# What is the RMSE?
trainRMSE <- RMSE(y_pred = trainingResults$_________,
                  y_true =  trainingResults$_______)
trainRMSE


# Since we haven't looked at the test set, we *could* go back and adjust the model.
# Let's continue to the test set evaluation using the fit2
testPreds   <- predict(____, ___________)

#Organize training set preds
testResults <- data.frame(actuals   = testSet$listPrice,
                          predicted = testPreds)
head(testResults)

# KPI
testRMSE <- RMSE(y_pred = testResults$_________,
                 y_true = testResults$_______)
testRMSE

# Side by Side
trainRMSE
testRMSE

# Is this a consistent model?

# End

