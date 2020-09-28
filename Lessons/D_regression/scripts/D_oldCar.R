#' Author: Ted Kwartler
#' Date: 9-27-2020
#' Purpose: OldCar Toyota Corolla Regression
#' 

# Libs
library(vtreat)
library(dplyr)
library(ModelMetrics)

# Options
options(scipen=999)

# SetWD
setwd("/cloud/project/Lessons/D_regression/data")

# Dat
cars <- read.csv('oldCar.csv')

# Partitioning 20% test set
splitPercent <- round(nrow(cars) %*% .8)

set.seed(2017)
idx      <- sample(1:nrow(cars), splitPercent)
trainSet <- cars[idx, ]
testSet  <- cars[-idx, ]

# EDA
summary(trainSet)

# Get the column names of our data frame
names(cars)

(informativeFeatureNames <- names(cars)[2:12])
(outcomeVariableName     <- names(cars)[13]) # Or simply "Price"

# Preprocessing & Automated Engineering
# id & constant variable removal, dummy $Fuel_Type
dataPlan     <- designTreatmentsN(cars, 
                                  informativeFeatureNames, 
                                  outcomeVariableName)

treatedTrain <- prepare(dataPlan, trainSet)

#################
### Go to the ppt slide on Multi-Colinearity
#################

# Fit 3 variable model which is has a multicolinearity engineered variable from vtreat 
# Don't worry about how vtreat created the variable...it's math.
fit <- lm(Price ~ Fuel_Type_catP + Fuel_Type_catN + Fuel_Type_catD ,
          data = treatedTrain)

# Did R catch the issue?
summary(fit)

# Since we are comfortable with the data, we can concisely write a lm equation that includes all variables using period instead of an equation
fit <- lm(Price ~ ., treatedTrain)
summary(fit)

#################
### Go to the ppt slides on Summary Output
#################

# Drop uninformative vars
drops                 <- c('CC', 'Automatic', 'Met_Color','Fuel_Type_catN',
                           'Fuel_Type_catD', 'Fuel_Type_lev_x_Diesel',
                           'Fuel_Type_lev_x_Petro')
treatedTrainParsimony <- treatedTrain[, !(names(treatedTrain) %in% drops)]

fit2 <- lm(Price ~ ., treatedTrainParsimony)
summary(fit2)

#################
### Go to the ppt
#################

# Get Training Set Predictions
# Warning can be ignored but for those interested: 
# https://stackoverflow.com/questions/26558631/predict-lm-in-a-loop-warning-prediction-from-a-rank-deficient-fit-may-be-mis
trainingPreds <- predict(fit2, treatedTrainParsimony)

#Organize training set preds
trainingResults <-data.frame(actuals        = treatedTrainParsimony$Price,
                             predicted      = trainingPreds,
                             residualErrors = treatedTrainParsimony$Price-trainingPreds )
head(trainingResults)

# What is the RMSE? 
# Be careful!  Different libraries have subtle differences
# library(ModelMetrics) has rmse(a, p)
# library(MLmetrics) has RMSE(p, a)
ModelMetrics::rmse(trainingResults$actuals, 
                   trainingResults$predicted)

(trainRMSE <- MLmetrics::RMSE(trainingResults$predicted, 
                              trainingResults$actuals))

# What is the MAPE?
(trainMAPE <- MLmetrics::MAPE(trainingResults$predicted, 
                              trainingResults$actuals))

# Since we haven't looked at the test set, we *could* go back and adjust the model.
# Let's continue to the test set evaluation
testPreds <- predict(fit2, testSet)

# Oops!  
# We didn't prepare our data the EXACT same way as the training set and got an error that an expected variable is missing!!
treatedTest <- prepare(dataPlan, testSet)
testPreds   <- predict(fit2, treatedTest) 

#Organize training set preds
testResults <- data.frame(actuals   = testSet$Price,
                          predicted = testPreds)
head(testResults)

# KPI
(testRMSE <- MLmetrics::RMSE(testResults$predicted, 
                             testResults$actuals))

# What is the MAPE?
(testMAPE <- MLmetrics::MAPE(testResults$predicted, 
                             testResults$actuals))

# Side by Side
trainRMSE
testRMSE

trainMAPE
testMAPE

# The book prefers lib(forecast)'s accuracy() function; but accuracy is a KPI for classification so I don't like the function.
forecast::accuracy(testResults$predicted, testResults$actuals)

# End 

