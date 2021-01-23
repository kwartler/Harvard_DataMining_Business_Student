#' Author: Ted Kwartler
#' Data: 5-29-2018
#' Purpose: Load data build a KNN Predictor

## Set the working directory
setwd("/cloud/project/Lessons/E_LogReg_KNN/data")

## Load the libraries
library(caret)
library(e1071)
library(MLmetrics)

## Bring in some data
dat <- read.csv('Absenteeism_at_work_v3.csv')

# Drop the unique employee ID
dat$ID <- NULL

# Data partitioning
set.seed(1234)
splitPercent <- round(nrow(dat) %*% .9)
totalRecords <- 1:nrow(dat)
idx <- sample(totalRecords, splitPercent)

trainDat <- dat[idx,]
testDat  <- dat[-idx,]

# We don't have to tell train to do something different, because the target is continuous the KNN algorithm will automatically get avg neighbor values not categories
knnFit <- train(Absenteeism.time.in.hours ~ ., 
                data = trainDat, 
                method = "knn", 
                preProcess = c("center","scale"), 
                tuneLength = 10)

# Evaluation
knnFit
plot(knnFit)

# training set accuracy & review the top 10
trainPreds <- predict(knnFit,trainDat)
RMSE(trainPreds,trainDat$Absenteeism.time.in.hours)
data.frame(predictions = head(trainPreds, 10),
           actuals     = head(trainDat$Absenteeism.time.in.hours, 10))

# Testing set accuracy
testPreds <- predict(knnFit,testDat)
RMSE(testPreds,testDat$Absenteeism.time.in.hours)
data.frame(predictions = head(testPreds, 10),
           actuals     = head(testDat$Absenteeism.time.in.hours, 10))

# End

