#' Author: Ted Kwartler
#' Data: 10-9-2019
#' Purpose: Compare 2 algos DT & KNN
#' https://archive.ics.uci.edu/ml/datasets/bank+marketing


## Set the working directory
setwd("/cloud/project/Lessons/F_DT_RF/Data")
options(scipen=999)

## Load the libraries
library(caret)
library(rpart.plot) 
library(vtreat)
library(MLmetrics)

# Data
df <- read.csv('bank-full_v2.csv') 

## EDA
summary(df)
head(df)

# Partitioning
splitPercent <- round(nrow(df) %*% .1) # KNN takes ages, so dont increase!
totalRecords <- 1:nrow(df)
set.seed(1234)
idx <- sample(totalRecords, splitPercent)

trainDat <- df[idx,]
testDat  <- df[-idx,]

# Treatment
targetVar       <- names(trainDat)[17]
informativeVars <- names(trainDat)[1:16]

# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(trainDat, 
                          informativeVars,
                          targetVar,'yes')

treatedTrain <- prepare(plan, trainDat)
treatedTest  <- prepare(plan, testDat)

# Decision Tree
treeFit <- train(y ~., data = treatedTrain, 
             method = "rpart", 
             tuneGrid = data.frame(cp = c(0.01, 0.05)), 
             control = rpart.control(minsplit = 1, minbucket = 2)) 
saveRDS(treeFit, 'comparisonTree.rds')
treeFit <- readRDS('comparisonTree.rds')

# Knn Fit - Takes a LONG time; think about why this is.  Distances are measured from all points!  As a result, KNN is not used on large data sets.
knnFit  <- train(y ~ ., 
                data = treatedTrain, 
                method = "knn", 
                preProcess = c("center","scale"),
                tuneLength = 1)
saveRDS(knnFit, 'comparisonKnn.rds')
knnFit <- readRDS('comparisonKnn.rds')

# Training Set Evaluation
trainPredsTREE <- predict(treeFit, treatedTrain)
trainPredsKNN  <- predict(knnFit, treatedTrain)

# Organize Training
trainingResults <- data.frame(tree   = trainPredsTREE,
                              knn    = trainPredsKNN,
                              actual = treatedTrain$y)

head(trainingResults, 14)

# Conf Matrices Accuracy from the MLmetrics Library
table(trainingResults$tree,trainingResults$actual)
Accuracy(trainingResults$tree,trainingResults$actual)

table(trainingResults$knn, trainingResults$actual)
Accuracy(trainingResults$knn, trainingResults$actual)

# Would still need to predict and evaluate the test set to determine the best and consistent algo.  KNN preds on a lot of records will take a while.

# End
