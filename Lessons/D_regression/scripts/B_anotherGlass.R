#' Author: Ted Kwartler
#' Date: 7-5-2018
#' Purpose: Partitioning Schema
#' 

# Libs

# Setwd
setwd("/cloud/project/Lessons/D_regression/data")

# Data
wine <- read.csv('Wine.csv')

# Train 90%/Test 10% Partitioning
splitPercent  <- round(nrow(wine) %*% .9)
(totalRecords <- 1:nrow(wine))
(idx          <- sample(totalRecords, splitPercent))

# Remember its row, then columns with the indexing operation.  Here you use the vector of numbers in the "row" position to create a training set and the minus for test set.
trainSet <- wine[idx, ]
testSet  <- wine[-idx, ]

# Dimensions
dim(trainSet)
dim(testSet)

## Now you can train a model on the trainSet and review realistic results on the testSet

# Start over
rm(list=ls())

# Data
wine <- read.csv('Wine.csv')

# Train 50%/Validation 40% /Testing 10%
trainPercent      <- round(nrow(wine) %*% .5)
validationPercent <- round(nrow(wine) %*% .4)

# Sample index for training
trainIdx <- sample(1:nrow(wine), trainPercent)

# Identify the rows not in the training set, its the "difference" 
remainingRows <- setdiff(1:nrow(wine), trainIdx)

# Create another sample but limit the row numbers to only those identified as *not* in training to get the validation index
validationIdx <-sample(remainingRows, validationPercent)

# With the two idx vectors of randomly generated numbers, without any overlap you can put them in the "row" position for indexing. 
trainSet      <- wine[trainIdx, ]
validationSet <- wine[validationIdx, ]

# Here you combine both the index and put that with a minus.  Essentially removing any rows in training, or validation indexing leaving you with the test set.
testSet <- wine[-c(trainIdx, validationIdx), ]

# Chk
nrow(trainSet) + nrow(validationSet) + nrow(testSet)
nrow(wine)

# End
