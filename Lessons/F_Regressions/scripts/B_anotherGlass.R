#' Author: Ted Kwartler
#' Date: Mar 10 2025
#' Purpose: Partitioning Schema
#' 

# Libs
library(vtreat)

# Data
wine <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/F_Regressions/data/Wine.csv')

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
wine <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/F_Regressions/data/Wine.csv')

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

# Another example with variable treatment
rm(list=ls())

# Get the data
wine <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/F_Regressions/data/Wine.csv')

# Train 50%/Validation 40% /Variable Treatment 10%
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
prepData <- wine[-c(trainIdx, validationIdx), ]

plan <- designTreatmentsN(prepData, 
                          names(prepData)[1:13],
                          'Proline')

# Apply the plan to both sections for modeling and evaluation next
treatedTrain      <- prepare(plan, trainSet)
treatedValidation <- prepare(plan, validationSet)



# End
