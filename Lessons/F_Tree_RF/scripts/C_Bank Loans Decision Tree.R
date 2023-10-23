#' Author: Ted Kwartler
#' Data: Oct 23, 2023
#' Purpose: Load data build a decision tree
#' https://archive.ics.uci.edu/ml/datasets/bank+marketing


## Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")
options(scipen=999)

## Load the libraries
library(caret)
library(rpart.plot) #visualizing

## Bring in some data
dat <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/F_Tree_RF/data/bank2.csv') 

# Partitioning
splitPercent <- round(nrow(dat) %*% .9)
totalRecords <- 1:nrow(dat)
set.seed(1234)
idx <- sample(totalRecords, splitPercent)

trainDat <- dat[idx,]
testDat  <- dat[-idx,]

## EDA
summary(trainDat)
head(trainDat)
table(trainDat$y)

# Force a full tree (override default parameters)
# Don't do this one in your case!
overFit <- rpart(as.factor(y) ~ ., 
                 data = trainDat, 
                 method = "class", 
                 minsplit = 1, 
                 minbucket = 1, 
                 cp=-1)

# Look at all the rules!!
overFit

# Don't bother plotting, takes a while but a copy is saved in the data folder.
#prp(overFit, extra = 1)

# Look at training probabilities
trainProbs <- predict(overFit, trainDat) 
head(trainProbs, 10)

# Get the final class and actuals
trainClass <- data.frame(class = colnames(trainProbs)[max.col(trainProbs)],
                         actual = trainDat$y)
head(trainClass, 10)

# Confusion Matrix
confMat <- table(trainClass$class,trainClass$actual)
confMat

# Accuracy
sum(diag(confMat))/sum(confMat)

# Now predict on the test set
testProbs <- predict(overFit, testDat)

# Get the final class and actuals
testClass<-data.frame(class  = colnames(testProbs)[max.col(testProbs)],
                      actual = testDat$y)

# Confusion Matrix
confMat <- table(testClass$class,testClass$actual)
confMat

# Accuracy
sum(diag(confMat))/sum(confMat)

# Start over
rm(list=ls())

##########
dat <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/F_Tree_RF/data/bank-full_v2.csv') # now a bit more data to approximate real scenario 


# To save time in class, we are only training on 50% of the data
splitPercent <- round(nrow(dat) %*% .5)
totalRecords <- 1:nrow(dat)
set.seed(1234)
idx <- sample(totalRecords, splitPercent)

trainDat <- dat[idx,]
testDat  <- dat[-idx,]


## EDA
summary(trainDat)
head(trainDat)

# No modification needed in this cleaned up data set.  One could engineer some interactions though.

# Fit a decision tree with caret
set.seed(1234)
fit <- train(as.factor(y) ~., #formula based
             data = trainDat, #data in
             #instead of knn, caret does "recursive partitioning (trees)
             method = "rpart", 
             #Define a range for the CP to test
             tuneGrid = data.frame(cp = c(0.0001, 0.001,0.01, 0.05, 0.07)), 
             #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
             control = rpart.control(minsplit = 1, minbucket = 2)) 

# Or without the comments:
#fit <- train(y ~., data = trainDat, 
#             method = "rpart", 
#             tuneGrid = data.frame(cp = c(0.01, 0.05)), 
#             control = rpart.control(minsplit = 1, minbucket = 2)) 

# Since it takes a long time to fit there is a pre-saved version
# saveRDS(fit,'fullDataTreeFit.rds')
# fit <- readRDS('fullDataTreeFit.rds')
# Examine
fit

# Plot the CP Accuracy Relationship to adust the tuneGrid inputs
dev.off()
plot(fit)

# Plot a pruned tree
dev.off()
prp(fit$finalModel, extra = 1)

# Make some predictions on the training set
trainCaret <- predict(fit, trainDat)
head(trainCaret)

# Get the conf Matrix
confusionMatrix(trainCaret, as.factor(trainDat$y))

# Now more consistent accuracy & fewer rules!
testCaret <- predict(fit,testDat)
confusionMatrix(testCaret,as.factor(testDat$y))

# End
