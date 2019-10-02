#' Author: Ted Kwartler
#' Data: 2-27-2019
#' Purpose: Load data build a KNN classifier

## Set the working directory
setwd("/cloud/project/Lessons/E_KNN/data")

## Load the libraries
library(caret)
library(e1071)
library(plyr)
library(class)

## Bring in some data
dat <- read.csv('Absenteeism_at_work_v3.csv')

## Explore to get familiar
# Dimensions
dim(dat)

# Summary Stats
summary(dat)

# Head
head(dat)

# Get the appropriate X predictors & scale
xDat <- dat[,-c(1,2)]

# Scaling; we don't actually use this code since we're using library(caret) though
xDat <- scale(xDat, scale=T, center=T)

# Examine scaling mean across all columns
summary(xDat)

# Drop the unique employee ID
dat$ID <- NULL

#  Tally reason codes
table(dat$Reason.for.absence)

# Data partitioning
set.seed(1234)
splitPercent <- round(nrow(dat) %*% .9)
totalRecords <- 1:nrow(dat)
idx          <- sample(totalRecords, splitPercent)

trainDat <- dat[idx,]
testDat  <- dat[-idx,]

# The caret package is robust and lets you apply the needed scaling during the fit
knnFit <- train(Reason.for.absence ~ ., #similar formula to lm
                data = trainDat, #data input
                method = "knn", #caret has other methods so specify KNN
                preProcess = c("center","scale")) #normalization

# Evaluation
knnFit
plot(knnFit)

# You can also expand the K parameter search with the following parameter addition
knnFit <- train(Reason.for.absence ~ ., data = trainDat, method = "knn", 
                preProcess = c("center","scale"), tuneLength = 10)
knnFit
plot(knnFit)

# training set accuracy
trainClasses <- predict(knnFit,trainDat)
resultsDF    <- data.frame(actual = trainDat$Reason.for.absence, 
                        classes = trainClasses)
head(resultsDF)

confusionMatrix(trainClasses,trainDat$Reason.for.absence) #predictions then reference (actual)

# Testing set accuracy; PREDICT WILL CENTER THE NEW DATA FOR YOU!!
testClasses <- predict(knnFit,testDat)
confusionMatrix(testClasses,testDat$Reason.for.absence)

# To see probabilities 
trainProbs <- predict(knnFit, trainDat, type=c('prob'))
head(trainProbs)

# What is the column with the maximum value for each record? ONLY first 6 as example
topProb <- max.col(head(trainProbs))

# Get the name of the top valued probability
names(trainProbs)[topProb]

# Is this the same as predicting the classes directly?
head(as.character(trainClasses))

# And now the clunky knn book version!  Used if your k is less than 5 or needs to be specified.  From the class library
y                           <- as.factor(trainDat$Reason.for.absence)
trainDat$Reason.for.absence <- NULL
testDat$Reason.for.absence  <- NULL
results                     <- knn(trainDat, testDat, 
                                   cl = y , k = 3, prob=F)
head(results)
#results                     <- knn(trainDat, testDat, 
#                                   cl = y , k = 3, prob=TRUE)
#head(attributes(results)$prob)

# End
