#' Author: Ted Kwartler
#' Data: Oct 26
#' Purpose: Load data build a random forest tree; this version uses more equally balanced target classes
#' https://archive.ics.uci.edu/ml/datasets/bank+marketing


## Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/G_RF_TimeSeries/data")

# Options
options(scipen=999)

## Load the libraries
library(MLmetrics)
library(caret)
library(rpart.plot) 
library(randomForest)
library(vtreat)

## Bring in some data
dat <- read.csv('bank-downSampled.csv')

# EDA
names(dat)
head(dat)
summary(dat)

# To save time in class, we are only training on 20% of the data
splitPercent <- round(nrow(dat) %*% .2)
totalRecords <- 1:nrow(dat)
set.seed(2021)
idx <- sample(totalRecords, splitPercent)

trainDat <- dat[idx,]
testDat  <- dat[-idx,]

# Treatment
targetVar       <- names(trainDat)[17]
informativeVars <- names(trainDat)[1:16]

# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(trainDat, 
                          informativeVars,
                          targetVar,'yes')

treatedTrain <- prepare(plan, trainDat)
treatedTest  <- prepare(plan, testDat)

# Fit a random forest model with Caret
downSampleFit <- train(Class ~ .,
                      data = treatedTrain,
                      method = "rf",
                      verbose = FALSE,
                      ntree = 3,
                      tuneGrid = data.frame(mtry = 1)) #num of vars used in each tree
downSampleFit

predProbs   <- predict(downSampleFit,  
                       treatedTrain, 
                       type = c("prob"))
predClasses <- predict(downSampleFit,  treatedTrain)

# Confusion Matrix; MLmetrics has the same function but use CARET!!
caret::confusionMatrix(predClasses, as.factor(treatedTrain$Class))

# Other interesting model artifacts
varImp(downSampleFit)
plot(varImp(downSampleFit), top = 20)

# Add more trees to the forest with the randomForest package (caret takes a long time bc its more thorough)
moreVoters <- randomForest(as.factor(Class) ~ .,
                           data  = treatedTrain, 
                           ntree = 500,
                           mtry  = 1)

# Confusion Matrix, compare to 3 trees ~63% accuracy
trainClass <- predict(moreVoters, treatedTrain)
confusionMatrix(trainClass, as.factor(treatedTrain$Class))

# Look at improved var importance
varImpPlot(moreVoters)

# Out of Bag OOB= avg prediction error on each training sample using trees that weren't built with those records (similar to a validation)
#https://en.wikipedia.org/wiki/Out-of-bag_error

# plot the RF with a legend
# https://stackoverflow.com/questions/20328452/legend-for-random-forest-plot-in-r
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(moreVoters, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(moreVoters$err.rate),col=1:4,cex=0.8,fill=1:4)


# Let's optimize # of trees 
someVoters <- randomForest(as.factor(Class) ~ .,
                           data = treatedTrain, 
                           ntree=100,
                           mtry = 1)

# Confusion Matrix
trainClass <- predict(someVoters, treatedTrain)
confusionMatrix(trainClass, as.factor(treatedTrain$Class))

### Now let's apply to the validation test set
threeVotes        <- predict(downSampleFit, treatedTest)
fiveHundredVoters <- predict(moreVoters,    treatedTest)
oneHundredVoters  <- predict(someVoters,    treatedTest)

# Accuracy Comparison from MLmetrics
Accuracy(testDat$Class, threeVotes)
Accuracy(testDat$Class, fiveHundredVoters)
Accuracy(testDat$Class, oneHundredVoters)

# End
