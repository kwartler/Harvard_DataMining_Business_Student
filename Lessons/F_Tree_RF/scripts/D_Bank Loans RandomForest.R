#' Author: Ted Kwartler
#' Data: Mar 21, 2022
#' Purpose: Load data build a random forest tree; this version uses more equally balanced target classes
#' https://archive.ics.uci.edu/ml/datasets/bank+marketing


## Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Options
options(scipen=999)

## Load the libraries
library(MLmetrics)
library(caret)
library(rpart.plot) 
library(randomForest)
library(vtreat)

## Bring in some data
dat <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/F_Tree_RF/data/bank-downSampled.csv')

# EDA
names(dat)
head(dat)
summary(dat)

# Prep and non prep
set.seed(2022)
idxPrep        <- sample(1:nrow(dat),.1*nrow(dat))
prepData    <- dat[idxPrep,]
nonPrepData <- dat[-idxPrep,]

# Treatment
targetVar       <- names(prepData)[17]
informativeVars <- names(prepData)[1:16]


# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(prepData, 
                          informativeVars,
                          targetVar,'yes')

# Partition to avoid overfitting
set.seed(1234)
idx        <- sample(1:nrow(nonPrepData),.8*nrow(nonPrepData))
train      <- nonPrepData[idx,]
validation <- nonPrepData[-idx,]

# Now apply the variable treatment plan
treatedTrain <- prepare(plan, train)
treatedTest  <- prepare(plan, validation)

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

# Confusion Matrix
caret::confusionMatrix(predClasses, 
                       as.factor(treatedTrain$Class))

# Other interesting model artifacts
varImp(downSampleFit)
plot(varImp(downSampleFit), top = 20)

# Add more trees to the forest with the randomForest package (caret takes a long time bc its more thorough, with x-validation)
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

# This code helps to determine mtry with OOB errors;
# one could go back and use this mtry but in this data set it's 
# pretty high so still may not be optimal
#optimalRF <- tuneRF(x = treatedTrain[,1:59], 
#                    y = ifelse(treatedTrain[,60]=='yes',1,0), 
#                    stepFactor=1.5)

### Now let's apply to the validation test set
threeVotes        <- predict(downSampleFit, treatedTest)
fiveHundredVoters <- predict(moreVoters,    treatedTest)
oneHundredVoters  <- predict(someVoters,    treatedTest)

# Accuracy Comparison from MLmetrics
Accuracy(treatedTest$Class, threeVotes)
Accuracy(treatedTest$Class, fiveHundredVoters)
Accuracy(treatedTest$Class, oneHundredVoters)

# End
