#' Author: Ted Kwartler
#' Data: 3-8-2020
#' Purpose: Load data build a random forest tree
#' https://archive.ics.uci.edu/ml/datasets/bank+marketing


## Set the working directory
setwd("/cloud/project/lessons/6_Mar6_DT_RF/wk6_Data")

# Options
options(scipen=999)

## Load the libraries; 1st time use install.packages('ggplot2')
library(caret)
library(rpart.plot) #visualizing
library(randomForest)
library(vtreat)
library(MLmetrics)

## Bring in some data
df <- read.csv('bank-full_v2.csv')

# EDA
summary(df)

# Partition
set.seed(1234)
# To save time in class, we are only training on 20% of the data
splitPercent <- round(nrow(df) %*% .2)
totalRecords <- 1:nrow(df)
idx <- sample(totalRecords, splitPercent)

trainDat <- df[idx,]
testDat  <- df[-idx,]

# Unbalanced class
tallyY <- table(df$y)

tallyY
tallyY[2]/tallyY[1]

# Down sample all those No Values! Other methods here: 
#https://topepo.github.io/caret/subsampling-for-class-imbalances.html
downDat <- downSample(x = df[, -ncol(df)], y = df$y)

# Balanced class
table(downDat$Class) #####~~changed name to class~~######

# Partition DownSampled using a different function
set.seed(1234)
idx <- createDataPartition(y = downDat$Class, p= 0.8, list=F)
trainDat <- downDat[idx,]
testDat  <- downDat[-idx,]

# Treatment
targetVar       <- names(trainDat)[17]
informativeVars <- names(trainDat)[1:16]

# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(trainDat, 
                          informativeVars,
                          targetVar,'yes')

treatedTrain <- prepare(plan, trainDat)
treatedTest  <- prepare(plan, testDat)

# Fit a down sampled model with Caret
downSampleFit <- train(Class ~ .,
                      data    = treatedTrain,
                      method  = "rf",
                      verbose = FALSE,
                      ntree   = 3,tuneGrid = data.frame(mtry = 1))

# Predictions
predProbs   <- predict(downSampleFit, 
                       treatedTrain, 
                       type = c("prob"))
predClasses <- predict(downSampleFit, 
                       treatedTrain)

downSampleFit

# Confusion Matrix
table(predClasses, treatedTrain$Class)

# Other interesting model artifacts
varImp(downSampleFit)
plot(varImp(downSampleFit), top = 20)

# See an example tree; but can be a weak one
getTree(downSampleFit$finalModel,1, labelVar=TRUE)

# Add more trees to the forest with the randomForest package (caret takes a long time bc its more thorough)
#moreVoters <- randomForest(Class ~ ., 
#                           data = treatedTrain, 
#                           ntree=500)
#saveRDS(moreVoters, 'moreVoters.rds')
moreVoters <- readRDS('moreVoters.rds')

# Confusion Matrix, compare to 3 trees 66% accuracy
trainClass <- predict(moreVoters, treatedTrain)
(confMat   <- table(trainClass, treatedTrain$Class))
sum(diag(confMat)) / sum(confMat)

# Review a tree
head(getTree(moreVoters,1, labelVar=TRUE),27)

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
someVoters <- randomForest(Class ~ .,
                           data = treatedTrain, 
                           ntree=100)

# Confusion Matrix, compare to 100 trees ~73.8% accuracy
trainClass        <- predict(someVoters, treatedTrain)
(lessTreesConfMat <- table(trainClass, treatedTrain$Class))
someVoters
sum(diag(lessTreesConfMat)) / sum(lessTreesConfMat)

### Now let's apply to the validation test set
threeVotes        <- predict(downSampleFit, treatedTest)
fiveHundredVoters <- predict(moreVoters,    treatedTest)
oneHundredVoters  <- predict(someVoters,    treatedTest)

# Accuracy Comparison
Accuracy(treatedTest$Class, threeVotes)
Accuracy(treatedTest$Class, fiveHundredVoters)
Accuracy(treatedTest$Class, oneHundredVoters)

# End