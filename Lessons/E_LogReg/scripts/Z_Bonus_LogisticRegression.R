#' Ted Kwartler
#' Date: Oct 4, 2021
#' Goal: Build a default model using logistic regression

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/E_LogReg/data")

# Libraries
library(ggplot2)
library(ggthemes)
library(vtreat)
library(MLmetrics)
library(ROSE)

# Data
ccData <- read.csv('UCI_Credit_Card.csv')

idx <- sample(1:nrow(ccData), 30000*0.8)

# Sample
training <- ccData[idx,]
validation <- ccData[-idx,]

# Explore
head(training)
summary(training)
names(training)
table(training$default.payment.next.month)
ggplot(training, aes(x= MARRIAGE)) + geom_bar() + theme_hc()
ggplot(training, aes(x= SEX)) + geom_bar() + theme_hc()
ggplot(training, aes(x= default.payment.next.month)) + 
  geom_bar() + theme_hc()


# Modify
informativeFeatures <- names(ccData)[2:24]
targetVariable      <- names(ccData)[25]
successClass        <- '1'

plan <- designTreatmentsC(training, 
                          informativeFeatures, 
                          targetVariable, 
                          successClass)

treatedTrain <- prepare(plan, training)
treatedValidation <- prepare(plan, validation)

#### Unbalanced Y, page 141 explains over sampling data to rebalance
# ROSE - Random Over Sample Examples
#trainingRose <- ROSE(default.payment.next.month~., treatedTrain)
#table(trainingRose$data$default.payment.next.month)
# You could use trainingRose and treatedTrain to build two models then compare choosing the more accurate model (assumes balanced classification costs)

# Model
fit <- glm(default.payment.next.month ~., treatedTrain, family = 'binomial')
summary(fit)

# Assess
trainPreds <- predict(fit,  treatedTrain, type='response')
testPreds  <- predict(fit,  treatedValidation, type='response')

# Classify 
cutoff      <- 0.5
trainClasses <- ifelse(trainPreds >= cutoff, 1,0)

# Organize w/Actual
trainResults <- data.frame(ccData$ID[idx],
                           actual  =treatedTrain$default.payment.next.month,
                           probablity = trainPreds,
                           classes = trainClasses)
head(trainResults)

# Get a confusion matrix
(confMat <- ConfusionMatrix(trainResults$classes, trainResults$actual))
Accuracy(trainResults$classes, trainResults$actual)



# End