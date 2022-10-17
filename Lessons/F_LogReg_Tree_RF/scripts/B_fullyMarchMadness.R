#' Author: Ted Kwartler
#' Date: Oct 17 2022
#' Purpose: Fit a robust logistic regression on basketball data
#' 

# Libs
library(vtreat)
library(MLmetrics)
library(pROC)
library(ggplot2)
library(readr)

# wd
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Data
bball <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/F_LogReg_Tree_RF/data/ncaa.csv')

# Identify the informative and target
names(bball)
targetVar       <- names(bball)[51]
informativeVars <- names(bball)[3:47]

# Segment the prep data
set.seed(1234)
idx         <- sample(1:nrow(bball),.1*nrow(bball))
prepData    <- bball[idx,]
nonPrepData <- bball[-idx,]
nameKeeps <- bball$Name[-idx] #tracking team name for later

# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(prepData, 
                          informativeVars,
                          targetVar, 1)

# Apply to xVars
treatedX <- prepare(plan, nonPrepData)

# Partition to avoid over fitting
set.seed(2022)
idx        <- sample(1:nrow(treatedX),.8*nrow(treatedX))
train      <- treatedX[idx,]
validation <- treatedX[-idx,]

# Fit a logistic regression model
fit <- glm(R1.Class.1.win ~., data = train, family ='binomial')
summary(fit)

# Backward Variable selection to reduce chances of multi-colinearity
# See chap6 for an explanation
# Takes 1m  to run so load a pre-saved copy that I already made 
#bestFit <- step(fit, direction='backward')
#saveRDS(bestFit, 'bestFit.rds')
bestFit <- readRDS('bestFit.rds')
summary(bestFit)

# Compare model size
length(coefficients(fit))
length(coefficients(bestFit))

# Get predictions
teamPreds <- predict(bestFit,  validation, type='response')
tail(teamPreds)

# Classify 
cutoff      <- 0.5
teamClasses <- ifelse(teamPreds >= cutoff, 1,0)

# Organize w/Actual
results <- data.frame(actual  = validation$R1.Class.1.win,
                      team    = nameKeeps[-idx],
                      seed    = validation$Seeds,
                      classes = teamClasses,
                      probs   = teamPreds)
head(results)

# Get a confusion matrix
(confMat <- ConfusionMatrix(results$classes, results$actual))

# Go to pptx slide 15

# What is the accuracy?
sum(diag(confMat)) / sum(confMat)

# This is the actual KPI Accuracy not to be confused with the forecast package function accuracy() which the book uses :(
Accuracy(results$classes, results$actual)

# Visually how well did we separate our classes?
ggplot(results, aes(x=probs, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff), color = 'green')

# ROC
ROCobj <- roc(results$classes, results$actual)
plot(ROCobj)

# AUC
AUC(results$actual,results$classes)

# Increase the cutoff to improve balanced accuracy
newCutoff <- .65
newClasses <- ifelse(teamPreds >= newCutoff, 1,0)
(confMat <- ConfusionMatrix(newClasses, results$actual))
Accuracy(newClasses, results$actual)

# End
