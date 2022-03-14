#' Author: Ted Kwartler
#' Date: 9-23-19
#' Purpose: Fit a robust logistic regression on basketball data
#' 

# Libs
library(vtreat)
library(MLmetrics)
library(pROC)
library(ggplot2)

# wd
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/E_LogReg/data")

# Data
bball <- read.csv('ncaa.csv')

# Identify the informative and target
names(bball)
targetVar       <- names(bball)[51]
informativeVars <- names(bball)[3:47]

# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(bball, 
                          informativeVars,
                          targetVar, 1)

# Apply to xVars
treatedX <- prepare(plan, bball)

# Fit a logistic regression model
fit <- glm(R1.Class.1.win ~., data = treatedX, family ='binomial')
summary(fit)

# Backward Variable selection to reduce chances of multi-colinearity
# See chap6 for an explanation
# Takes 1m  to run so load a pre-saved copy that I already made 
bestFit <- step(fit, direction='backward')
#saveRDS(bestFit, 'bestFit.rds')
bestFit <- readRDS('bestFit.rds')
summary(bestFit)

# Compare model size
length(coefficients(fit))
length(coefficients(bestFit))

# Get predictions
teamPreds <- predict(bestFit,  treatedX, type='response')
tail(teamPreds)

# Classify 
cutoff      <- 0.5
teamClasses <- ifelse(teamPreds >= cutoff, 1,0)

# Organize w/Actual
results <- data.frame(actual  = bball$R1.Class.1.win,
                      team    = bball$Name,
                      seed    = bball$Seeds,
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
newCutoff <- .55
newClasses <- ifelse(teamPreds >= newCutoff, 1,0)
(confMat <- ConfusionMatrix(newClasses, results$actual))
Accuracy(newClasses, results$actual)

# Something more absurd; remember in games 50% is a coin flip
absurdCutoff <- .95
absurdClasses <- ifelse(teamPreds >= absurdCutoff, 1,0)
(confMat <- ConfusionMatrix(absurdClasses, results$actual))
Accuracy(absurdClasses, results$actual)
ROCobj <- roc(absurdClasses, results$actual)
plot(ROCobj)

absurdCutoff <- .001
absurdClasses <- ifelse(teamPreds >= absurdCutoff, 1,0)
(confMat <- ConfusionMatrix(absurdClasses, results$actual))
Accuracy(absurdClasses, results$actual)
ROCobj <- roc(absurdClasses, results$actual)
plot(ROCobj)


# End
