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
setwd("/cloud/project/Lessons/D_Regression_LogisticRegression/data")

# Data
bball <- read.csv('ncaa.csv')

# Idenfity the informative and target
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
# Takes 5-10m  to run so load a pre-saved copy that I already made 
bestFit <- step(fit, direction='backward')
summary(bestFit)

# Compare model size
length(coefficients(fit))
length(coefficients(bestFit))

# Get predictions
teamPreds <- predict(bestFit,  treatedX, type='response')

# Classify 
cutoff      <- 0.5
teamClasses <- ifelse(teamPreds >= cutoff, 1,0)

# Organize w/Actual
results <- data.frame(actual  = bball$R1.Class.1.win,
                      classes = teamClasses,
                      probs   = teamPreds)
head(results)

# Get a confusion matrix
(confMat <- ConfusionMatrix(results$classes, results$actual))

# What is the accuracy?
sum(diag(confMat)) / sum(confMat)

# This is the actual KPI Accuracy not to be confused with the forecast package function accuracy() which the book uses :(
Accuracy(results$classes, results$actual)

# Visually how well did we separate our classes?
ggplot(results, aes(x=teamPreds, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff), color = 'green')

# AUC
AUC(results$actual,results$classes)

# ROC
ROCobj <- roc(results$classes, results$actual)
plot(ROCobj)


# End
