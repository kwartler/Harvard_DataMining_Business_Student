#' Author: Ted Kwartler
#' Date: 2-27-2019
#' Purpose: Fit a robust logistic regression on basketball data
#' 

# Libs
library(vtreat)
library(MLmetrics)
library(pROC)
library(ggplot2)

# wd
setwd("/cloud/project/Lessons/E_KNN/data")

# Data
bball <- read.csv('ncaa.csv')

# Idenfity the informative and target
names(bball)
targetVar       <- names(bball)[51]
informativeVars <- names(bball)[3:47]

# Review to get familiar
head(bball[,1:7])
head(bball[,47:51])

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
#saveRDS(bestFit, 'bestFitNCAA.rds')
#bestFit <-readRDS('bestFitNCAA.rds')
summary(bestFit)

# Compare model size
length(coefficients(fit))
length(coefficients(bestFit))

# Get predictions; note 5 & 6 probabilities and outcome
teamPreds <- predict(bestFit, treatedX,type='response')
head(teamPreds)
head(bball[,c(1:3,51)])

# Classify 
cutoff      <- 0.5
teamClasses <- ifelse(teamPreds >= cutoff, 1,0)

# Organize w/Actual
results <- data.frame(Name                = bball$Name,
                      year                = bball$Year,
                      seed                = bball$Seeds,
                      actual              = bball$R1.Class.1.win,
                      ModelClassification = teamClasses)
head(results,12)

# Get a confusion matrix
(confMat <- ConfusionMatrix(results$ModelClassification, results$actual))

# What is the accuracy?
sum(diag(confMat)) / sum(confMat)

# This is the actual KPI Accuracy not to be confused with the forecast package function accuracy() which the book uses :(
Accuracy(results$ModelClassification, results$actual)

# Visually how well did we separate our classes?
ggplot(results, aes(x=teamPreds, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff), color = 'green')


# End
