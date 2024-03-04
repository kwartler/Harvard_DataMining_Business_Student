#' Purpose: apply a logistic regression to basketball data
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: Oct 10, 2023
#'

# Libs
library(vtreat)
library(MLmetrics)
library(pROC)
library(ggplot2)

# Data location
filePath <- 'https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/E_Regressions/data/ncaa.csv'

# Data
bball <- read.csv(filePath)

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
# AIC low is better!
# Balance the trade-off between how well the model fits the data and how many inputs are needed
bestFit <- step(fit, direction='backward')
summary(bestFit)

# Compare model size - parsimony!
length(coefficients(fit))
length(coefficients(bestFit))

# Get predictions
teamPreds <- predict(bestFit, type='response')
head(teamPreds)

# Classify
cutoff <- 0.5
teamClasses <- ifelse(teamPreds >= cutoff, 1,0)

# Organize w/Actual
results <- data.frame(Name                = bball$Name,
                      year                = bball$Year,
                      seed                = bball$Seeds,
                      actual              = bball$R1.Class.1.win,
                      winningProbability  = teamPreds,
                      ModelClassification = teamClasses)
head(results,12)

# Check pptx
# Get a confusion matrix
confMat <- ConfusionMatrix(results$ModelClassification, results$actual)
confMat

# What is the accuracy?
sum(diag(confMat)) / sum(confMat)
Accuracy(y_pred = results$ModelClassification, y_true = results$actual)

# Let's bucket probabilities of winning teams to see the distribution
winnningTeams <- subset(results, results$actual==1)
ggplot(winnningTeams, aes(x = winningProbability)) + geom_histogram()

# Let's bucket probabilities of losing teams to see the distribution
losingTeams <- subset(results, results$actual!=1)
ggplot(losingTeams, aes(x = winningProbability)) + geom_histogram() 

# It's best to compare side by side & easier to see as a density plot
# Visually how well did we separate our classes?
ggplot(results, aes(x=winningProbability, color=as.factor(actual))) +
  geom_density() +
  geom_vline(aes(xintercept = cutoff), color = 'green')


# End
