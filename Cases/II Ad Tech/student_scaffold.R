#' Author: Ted Kwartler
#' Date: Oct 11 2020
#' Purpose: Case 2 scaffold
#' 

# libs
library(vtreat)
library(caret)
library(_____)

# options
options(stringsAsFactors = F)

# wd
setwd("________________")

# Sample
adData <- read.csv('_____________')

set.seed(1234)
idx <- sample(_____________)) #find the sampling code and choose your percentage
train <- adData[idx,]
test  <- adData[-idx,]
              
# Explore
table(train$y_click)
# other EDA

# Modify - Prepare your data for modeling
xVars <- '' #names of the input features
y     <- '' #name of the y variable

# Pick the correct design treatment, C or N for the specific type of business problem
plan <- designTreatmentsX(dframe        = _____, #dataframe
                          varlist       = _____, #input var vector
                          outcomename   = _____, #y name
                          outcometarget = _____) # success class

# Prepare your data using the plan you created
treatedTrain <- _______(plan, train)
treatedTest  <- _______(plan, test)

# Model - log regression
fitLog <- ____(_______ ~., _______, family = 'binomial')

# Model - KNN
knnFit <- train(_________ ~ ., #similar formula to lm
                data = ______, #data input
                method = "knn", #caret has other methods so specify KNN
                preProcess = c("center","scale")) #normalization

# Assess
# Review the model outputs, tuning both based on the predictions on the training set, reviewing **accuracy** of both on records from the training set.
#1. choose variables for the logistic regression 
#2. choose the optimal knn K value by exploring the accuracy of both models on the training data.  
#3. Once you have the "best" model you can make, then prepare the test set, and using your best model make classifications.
#4. Calculate the accuracy of your best model on the **test** set, and compare it with the accuracy of the **training** set

# end