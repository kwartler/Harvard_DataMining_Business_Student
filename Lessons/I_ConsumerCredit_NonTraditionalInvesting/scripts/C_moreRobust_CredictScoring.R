#' Author: Ted Kwartler
#' Date: 04-05-2020
#' Purpose: Lending Club SEMMA Champion/Challenger - Prob of Default BINARY
#' 

# Libs
library(reshape2)
library(vtreat)
library(caret)
library(randomForest) 
library(dplyr)
library(MLmetrics)

# Variables of interest for this exercise
keeps <-c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "collections_12_mths_ex_med", "y")

# setwd
setwd("/cloud/project/Lessons/I_ConsumerCredit_NonTraditionalInvesting/data")

# custom functions
source('/cloud/project/Lessons/I_ConsumerCredit_NonTraditionalInvesting/scripts/z_trimTrain.R')

# Data Prep
allData <- read.csv('20K_sampleLoans.csv')
allData <- allData[,keeps]

allData$term <- gsub('months', '', allData$term) %>% 
  as.character() %>% 
  as.integer()
allData$revol_util <- gsub('%', '', allData$revol_util) %>% 
  as.character() %>% 
  as.numeric()
allData$int_rate   <- gsub('%', '', allData$int_rate) %>%
  as.character() %>% 
  as.numeric()

# SAMPLE 80/20 split
set.seed(1234)
num          <- (nrow(allData) %/% 10) * 8
idx          <- sample(1:nrow(allData), num)
trainingDF   <- allData[idx, ]
validationDF <- allData[-idx,]

# EXPLORE
table(trainingDF$y)
as.numeric(table(trainingDF$y)[1]/table(trainingDF$y)[2])

names(trainingDF)

# Lending Club Grades look important
aggregate(as.numeric(trainingDF$y), list(trainingDF$grade), mean, na.rm = T)

# Shorter loans are better by far
aggregate(as.numeric(trainingDF$y), list(trainingDF$term), mean, na.rm = T)

# Example text relationship to Y
table(trainingDF[grep('wedding', trainingDF$title,ignore.case = T),27]) #12%
table(trainingDF[grep('business', trainingDF$title, ignore.case = T),27])#23%

# Meh :/ not very informative but you get the idea
plot(trainingDF$loan_amnt, trainingDF$dti)

# MODIFY
consumerCreditVarPlan <- designTreatmentsC(trainingDF,keeps, 'y', 1)
saveRDS(consumerCreditVarPlan, 'FINAL_treatmentPlan.rds')

# Treat both the training and validation sets
treatedTrain      <- prepare(consumerCreditVarPlan, trainingDF)
treatedValidation <- prepare(consumerCreditVarPlan, validationDF)

# Per vtreat authors remove the catB engineered variables as leakage
grep('_catB', names(treatedTrain))
treatedTrain <- treatedTrain[,-grep('_catB', names(treatedTrain))]

# MODEL - no CV but Log Reg, Decision Tree, RF
st <- Sys.time()
logisticFit <- train(as.factor(y) ~ ., 
                     data      = treatedTrain,
                     method    = "glm",
                     family    = "binomial")
difftime(Sys.time(), st)
saveRDS(trimTrain(logisticFit),'FINAL_logisticFit.rds') 
logisticFit <-readRDS('FINAL_logisticFit.rds')

# RF
#st <- Sys.time()
#randomForestFit <- randomForest(as.factor(y) ~., 
#                          data = treatedTrain,
#                          num.trees = 100)
#end <- Sys.time()
#difftime(end, st) #5m vs 35+m w caret
#saveRDS(randomForestFit, 'FINAL_randomForestFit.rds')
randomForestFit <- readRDS('FINAL_randomForestFit.rds')

# ASSESS
# in-sample
logInSample <- predict(logisticFit, treatedTrain, type = 'prob')
rfInSample  <- predict(randomForestFit, treatedTrain, type = 'prob')

# Accuracy
cutoff <- 0.5
Accuracy(ifelse(logInSample[,2]>=cutoff,1,0),treatedTrain$y)
Accuracy(ifelse(rfInSample[,2]>=cutoff,1,0),treatedTrain$y)

# out-of-sample
logOutSample <- predict(logisticFit, treatedValidation, type = 'prob')
rfOutSample  <- predict(randomForestFit, treatedValidation, type = 'prob')

# Accuracy
Accuracy(ifelse(logOutSample[,2]>=cutoff,1,0),treatedTrain$y)
Accuracy(ifelse(rfOutSample[,2]>=cutoff,1,0),treatedTrain$y)

## What are the next steps?  RF overfit, but validation was ok.  More records?, 3 partitions training, validation and holdout? More algos?  Examine the top 100 notes by probability for each method and compare accuracy since that is all you would invest at one time anyway?  It all depends on your time, resources and risk tolerance.

## How would you score new notes...ie productionalize this?
# Get new notes, apply variable treatment, score the new notes, select the notes for investment withg best probabilities

# End