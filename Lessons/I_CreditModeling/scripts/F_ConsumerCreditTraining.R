#' Author: Ted Kwartler
#' Date: Oct 31, 2022
#' Purpose: Lending Club Modeling, Cross Validation
#' 

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Options
options(scipen = 999)

# Libraries
#library(rpart) # you can try any of the methods from our class to improve performance
#library(randomForest)
library(dplyr)
library(caret)
library(e1071)
library(vtreat)

# I/O
df <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/I_CreditModeling/data/20K_sampleLoans.csv') 

# Keep the pertinent information
keeps <- c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "collections_12_mths_ex_med", "mths_since_last_major_derog","y")
df    <- df[,keeps]

# Make a data change 
df$revol_util <- as.numeric(gsub('%', '', df$revol_util))
df$int_rate <- as.numeric(gsub('%', '', df$int_rate))

## Sample & Segment the prep data
set.seed(1234)
idx         <- sample(1:nrow(df),.1*nrow(df))
prepData    <- df[idx,]
nonPrepData <- df[-idx,]

## Modify
# Design a "C"ategorical variable plan 
dataPlan <- designTreatmentsC(dframe        = prepData, 
                              varlist       = keeps,
                              outcomename   = 'y', 
                              outcometarget = 1)

# Apply to xVars
treatedX <- prepare(dataPlan, nonPrepData)

# Partition to avoid over fitting
set.seed(2022)
idx        <- sample(1:nrow(treatedX),.8*nrow(treatedX))
training   <- treatedX[idx,]
validation <- treatedX[-idx,]

## Explore
head(training)

## Model
# Now let's do a logistic regression with a 3 fold CV
crtl <- trainControl(method      = "cv", 
                     number      = 3, 
                     verboseIter = TRUE)

# Fit lm model using 3-fold CV: model
fit3 <- train(as.factor(y) ~ ., 
              data      = training,
              method    = "glm",
              family    = "binomial",
              trControl = crtl)
fit3

## Assess in sample
preds <- predict(fit3, training)
table(preds, training$y)
MLmetrics::Accuracy(preds, training$y)

# what happens when we put in original data?
newPreds <- predict(fit3, df)

# Remember to use the treated version of the data
newPreds <- predict(fit3, validation)
table(newPreds, validation$y)
MLmetrics::Accuracy(newPreds, validation$y)

# Let's explore for our use case because equal accuracy isn't appropriate
# predicted=1 but actual=0 is where we lose money
# Organize
losingMoney <- data.frame( probs   = predict(fit3, validation, type = 'prob'),
                           actuals = validation$y)
head(losingMoney) 

# Subset to preds =1, and actual = 0
chk <- subset(losingMoney, losingMoney$probs.1>=0.5 & actuals == 0)
nrow(chk)
table(newPreds, validation$y)

# What is the probability distribution of these misclassifications (false positives)
hist(chk$probs.1)
plot(density(chk$probs.1))
summary(chk$probs.1)

# Depending on the amount of your portfolio and what we learned you may want to increase the cutoff to optimize payoff not overall accuracy
losingMoney$highCutoff <- ifelse(losingMoney$probs.1 > 0.98,1,0)
highThreshold <- table(losingMoney$highCutoff, validation$y) #704 is now 55, much less chance of getting it wrong
highThreshold # more like 10 correctly identified and only 1 misidentified
MLmetrics::Accuracy(losingMoney$highCutoff, validation$y)

# End
