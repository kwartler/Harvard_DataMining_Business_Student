#' Author: Ted Kwartler
#' Date: Apr 4 2022
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
df <- read.csv('20K_sampleLoans.csv') 

# Keep the pertinent information
keeps <- c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "collections_12_mths_ex_med", "mths_since_last_major_derog","y")
df    <- df[,keeps]

## Sample
set.seed(2022)
idx        <- sample(1:nrow(df), 16000)
training   <- df[idx,]
validation <- df[-idx,]

## Explore
head(training)

## Modify
#Make % a numeric
training$revol_util <- as.numeric(gsub('%', '', training$revol_util))
training$int_rate   <- as.numeric(gsub('%', '', training$int_rate))
validation$revol_util <- as.numeric(gsub('%', '', validation$revol_util))
validation$int_rate   <- as.numeric(gsub('%', '', validation$int_rate))

# Now easy variable treatment plan
dataPlan <- designTreatmentsC(dframe        = training, 
                              varlist       = keeps,
                              outcomename   = 'y', 
                              outcometarget = 1)
#saveRDS(dataPlan, 'consumerCredit_dataPlan.rds') #backup bc take a long time

# Now apply the plan to the data
treatedTrain <- prepare(dataPlan, training)
treatedValid <- prepare(dataPlan, validation)

# Observe the difference
ncol(training)
ncol(treatedTrain)
names(treatedTrain)

## Model
# Now let's do a logistic regression with a 3 fold CV
crtl <- trainControl(method      = "cv", 
                     number      = 3, 
                     verboseIter = TRUE)

# Fit lm model using 3-fold CV: model
fit3 <- train(as.factor(y) ~ ., 
              data      = treatedTrain,
              method    = "glm",
              family    = "binomial",
              trControl = crtl)
fit3

## Assess in sample
preds <- predict(fit3, treatedTrain)
table(preds, treatedTrain$y)
MLmetrics::Accuracy(preds, treatedTrain$y)

# what happens when we put in original data?
newPreds <- predict(fit3, df)

# Remember to use the treated version of the data
newPreds <- predict(fit3, treatedValid)
table(newPreds, treatedValid$y)
MLmetrics::Accuracy(newPreds, treatedValid$y)

# Let's explore for our use case because equal accuracy isn't appropriate
# predicted=1 but actual=0 is where we lose money
# Organize
losingMoney <- data.frame( probs   = predict(fit3, treatedValid, type = 'prob'),
                           actuals = treatedValid$y)
head(losingMoney) 

# Subset to preds =1, and actual = 0
chk <- subset(losingMoney, losingMoney$probs.1>=0.5 & actuals == 0)
nrow(chk)
table(newPreds, treatedValid$y)

# What is the probability distribution of these misclassifications (false positives)
hist(chk$probs.1)
summary(chk$probs.1)

# Depending on the amount of your portfolio and what we learned you may want to increase the cutoff to optimize payoff not overall accuracy
losingMoney$highCutoff <- ifelse(losingMoney$probs.1 > 0.999,1,0)
highThreshold <- table(losingMoney$highCutoff, treatedValid$y) #704 is now 55, much less chance of getting it wrong
highThreshold
MLmetrics::Accuracy(losingMoney$highCutoff, treatedValid$y)

# End