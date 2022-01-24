#' Author: Ted Kwartler
#' Date: 10-30-2020
#' Purpose: Lending Club Modeling, Cross Validation
#' 

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/K_ConsumerCredit_NonTraditionalInvesting/data")

# Trimming Linear Model Function
source('~/Desktop/Harvard_DataMining_Business_Student/Lessons/K_ConsumerCredit_NonTraditionalInvesting/scripts/z_trimTrain.R')

# Libraries
#library(rpart) # you can try any of the methods from our class to improve performance
#library(randomForest)
library(dplyr)
library(caret)
library(e1071)
library(vtreat)

# I/O
df <- read.csv('20K_sampleLoans.csv') 

# Keep the pertinent information; as you explore you can add others or delete
keeps <- c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "collections_12_mths_ex_med", "mths_since_last_major_derog","y")
df    <- df[,keeps]

# Examine
head(df)

## Data Prep
#Make % a numeric
df$revol_util <- gsub('%', '', df$revol_util) %>% 
  as.character() %>% 
  as.numeric()
df$int_rate   <- gsub('%', '', df$int_rate) %>% 
  as.character() %>% 
  as.numeric()

# Now easy variable treatment plan
dataPlan <- designTreatmentsC(dframe        = df, 
                              varlist       = keeps,
                              outcomename   = 'y', 
                              outcometarget = 1)
#saveRDS(dataPlan, 'consumerCredit_dataPlan.rds') #backup bc take a long time

# Now apply the plan to the data
treatedDF <- prepare(dataPlan, df)

# Observe the difference
ncol(df)
ncol(treatedDF)

names(df)
names(treatedDF)

# Partitioning 
num <- (nrow(treatedDF) * .8)
set.seed(1234)
idx <- sample(1:nrow(treatedDF), num)

# Train, Validation 
trainDF      <- treatedDF[idx,]
validationDF <- treatedDF[-idx,]

# Now let's do a logistic regression with a 3 fold CV
crtl <- trainControl(method      = "cv", 
                     number      = 3, 
                     verboseIter = TRUE)

# Fit lm model using 3-fold CV: model
fit3 <- train(as.factor(y) ~ ., 
              data      = trainDF,
              method    = "glm",
              family    = "binomial",
              trControl = crtl)
fit3
preds <- predict(fit3, trainDF)
table(preds, trainDF$y)

# what happens when we put in original data?
newPreds <- predict(fit3, df)

# Remember to use the treated version of the data
newPreds <- predict(fit3, validationDF)

table(newPreds, validationDF$y)

# Let's explore pred=1, actual=0 because equal accuracy isn't appropriate
# Organize
losingMoney <- data.frame( probs = predict(fit3, validationDF, type = 'prob'),
                           actuals = validationDF$y)
head(losingMoney) 

# Subset to preds =1, and actual = 0
chk <- subset(losingMoney, losingMoney$probs.1>=0.5 & actuals == 0)
nrow(chk)
table(newPreds, validationDF$y)

# What is the probability distribution of these misclassifications
hist(chk$probs.1)
summary(chk$probs.1)

# Depending on the amount of your portfolio and what we learned you may want to increase the cutoff to optimize payoff not overall accuracy
losingMoney$highCutoff <- ifelse(losingMoney$probs.1 > 0.999,1,0)
table(losingMoney$highCutoff, validationDF$y) #638 is now 160, much less chance of getting it wrong
sum(diag(table(losingMoney$highCutoff, validationDF$y))) / 
  sum(table(losingMoney$highCutoff, validationDF$y))

# Save the model
# In R GLM model objects are huge, saving a lot of extra info.  This gets rid of a lot of it but retains the ability to make predictions.
fit3 <- trimTrain(fit3)

pth<-'consumerCredict_LogRegCV_fit3.rds'
saveRDS(fit3, pth)

# End