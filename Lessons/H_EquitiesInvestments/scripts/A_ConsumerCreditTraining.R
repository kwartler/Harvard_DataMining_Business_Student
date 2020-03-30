#' Author: Ted Kwartler
#' Date: 03-30-2020
#' Purpose: Lending Club Modeling, Cross Validation
#' 

# WD
setwd("~/Documents/Harvard_DataMining_Business_Student/Lessons/H_EquitiesInvestments/data")

# Libraries
library(rpart)
library(randomForest)
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
#saveRDS(dataPlan, 'dataPlan.rds') #backup bc take a long time

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

# Save the treatment plan
pth <- 'treatmentPlan_fit3.rds'
saveRDS(dataPlan, pth)

# Save the model
# In R GLM model objects are huge, saving a lot of extra info.  This gets rid of a lot of it but retains the ability to make predictions.
trimTrain <- function(object, ...) {
  removals <- c("results", "pred", "bestTune", "call", "dots",
                "metric", "trainingData", "resample", "resampledCM",
                "perfNames", "maxmimize", "times")
  for(i in removals)
    if(i %in% names(object)) object[i] <- NULL
    c_removals <- c('method', 'number', 'repeats', 
                    'p', 'initialWindow', 'horizon',
                    'fixedWindow', 'verboseIter', 'returnData', 
                    'returnResamp', 'savePredictions', 
                    'summaryFunction', 'selectionFunction',
                    'index', 'indexOut', 'indexFinal',
                    'timingSamps', 'trim', 'yLimits')
    for(i in c_removals)
      if(i %in% names(object$control)) object$control[i] <- NULL  
    if(!is.null(object$modelInfo$trim))
      object$finalModel <- object$modelInfo$trim(object$finalModel)
    object
}

fit3 <- trimTrain(fit3)

pth<-'LogRegCV_fit3.rds'
saveRDS(fit3, pth)

# End