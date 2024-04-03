#' Author: Ted Kwartler
#' Date: Oct 31, 2022
#' Purpose: Lending Club score notes and visualize
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
library(MLmetrics)
library(ggplot2)
library(ggthemes)

# I/O
df <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/I_CreditModeling/data/20K_sampleLoans.csv') 

newNotes <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/I_CreditModeling/data/OpenNotesJune18_v2.csv')

# Keep the pertinent information
keeps <- c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "collections_12_mths_ex_med", "mths_since_last_major_derog","y")
df    <- df[,keeps]

# Make a data change 
df$revol_util <- as.numeric(gsub('%', '', df$revol_util))
df$int_rate   <- as.numeric(gsub('%', '', df$int_rate))
df$term       <- as.numeric(gsub(' months','',df$term))
newNotes$revol_bal <- as.numeric(gsub('%', '', newNotes$revol_bal))
newNotes$int_rate  <- as.numeric(gsub('%', '', newNotes$int_rate))
newNotes$term      <- as.numeric(gsub(' months','',newNotes$term))
newNotes$mths_since_last_major_derog <- NA

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

# Model w/increased CV
crtl <- trainControl(method      = "cv", 
                     number      = 10,
                     verboseIter = TRUE)

# Fit lm model using 10-fold CV: model
finalFit <- train(as.factor(y) ~ ., 
                  data      = training, 
                  method    = "glm", 
                  family    = "binomial",
                  trControl = crtl)

# Make predictions for 3 partitions
trainProbs    <- predict(finalFit, training, type = 'prob')
testProbs     <- predict(finalFit, validation, type = 'prob')
treatedNew    <- prepare(dataPlan, newNotes) #remember the new notes need to be treated for the model!
newNotesProbs <- predict(finalFit, treatedNew, type = 'prob')

# Change the cutoff threshold
cutoff              <- 0.80
cutoffProbsTrain    <- ifelse(trainProbs[,2]>=cutoff, 1, 0) 
cutoffProbsTest     <- ifelse(testProbs[,2]>=cutoff, 1, 0) 
cutoffProbsNewNotes <- ifelse(newNotesProbs[,2]>=cutoff, 1, 0) 

# Accuracy
Accuracy(training$y, cutoffProbsTrain)
Accuracy(validation$y, cutoffProbsTest)

table(training$y, cutoffProbsTrain)
table(validation$y, cutoffProbsTest)

## Suppose you want to review "A" and low risk notes 20% chance of default
# Organize
testSetComparison <- data.frame(y     = validation$y, #actual outcome
                                grade = nonPrepData[-idx,]$grade, #lending club guess
                                risk  = testProbs[,1]) #probability of 0 in model
head(testSetComparison)

# Get their best guess
onlyA <- subset(testSetComparison, testSetComparison$grade == "A")

# Get their best guess with our best risk score
testSetBEST <- subset(testSetComparison, 
                      testSetComparison$grade == "A" & 
                      testSetComparison$risk <= 0.1 )

# How many were 0 in the ENTIRE test set? Random Selection 
sum(testSetComparison$y==0) / nrow(testSetComparison)

# How many were 0 among "A" (their best guess)? Accept their model
sum(onlyA$y==0)/nrow(onlyA)

# How many were "A" (their guess) &  10% default risk for comparison (our guess)? Combine their expertise & our model
sum(testSetBEST$y==0) / nrow(testSetBEST)

# Using "A" & "10%" now apply to open/new loans
scoredNotes <- data.frame(id           = 1:nrow(treatedNew),
                          risk         = newNotesProbs[,1],
                          successProb  = newNotesProbs[,2],
                          reward       = newNotes$int_rate,
                          LCgrade      = newNotes$grade)

# Sort  by least risky and examine
scoredNotes <- scoredNotes[order(scoredNotes$risk),]
head(scoredNotes, 10)

# Subset to "A" & 10%; our and their best guess
bestNotes <- subset(scoredNotes, 
                    scoredNotes$LCgrade == "A" & scoredNotes$risk <= 0.1)

# Make a mkt ggplot2
ggplot(data = bestNotes, aes(x= risk, y = reward)) + 
  geom_point() + 
  geom_text(aes(label = id, vjust = -0.5)) +
  theme_gdocs() +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red")  +
  annotate("text", x = .025, y = 7.1, label = "Historical S&P500 return", color = "red") +
  geom_hline(yintercept = 5.86, linetype = "dashed", color = "red") +
  annotate("text", x = .025, y = 5.96, label = "5yr T-Bill Historical Avg Return", color = "red")

# Make an interactive plot
bestNotes |>
  e_charts(risk) |>
  e_scatter( reward, bind = id) |>
  e_tooltip(
    formatter = htmlwidgets::JS("
      function(params){
        return('<strong> noteID: ' + params.name + 
                '</strong><br />risk: ' + params.value[0] + 
                '<br />reward: ' + params.value[1]) 
                }
    ")) |>
  e_toolbox_feature(feature = "dataZoom") |>
  e_mark_line(data = list(yAxis = 7), title = "Historical SP500 Avg Return") |>
  e_mark_line(data = list(yAxis = 5.86), title = "Historical 5yr T-Bill Return")



# End