#' Author: Ted Kwartler
#' Date: April 4, 2022
#' Purpose: Lending Club score notes and visualize
#' 

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/I_ConsumerCredit_NonTraditionalInvesting/data")

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
newNotes <- read.csv('OpenNotesJune18_v2.csv')

# Keep the pertinent information
keeps <- c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "collections_12_mths_ex_med", "mths_since_last_major_derog","y")
df    <- df[,keeps]

## Sample
set.seed(2022)
idx        <- sample(1:nrow(df), 18000) #now we feel more comfortable with our model so we increase traning set
training   <- df[idx,]
validation <- df[-idx,]

## Explore
head(training)

## Modify
#Make % a numeric
training$revol_util <- as.numeric(gsub('%', '', training$revol_util))
training$int_rate   <- as.numeric(gsub('%', '', training$int_rate))
training$term       <- as.numeric(gsub(' months','',training$term))

validation$revol_util <- as.numeric(gsub('%', '', validation$revol_util))
validation$int_rate   <- as.numeric(gsub('%', '', validation$int_rate))
validation$term       <- as.numeric(gsub(' months','',validation$term))

newNotes$revol_util <- as.numeric(gsub('%', '', newNotes$revol_util))
newNotes$int_rate   <- as.numeric(gsub('%', '', newNotes$int_rate))
newNotes$mths_since_last_major_derog <- as.numeric(newNotes$mths_since_last_major_derog)


# Now easy variable treatment plan
dataPlan <- designTreatmentsC(dframe        = training, 
                              varlist       = keeps,
                              outcomename   = 'y', 
                              outcometarget = 1)

# Now apply the plan to the data
treatedTrain <- prepare(dataPlan, training)
treatedValid <- prepare(dataPlan, validation)
treatedNew   <- prepare(dataPlan, newNotes)

# Model w/increased CV
crtl <- trainControl(method      = "cv", 
                     number      = 10,
                     verboseIter = TRUE)

# Fit lm model using 10-fold CV: model
finalFit <- train(as.factor(y) ~ ., 
                  data      = treatedTrain, 
                  method    = "glm", 
                  family    = "binomial",
                  trControl = crtl)

# Make predictions for 3 partitions
trainProbs    <- predict(finalFit, treatedTrain, type = 'prob')
testProbs     <- predict(finalFit, treatedValid, type = 'prob')
newNotesProbs <- predict(finalFit, treatedNew, type = 'prob')

# Change the cutoff threshold
cutoff              <- 0.80
cutoffProbsTrain    <- ifelse(trainProbs[,2]>=cutoff, 1, 0) 
cutoffProbsTest     <- ifelse(testProbs[,2]>=cutoff, 1, 0) 
cutoffProbsNewNotes <- ifelse(newNotesProbs[,2]>=cutoff, 1, 0) 

# Accuracy
Accuracy(treatedTrain$y, cutoffProbsTrain)
Accuracy(treatedValid$y, cutoffProbsTest)

table(treatedTrain$y, cutoffProbsTrain)
table(treatedValid$y, cutoffProbsTest)

## Suppose you want to review "A" and low risk notes 20% chance of default
# Organize
testSetComparison <- data.frame(y     = validation$y, #actual outcome
                                grade = validation$grade, #lending club guess
                                risk  = testProbs[,1]) #probability of 0 in model
head(testSetComparison)

# Get their best guess
onlyA <- subset(testSetComparison, testSetComparison$grade == "A")

# Get their best guess with our best risk score
testSetBEST <- subset(testSetComparison, 
                      testSetComparison$grade == "A" & 
                        testSetComparison$risk <= 0.2 )

# How many were 0 in the ENTIRE test set? Random Selection 
sum(testSetComparison$y==0) / nrow(testSetComparison)

# How many were 0 among "A" (their best guess)? Accept their model
sum(onlyA$y==0)/nrow(onlyA)

# How many were "A" and 20% default risk for comparison (our best guess)? Combine their expertise & our model
sum(testSetBEST$y==0) / nrow(testSetBEST)

# Using "A" & "20%" now apply to open/new loans
scoredNotes <- data.frame(id           = 1:nrow(treatedNew),
                          risk         = newNotesProbs[,1],
                          successProb  = newNotesProbs[,2],
                          reward       = newNotes$int_rate,
                          LCgrade      = newNotes$grade)

# Sort  by least risky and examine
scoredNotes <- scoredNotes[order(scoredNotes$risk),]
head(scoredNotes, 10)

# Subset to "A" & 20%; our and their best guess
bestNotes <- subset(scoredNotes, 
                    scoredNotes$LCgrade == "A" & scoredNotes$risk <= 0.2)

# Make a mkt Plot
mktPlot <- figure(legend_location = "bottom_right") %>% 
  ly_points(risk, reward, 
            data  = bestNotes,
            color = LCgrade, glyph = LCgrade,
            hover = list(id, risk, reward, LCgrade)) 
mktPlot

# Make a CAPM-style Risk/Reward Plot
mktPlot2 <- mktPlot %>% 
  ly_abline(a      = 7.0, 
            b      = 0,
            type   = 2, 
            color  = 'red',
            legend = "historical SP500 return") %>% 
  ly_abline(a      = 5.86, 
            b      = 0, 
            type   = 2, 
            color  = 'green', 
            legend = "historical 5yr T-bill")
mktPlot2


# End