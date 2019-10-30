#' Author: Ted Kwartler
#' Date: 10-23-2019
#' Purpose: Lending Club score notes and visualize
#' 

# Options
options(scipen=999)

# libs
library(caret)
library(e1071)
library(vtreat)
library(dplyr)
library(rbokeh)
library(MLmetrics)

# Data Directory
setwd("/cloud/project/Lessons/H_Equity_Credit_nonTraditionalMkts/consumerCredit_nonTraditionalMkts/data")

# Training Data
originalNotes <- read.csv("20K_sampleLoans.csv")
newNotes      <- read.csv("OpenNotesJune18_v2.csv")

# Keep the pertinent information; as you explore you can add others or delete
keeps <-c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "collections_12_mths_ex_med", "y")

originalNotes <- originalNotes[,keeps]

## Data Prep
originalNotes$term <- gsub('months', '', originalNotes$term) %>% 
  as.character() %>% 
  as.integer()
originalNotes$revol_util <- gsub('%', '', originalNotes$revol_util) %>% 
  as.character() %>% 
  as.numeric()
originalNotes$int_rate   <- gsub('%', '', originalNotes$int_rate) %>%
  as.character() %>% 
  as.numeric()

# Partition 10% for evaluating 
set.seed(1234)
num          <- (nrow(originalNotes) %/% 10) * 9
idx          <- sample(1:nrow(originalNotes), num)
trainingDF   <- originalNotes[idx, ]
validationDF <- originalNotes[-idx,]

# Make a plan on all the data
dataPlan <-designTreatmentsC(dframe  = trainingDF, 
                             varlist = keeps,
                             outcomename = 'y', 
                             outcometarget = 1)

# Prepare all partitions
treatedTrain <- prepare(dataPlan, trainingDF)
treatedTest  <- prepare(dataPlan, validationDF)
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
finalFit
#finalFit <- trimTrain(finalFit)
#saveRDS(finalFit, 'finalFit.rds')

# Make predictions for 3 paritions
trainProbs    <- predict(finalFit, treatedTrain, type = 'prob')
testProbs     <- predict(finalFit, treatedTest, type = 'prob')
newNotesProbs <- predict(finalFit, treatedNew, type = 'prob')

# Change the cutoff threshold
cutoff              <- 0.80
cutoffProbsTrain    <- ifelse(trainProbs[,2]>=cutoff, 1, 0) 
cutoffProbsTest     <- ifelse(testProbs[,2]>=cutoff, 1, 0) 
cutoffProbsNewNotes <- ifelse(newNotesProbs[,2]>=cutoff, 1, 0) 


# Accuracy
Accuracy(treatedTrain$y, cutoffProbsTrain)
Accuracy(treatedTest$y, cutoffProbsTest)

table(treatedTrain$y, cutoffProbsTrain)
table(treatedTest$y, cutoffProbsTest)


## Suppose you want to review "A" and low risk notes 20% chance of default
# Organize
testSetComparison <- data.frame(y     = treatedTest$y,
                                grade = validationDF$grade,
                                risk  = testProbs[,1])

testSetBEST <- subset(testSetComparison, 
                      testSetComparison$grade == "A" & 
                        testSetComparison$risk <= 0.2 )

# How many were 0 in the test set?  
sum(testSetComparison$y==0) / nrow(testSetComparison)

# How many were "A" and 20% default risk for comparison?
sum(testSetBEST$y==0) / nrow(testSetBEST)

# Using "A" & "20%" now apply to open loans
scoredNotes <- data.frame(id           = 1:nrow(treatedNew),
                          risk         = newNotesProbs[,1],
                          successProb  = newNotesProbs[,2],
                          reward       = newNotes$int_rate,
                          LCgrade      = newNotes$grade)

# Sort  by least risky and examine
scoredNotes <- scoredNotes[order(scoredNotes$risk),]
head(scoredNotes, 10)

# Subset to "A" & 20%
bestNotes <- subset(scoredNotes, scoredNotes$LCgrade == "A" & scoredNotes$risk <= 0.2)


# Make a mkt Plot
mktPlot <- figure(legend_location = "bottom_right") %>% 
  ly_points(risk, reward, 
            data  = bestNotes,
            color = LCgrade, glyph = LCgrade,
            hover = list(id, risk, reward, LCgrade)) 
mktPlot

# Make a CAPM Plot
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