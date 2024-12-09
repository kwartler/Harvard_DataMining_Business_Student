#' Author: Ted Kwartler
#' Date: Dec 5, 2022
#' Purpose: Biased Modeling Example
#'Megacorp is a hypothetical large and successful corporation that makes modern high-tech products. Whenever Megacorp advertises new job vacancies, their human resources team are overwhelmed by the many people who apply for a role. They want an automated process to filter through the resumes, to give them a short list of applicants who match best. Megacorp has a database containing the resumes and hiring results of applicants from the past few years. They track variables like age, gender, education and other details around the job applicant’s profile, and they want to use the text from the resume, including participation in extracurricular activities.

# Set WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")
options(scipen = 999)


# Libs
library(text2vec)
library(caret)
library(tm)
library(glmnet)
library(MLmetrics)
library(vtreat)
library(fairness)

# Custom cleaning function
resumeClean<-function(xVec, stops=stopwords("SMART")){
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  xVec <- removeWords(xVec, stops)
  return(xVec)
}

# Data
candidates <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/M_Technology_Ethics/data/HR%20Hiring%20(Bias%20%26%20Fairness).csv')

### SAMPLE : Partitioning
set.seed(1234)
idx              <- createDataPartition(candidates$Hired,p=.7,list=F)
trainCandidates <- candidates[idx,]
testCandidates  <- candidates[-idx,]

### EXPLORE
head(as.data.frame(trainCandidates),2)

table(trainCandidates$Hired)

### MODIFY
trainCandidates$Summary <- resumeClean(as.character(trainCandidates$Summary))

# Tokenize and Make vocab
iterMaker <- itoken(trainCandidates$Summary)
textVocab <- create_vocabulary(iterMaker, 
                               ngram = c(1,3))

#prune vocab to make DTM smaller
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)
nrow(prunedtextVocab)
prunedtextVocab

# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Take the vocabulary lexicon and the pruned text function to make a DTM 
trainingDTM <- create_dtm(iterMaker, vectorizer)
dim(trainingDTM)

# Examine the DTM
idx <- which(trainingDTM != 0)
trainingDTM[ head(idx),grep('human_resources', colnames(trainingDTM))]

# Append DTM to original data
allCandidateData <- cbind(trainCandidates, as.matrix(trainingDTM))
names(allCandidateData)

# Let's drop ApplicationID,AgeBracket, Gender and raw text Summary
drops <- c('ApplicationID', 'AgeBracket', 'Gender', 'Summary')
allCandidateData <- allCandidateData[, !(names(allCandidateData) %in% drops)]

# Examine all the data used in the model
allCandidateData[head(idx),c(1:ncol(trainCandidates),grep('human_resources', colnames(allCandidateData)))]

# Now let's prepare for modeling by making dummy variables
plan <- designTreatmentsC(allCandidateData, #data
                          names(allCandidateData), #x-var columns
                          'Hired', # y-var name
                          'Yes') #success factor level
#saveRDS(plan, 'variable_treatment_plan.rds')
#plan <- readRDS('variable_treatment_plan.rds')
allCandidateData <- prepare(plan, allCandidateData)

# Separate the y var & engineered variables
allCandidateData <- allCandidateData[,-grep('Hired|_catP', 
                                            names(allCandidateData))]

### MODEL
candidateFit <- cv.glmnet(as.matrix(allCandidateData),
                          y=as.factor(trainCandidates$Hired),
                          alpha=0.9,
                          family='binomial',
                          type.measure='auc',
                          nfolds=3,
                          intercept=F)
#saveRDS(candidateFit, 'candidateFit.rds')

### ANALYZE
# Get Predictions on the training set from all 3 models
trainPreds   <- predict(candidateFit,
                        as.matrix(allCandidateData),
                        type = 'class',
                        s    = candidateFit$lambda.min)
table(trainPreds, trainCandidates$Hired)
Accuracy(trainPreds, trainCandidates$Hired)

## Test Set Prep
# Cleaning
testCandidates$Summary <- resumeClean(as.character(testCandidates$Summary)) #preprocess

# Token & Original training DTM vocab
iterMaker      <- itoken(testCandidates$Summary) 
testDTM        <- create_dtm(iterMaker, vectorizer) 
dim(testDTM)

# Append to non text
testCandidateData <- cbind(testCandidates, as.matrix(testDTM))

# Drops
testCandidateData <- testCandidateData[, !(names(testCandidateData) %in% drops)]

# Dummy var prep
testCandidateData <- prepare(plan, testCandidateData)
testCandidateData <- testCandidateData[,-grep('Hired|_catP', 
                                              names(testCandidateData))]

# Get Test preds
testPreds   <- predict(candidateFit,
                       as.matrix(testCandidateData),
                       type = 'class',
                       s    = candidateFit$lambda.min)
table(testPreds, testCandidates$Hired)
Accuracy(testPreds, testCandidates$Hired)

# Let's investigate further
trainDF <- data.frame(Preds  = trainPreds[,1], 
                      actuals    = trainCandidates$Hired,
                      AgeBracket = trainCandidates$AgeBracket,
                      Gender     = trainCandidates$Gender)
testDF <- data.frame(Preds  = testPreds[,1], 
                     actuals    = testCandidates$Hired,
                     AgeBracket = testCandidates$AgeBracket,
                     Gender     = testCandidates$Gender)
head(trainDF)

# Model behavior
# Test for equal representation "positive class parity" for every one
# 40 and over candidate predicted to be hired, 
# how many under 40 candidates are predicted to be hired? 
# Positive class for >40 / positive class for <40
dem_parity(data = trainDF, 
           outcome = 'actuals', 
           group = 'AgeBracket',
           preds = 'Preds', base = '40 and Over')

# What about gender?  For every male predicted to be hired, 
#how many females are predicted to be hired? 
dem_parity(data = trainDF, 
           outcome = 'actuals', 
           group = 'Gender',
           preds = 'Preds', base = 'Male')

# Uh oh!  What about test set?
dem_parity(data = testDF, 
           outcome = 'actuals', 
           group = 'Gender',
           preds = 'Preds', base = 'Male')

# What about by probability
trainDF$trainProbs <- predict(candidateFit,
                      as.matrix(allCandidateData),
                      type = 'response',
                      s    = candidateFit$lambda.min)
dem_parity(data = trainDF, 
           outcome = 'actuals', 
           group = 'Gender',
           probs = 'trainProbs',
           preds = 'Preds', base = 'Male')


# Since gender was removed, let's figure out whats happening.
genderFit <- cv.glmnet(as.matrix(allCandidateData),
                       y=as.factor(trainCandidates$Gender), #predicting "male"
                       alpha=0.9,
                       family='binomial',
                       type.measure='auc',
                       nfolds=3,
                       intercept=F)

# Subset to impacting terms to identify issues for rebuilding the model
bestTerms <- subset(as.matrix(coefficients(genderFit)), 
                    as.matrix(coefficients(genderFit)) !=0)
bestTerms <- data.frame(term = rownames(bestTerms), value = bestTerms[,1], row.names = NULL)
bestTerms <- bestTerms[order(bestTerms$value, decreasing=T), ] #proxies

# Indicative of "male"
head(bestTerms, 15)

# What about for age?
ageFit <- cv.glmnet(as.matrix(allCandidateData),
                       y=as.factor(trainCandidates$AgeBracket), #predicting "40 and Over"
                       alpha=0.9,
                       family='binomial',
                       type.measure='auc',
                       nfolds=3,
                       intercept=F)

# Subset to impacting terms to identify issues for rebuilding the model
bestTerms <- subset(as.matrix(coefficients(ageFit)), 
                    as.matrix(coefficients(ageFit)) !=0)
bestTerms <- data.frame(term = rownames(bestTerms), value = bestTerms[,1], row.names = NULL)
bestTerms <- bestTerms[order(bestTerms$value, decreasing=T), ] #proxies
# Indicative of "40 and Over"
head(bestTerms, 15)

# End
