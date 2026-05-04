#' Author: Ted Kwartler (Updated for fairmodels)
#' Date: May 4, 2026
#' Purpose: Biased Modeling Example

# Options
options(scipen = 999)

# Libs
library(text2vec)
library(caret)
library(tm)
library(glmnet)
library(MLmetrics)
library(vtreat)
# library(fairness) # Deprecated
library(fairmodels) # Modern alternative
library(DALEX)      # Required for fairmodels

# Data Loading & Clearning
resumeClean <- function(xVec, stops=stopwords("SMART")){
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  xVec <- removeWords(xVec, stops)
  return(xVec)
}

# Data
candidates <- read.csv('https://github.com/kwartler/teaching-datasets/raw/refs/heads/main/HR%20Hiring%20(Bias%20&%20Fairness).csv')

# Sampling
set.seed(1234)
idx             <- createDataPartition(candidates$Hired, p=.7, list=F)
trainCandidates <- candidates[idx,]
testCandidates  <- candidates[-idx,]

# Cleaning & DTM Maker
trainCandidates$Summary <- resumeClean(as.character(trainCandidates$Summary))
iterMaker       <- itoken(trainCandidates$Summary)
textVocab       <- create_vocabulary(iterMaker, ngram = c(1,3))
prunedtextVocab <- prune_vocabulary(textVocab, term_count_min = 10,
                                    doc_proportion_max        = 0.5, 
                                    doc_proportion_min        = 0.001)
vectorizer  <- vocab_vectorizer(prunedtextVocab)
trainingDTM <- create_dtm(iterMaker, vectorizer)

# Append data & drop inappropriate vars
allCandidateData <- cbind(trainCandidates, as.matrix(trainingDTM))
drops <- c('ApplicationID', 'AgeBracket', 'Gender', 'Summary')
modelingData <- allCandidateData[, !(names(allCandidateData) %in% drops)]

# Design Treatment 
plan <- designTreatmentsC(modelingData, names(modelingData), 'Hired', 'Yes')
preparedTrain <- prepare(plan, modelingData)
preparedTrain <- preparedTrain[,-grep('Hired|_catP', names(preparedTrain))]

# Model Fit
candidateFit <- cv.glmnet(as.matrix(preparedTrain),
                          y=as.factor(trainCandidates$Hired),
                          alpha=0.9, 
                          family='binomial', 
                          type.measure='auc', 
                          nfolds=3, 
                          intercept=F)

# Check the model behavior

# Create a DALEX Explainer container (standardized object across model types)
explainer <- explain(candidateFit, 
                     data = as.matrix(preparedTrain), 
                     y = ifelse(trainCandidates$Hired == "Yes", 1, 0),
                     label = "Lasso Model",
                     type = "classification",
                     predict_function = function(m, x) { #chatGPT helped here
                       as.numeric(predict(m, x, type = "response", s = "lambda.min"))
                     })

# Check fairness (Demographic Parity)
fobject <- fairness_check(explainer, 
                          protected = as.factor(trainCandidates$Gender), #protected var
                          privileged = "Male")

# NOTE FOR STUDENTS: 
# 'Statistical Parity Ratio'
# - A ratio of 1.0 means perfect equality (equal representation).
# - A ratio of 0.5 means the "unprivileged" group (Female) is being predicted for 
#   hire at only 50% the rate of the "privileged" group (Male).
# - Generally, a ratio below 0.8 is considered evidence of significant bias (the 4/5ths rule).
print(fobject)
plot(fobject)

# Since Gender was NOT in the model, why is there bias? 
# We build a model to predict Gender using the other features.
genderFit <- cv.glmnet(as.matrix(preparedTrain),
                       y=as.factor(trainCandidates$Gender),
                       alpha=0.9, family='binomial', nfolds=3, intercept=F)

# Extract terms that are highly predictive of "Male"
bestTerms <- as.matrix(coef(genderFit, s = "lambda.min"))
bestTerms <- data.frame(term = rownames(bestTerms), value = bestTerms[,1])
bestTerms <- bestTerms[bestTerms$value != 0 & bestTerms$term != "(Intercept)", ]
bestTerms <- bestTerms[order(bestTerms$value, decreasing=T), ]

# These terms are the "Proxies" - words in resumes that correlate with being Male
# even though 'Gender' was deleted from the dataset.
cat("Top words/features predicting Male gender:\n")
head(bestTerms, 15)

# Repeat for Age
ageFit <- cv.glmnet(as.matrix(preparedTrain),
                    y=as.factor(trainCandidates$AgeBracket),
                    alpha=0.9, family='binomial', nfolds=3, intercept=F)

ageTerms <- as.matrix(coef(ageFit, s = "lambda.min"))
ageTerms <- data.frame(term = rownames(ageTerms), value = ageTerms[,1])
ageTerms <- ageTerms[ageTerms$value != 0 & ageTerms$term != "(Intercept)", ]
ageTerms <- ageTerms[order(ageTerms$value, decreasing=T), ]

cat("\nTop words/features predicting '40 and Over':\n")
head(ageTerms, 15)
