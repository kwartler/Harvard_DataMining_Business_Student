#' Title: LSA then modeling example
#' Purpose: Apply LSA then create a model
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 23-2020
#'
setwd("/cloud/project/Lessons/K_textMining_documentClassification/data/LSAVersion")

# Libs
library(lsa)
library(tm)
library(text2vec)

# ops
options(stringsAsFactors = F)

# Read in Data
auto <- read.csv('autoTxt.csv')
elec <- read.csv('sciTxt.csv')
test <- read.csv('fakeTestSet.csv')
test$y <- NA # Add in the unknowns

# Combine into a single corpus; unify encoding just in case
allTxt        <- rbind(auto, elec, test)
allTxt$status <- c(rep('training',2000),rep('test',nrow(test)))
allTxt$text   <- enc2utf8(allTxt$text)

# Create custom stop words
stops <- c(stopwords('SMART'), 'car', 'electronic')

txtClean<-function(xVec, stops){
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  xVec <- removeWords(xVec, stops)
  return(xVec)
}

# Clean the text
allTxt$text <- txtClean(allTxt$text, stops)

# Build the Modeling Matrix
# Make vocabulary
iterMaker <- itoken(allTxt$text, 
                    progressbar = T)
textVocab <- create_vocabulary(iterMaker)

#prune vocab to make DTM smaller
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)
# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Finally, make a DTM 
txtDTM <- create_dtm(iterMaker, vectorizer)
dim(txtDTM)

# LSA Needs a TDM, it works on columns, so you have to transpose it!!!!! Takes a few min
#lsaTDM <- lsa(t(as.matrix(txtDTM)), 10)
#saveRDS(lsaTDM, 'lsaTDM.rds')
lsaTDM <- readRDS('lsaTDM.rds')

# Get the modeling part out
modelingVectors <- as.data.frame(lsaTDM$dk)
head(modelingVectors)

# Append the meta back
modelingMatrix <- data.frame(doc_id = allTxt$doc_id, 
                             modelingVectors, 
                             y      = allTxt$y, 
                             status = allTxt$status)

# To save iteration time you can save out copies w/write.csv for each section
lsaTrain <- subset(modelingMatrix, modelingMatrix$status=='training')
lsaTest  <- subset(modelingMatrix, modelingMatrix$status=='test')

### SAMPLE : Patritioning
idx           <- sample(1:2000,.7*2000) 
trainTxt      <- lsaTrain[idx,]
validationTxt <- lsaTrain[-idx,]

### EXPLORE, maybe do word frequency, associations, sentiment, word clouds etc from the original data in addition to the functions below
head(trainTxt)
table(trainTxt$y)

### MODIFY
trainTxt$y <- ifelse(trainTxt$y=='automotive',1,0) # Chg to numeric
trainTxt$doc_id <- NULL # drop unneeded cols
trainTxt$status <- NULL

### MODEL
### with a dense data set you can apply any binary class method not just logistic regression below
# Logistic Reg
fit <- glm(y~., trainTxt, family = 'binomial')

### Assess - ROC Curves, accruacy sensitivity etc
# Training
trainingPreds <- predict(fit, trainTxt, type = c('resp'))
trainingPreds <-ifelse(trainingPreds>=.5,1,0)
table(trainingPreds, trainTxt$y)

# Validation
validationTxt$y      <- ifelse(validationTxt$y=='automotive',1,0)
validationTxt$doc_id <- NULL
validationTxt$status <- NULL

validationPreds <- predict(fit, validationTxt, type = c('resp'))
validationPreds <-ifelse(validationPreds>=.5,1,0)
table(validationPreds, validationTxt$y)

# Once you have a model you are ok with, you can score new data but CANNOT test accuracy because you don't know actuals!  This is just like real life, eventually you could know but not at the time you make predictions.
yHat <- predict(fit, lsaTest)
yHat <-ifelse(yHat>=.5,1,0)
yHat
table(yHat) 
#write.csv(yHat,'scored_documents.csv', row.names = F) #save a copy for your case

# End