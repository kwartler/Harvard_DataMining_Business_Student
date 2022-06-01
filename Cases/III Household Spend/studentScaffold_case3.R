#' Case III Scaffold
#' Ted Kwartler
#' Apr 11 2022

# Options & Set up
setwd("~/Desktop/Harvard_DataMining_Business_Student/Cases/III Household Spend/studentTables")
options(scipen=999)

# Libraries
library(plyr)
library(vtreat)
library(ModelMetrics)

# Get the files
trainingFiles <- list.files(pattern = '_training')
testingFiles  <- list.files(pattern = '_testing')
prospects     <- list.files(pattern = '_prospects')

# Read & Join the files
trainingTables <- lapply(trainingFiles, read.csv)
trainingTables <- join_all(trainingTables, by = 'tmpID')

testingTables <- lapply(testingFiles, read.csv)
testingTables <- join_all(testingTables, by = 'tmpID')

prospectTables <- lapply(prospects, read.csv)
prospectTables <- join_all(prospectTables, by = 'tmpID')

## Sample
set.seed(1234)
idx <- sample(1:nrow(trainingTables), 12000)
trainingTablesSectionA <- trainingTables[idx,]
trainingTablesSectionB <- trainingTables[-idx,]

## Explore -- do more exploration 
names(trainingTablesSectionA)
head(trainingTablesSectionA)
barplot(table(trainingTablesSectionA$NetWorth), las = 2)

## Modify 
# Choose which variables are ethical to use, and others which may not be useful; here I just chose 5 variables
informativeFeatures <- names(trainingTablesSectionA)[c(2:40,43,44,53:58,60:79)]
plan  <- designTreatmentsN(trainingTablesSectionA, 
                           informativeFeatures,
                           'y_householdSpend')

# Prepare all sections of the data
train      <- prepare(plan, trainingTablesSectionA)
validation <- prepare(plan, trainingTablesSectionB)
testing    <- prepare(plan, testingTables)
prospects  <- prepare(plan, prospectTables)

## Model(s)
fitLM <- lm(y_householdSpend ~ ., train)

## Assess all models
summary(fitLM)

trainPreds      <- predict(fitLM, train)
validationPreds <- predict(fitLM, validation)
testingPreds    <- predict(fitLM, testing)

# Choose the best model; evaluate all models and look for consistency among training, validation and test sets.  You dont know the propsect yHat so you can't evaluate that.
rmse(train$y_householdSpend, trainPreds)
rmse(validation$y_householdSpend,validationPreds)

# Make predictions on the prospect file
prospectsPreds  <- predict(fitLM, prospects)
write.csv(prospectsPreds, 'prospectPredictionFile.csv', row.names = F)


# End