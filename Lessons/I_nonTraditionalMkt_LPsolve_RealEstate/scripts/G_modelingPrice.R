#' Author: Ted Kwartler
#' Date: Nov 7, 2022
#' Purpose: Quick Examination of real rental history data
#' 

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")
options(scipen = 999)

# Libs
library(readr)
library(vtreat)
library(rpart)
library(rpart.plot)
library(caret)
library(MLmetrics)
library(ggplot2)
library(ggthemes)
library(reshape)

# Data
capeCod <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/I_nonTraditionalMkt_LPsolve_RealEstate/data/capeCod_Nov2022.csv')

# Sample for Variable Treatment
set.seed(1234)
idxTreatment <- sample(1:nrow(capeCod), 0.1 * nrow(capeCod))
dataPrepData <- capeCod[idxTreatment,]

informativeFeatures <- c("PROPERTY.TYPE", "BEDS", "BATHS", "Community", "SQUARE.FEET", "LOT.SIZE", "YEAR.BUILT", 
                         "DAYS.ON.MARKET", "HOA.MONTH", "SOURCE", "style", "pricePerSqFt", "redfinEst")

plan <- designTreatmentsN(dataPrepData,
                          informativeFeatures, 
                          'PRICE')

# Treat all the data
allData <- prepare(plan, capeCod)

# Remove the 35 rows used in the variable plan
property <- allData[-idxTreatment,]
property$URL <- capeCod$URL[-idxTreatment] # append an ID

## Sample
set.seed(2022)
idx <- sample(1:nrow(property), 0.8 * nrow(property))
training <- property[idx,]
validation <- property[-idx,]
validation$URL <- property$URL[-idx]# append an ID

## Explore
summary(training)
plot(density(training$PRICE))

## Modify - let's engineer a variable to see if it helps our decision tree, one could do more of course but this is an example
training$sizeBed <- training$SQUARE_FEET / training$BEDS
training$sizeBath <- training$SQUARE_FEET / training$BATHS


## Models
drops <- c('URL', 'redfinEst') #remove URL from informative features or any others for experimentation
reducedTraining <- training[,!(names(training) %in% drops)]
fitDT <- train(PRICE ~., data = reducedTraining, 
             method = "rpart", 
             tuneGrid = data.frame(cp = c(0.001, 0.005, 0.01, 0.05, 0.1)), 
             control = rpart.control(minsplit = 1, minbucket = 2)) 

fitLM <- lm(PRICE ~., data = reducedTraining) #remove URL from informative features)


## Assess

# DT
fitDT
plot(fitDT)
# Plot a pruned tree
prp(fitDT$finalModel, extra = 1)

# LM
summary(fitLM)

# Make some predictions on the training set
predsDF <- data.frame(DTpreds      = predict(fitDT, reducedTraining),
                      LMpreds      = predict(fitLM, reducedTraining),
                      actualPrice  = reducedTraining$PRICE,
                      URL          = training$URL)

# Another way to pivot longer for plotting
longPredsDF <- melt(predsDF)

# How much did the models agree?
ggplot(longPredsDF, aes(x = value, fill = variable)) + geom_density(alpha = 0.25) + theme_gdocs()

# RMSE DT
RMSE(predsDF$DTpreds, predsDF$actual)

# RMSE LM
RMSE(predsDF$LMpreds, predsDF$actual)

# Sometimes an "ensemble" model will improve results because the models learn differently [not here though bc the data is small]
RMSE(apply(predsDF[,1:2], 1, mean), predsDF$actual)

# Now predict on validation **this is a VERY tiny data set, results are illustrative only**
# Validation needs the same engineered variables to avoid a predict error
validation$sizeBed <- validation$SQUARE_FEET / validation$BEDS
validation$sizeBath <- validation$SQUARE_FEET / validation$BATHS
predsDFvalidation <- data.frame(DTpreds      = predict(fitDT, validation),
                                LMpreds      = predict(fitLM, validation),
                                actualPrice  = validation$PRICE,
                                URL          = validation$URL)

# RMSE DT
RMSE(predsDFvalidation$DTpreds, predsDFvalidation$actual)

# RMSE LM
RMSE(predsDFvalidation$LMpreds, predsDFvalidation$actual)

# Sometimes an "ensemble" model will improve results because the models learn differently [not here though bc the data is small]
# But the ensemble is the most stable!
RMSE(apply(predsDFvalidation[,1:2], 1, mean), predsDFvalidation$actual)

## So let's find the best opportunity among our validation set
predsDFvalidation$ensemblePreds <- apply(predsDFvalidation[,1:2], 1, mean)
predsDFvalidation$ensembleDiff  <- predsDFvalidation$ensemblePreds - predsDFvalidation$actualPrice #>0 means we predict a higher price than actual price
head(predsDFvalidation)

# Append Maximum predicted price
validation$ensembleDiff <- predsDFvalidation$ensembleDiff

# Append predictions
validation$ensemblePreds <- predsDFvalidation$ensemblePreds

# Order and ID 5 properties to look at
validation <- validation[order(validation$ensembleDiff, decreasing = T),]
head(validation[,c('URL','redfinEst','PRICE','ensemblePreds'),],5)

# End

