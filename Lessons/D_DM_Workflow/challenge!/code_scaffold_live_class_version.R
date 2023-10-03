#' Ted Kwartler
#' Code Along Scaffold
#' Linear Regression: SEMMA & Vtreat 
#' Marketing Attribtion: https://www.kaggle.com/datasets/ashydv/advertising-dataset

# Set the working directory
setwd('~/Desktop/Harvard_DataMining_Business_Student/Lessons/D_DM_Workflow/challenge!')

# Load libraries
library(dplyr)
library(vtreat)
library(ggplot2)
library(ggthemes)

# Data Munge: Joins!
news  <- read.csv("advertising_Newspaper.csv")
radio <- read.csv( "advertising_Radio.csv")
tv    <- read.csv("advertising_TV_Y.csv")

# First Join
joinData <- left_join(news, radio, by = 'periodID')
joinData <- left_join(joinData, tv,  by = 'periodID')

# Sample
set.seed(1234)
idx <- sample(1:nrow(joinData), 20)
predData <- joinData[idx, ]
leftOverData <- joinData[-idx, ]

# Explore & EDA 
summary(predData)

# Modify - vtreat prep
plan <- designTreatmentsN(dframe      = predData, 
                          varlist     = c('Newspaper', 'Radio', 'TV'), 
                          outcomename = 'Sales')

trainingData <- prepare(plan, leftOverData)

# Model!
fit <- lm(Sales ~., trainingData)

# Assess
salesValues <- predict(fit, trainingData)

resultsDF <- data.frame(trainingData, salesValues)

# Outcome: Explain the impact of each channel on sales & Make a prediction


# End