#' Author: Ted Kwartler
#' Date: 9-18-2019
#' Purpose: Fundraising PreProcessing

# Setwd
setwd("/cloud/project/Lessons/C_DataMiningProcess_DataPrep/data")

# Libs
library(vtreat)
library(dplyr)

# Read in the data
donors<- read.csv('fakeDonorBureau_v2.csv')

# Examine; Here you would perform EDA
summary(donors)

# Examine names for vtreat usage
names(donors)
informativeFeatures <- names(donors)[3:19]
targetVariable      <- names(donors)[20]
successClass        <- 'Yes'

# Automated variable processing
# for **categorical** outcomes 
# i. e.will the prospective donor give Y/N
# DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR, SUCCESS CLASS
plan <- designTreatmentsC(donors, 
                          informativeFeatures,
                          targetVariable, 
                          successClass)

# Apply the plan
treatedData <- prepare(plan, donors)

# Lots more appended vars; still need to drop redundant flags but much faster and robust!
summary(treatedData)

# Start over 
rm(list=ls())

# Data
donors <- read.csv('fakeDonorBureau_v2.csv')

# for **numeric** outcomes 
# how much will the prospective donor give?
# DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR
plan <- designTreatmentsN(donors, 
                          names(donors)[3:19],
                          'Y2_DonatedAmt')

# Apply the plan
treatedData <- prepare(plan, donors)

# Lots more appended vars; still need to drop redundant flags but much faster and robust!
summary(treatedData)

# Start over 
rm(list=ls())

# Data
donors <- read.csv('fakeDonorBureau_v2.csv')

# Fictitious Data Enrichment
thirdPartyData <- data.frame(uniqueID = donors$uniqueID,
                             likesDogs = rep(c('Yes','No'), 
                                             nrow(donors)%/%2),
                             creditScore = rnorm(nrow(donors),650))

# Append more records than in original data to make more realistic
newDat <- data.frame(uniqueID = c('Ted','Nora'),
                     likesDogs = c('Yes','Yes'),
                     creditScore = c(742, 783))

thirdPartyData <- rbind(newDat, thirdPartyData)

# Reorder to make more realistic
idx <- sample(1:nrow(thirdPartyData), nrow(thirdPartyData))
thirdPartyData <- thirdPartyData[idx, ]

# Examine
head(thirdPartyData)

# Perform a join to the existing data
# Bring new data to the 3120 donors; no "Ted" and "Nora"
leftData <- left_join(donors, thirdPartyData) 

# Bring donors to the 3122 new data points
rightData <- right_join(donors, thirdPartyData) 
rightData[c(2438:2439, 2589:2590),] #NA automatically filled in

# What's in common?
innerData <- inner_join(donors, thirdPartyData) #here identical to leftData

## A taste of whats to come...
plan <- designTreatmentsC(leftData,
                          names(leftData)[4:20],
                          'Y1_Donation',
                          'Yes')
treatedLeftData <- prepare(plan, leftData)
fit             <- glm(Y1_Donation ~ ., treatedLeftData, family='binomial')

# Our first model!
summary(fit)

# Make some real predictions
donationProbability <- predict(fit, type='response')

head(donationProbability)

# End