#' Author: Ted Kwartler
#' Date: 9-20-21
#' Purpose: Fundraising PreProcessing

# Setwd
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/C_ModelingProcess_DataPrep/data")

# Libs
library(vtreat)
library(dplyr)
options(scipen = 999)

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
# inputs: DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR, SUCCESS CLASS
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
thirdPartyData <- read.csv( 'fakeDataEnrichment.csv')

# Examine
head(thirdPartyData)

# Perform a join to the existing data
# Bring new data to the 3120 donors
leftData <- left_join(donors, thirdPartyData, by = c("uniqueID"))

# Bring donors to the new data points
rightData <- right_join(donors, thirdPartyData, by = c("uniqueID"))
rightData[c(3119:3122),] #NA automatically filled in

# Find records in common
innerData <- inner_join(donors, thirdPartyData) #here identical to leftData

## A taste of whats to come...for those in the know, yes we are skipping a lot of steps.
plan <- designTreatmentsC(leftData,
                          names(leftData)[4:20],
                          'Y1_Donation',
                          'Yes')
treatedLeftData <- prepare(plan, leftData)
fit             <- glm(as.factor(Y1_Donation) ~ ., treatedLeftData, family='binomial')

# Our first model!
summary(fit)

# Make some real predictions
donationProbability <- predict(fit, treatedLeftData, type='response')

head(donationProbability)

# End
