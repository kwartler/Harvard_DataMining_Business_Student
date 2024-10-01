#' Author: Ted Kwartler
#' Date: Sept 5 2022
#' Purpose: Fundraising PreProcessing

# Libs
library(vtreat)
library(dplyr)
library(ggplot2)
library(ggthemes)
options(scipen = 999)

# Read in the data
donors<- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/D_DM_Workflow/data/fakeDonorBureau_v2.csv')

# Examine; Here you would perform EDA
summary(donors)

# Examine names for vtreat usage
names(donors)
informativeFeatures <- names(donors)[3:19]
informativeFeatures

targetVariable      <- names(donors)[20]
targetVariable

# Examine the levels of Y1_Donation
levels(as.factor(donors$Y1_Donation))

# Declare the correct level for success for the use case
successClass        <- 'Yes'

# Select 10% for your preparation learning
set.seed(1234)
idx         <- sample(1:nrow(donors),.1*nrow(donors))
prepData    <- donors[idx,]
nonPrepData <- donors[-idx,]

# Automated variable processing
# for **categorical** outcomes 
# i.e. will the prospective donor give Y/N
# inputs: DATA FRAME, NAMES OF INFORMATIVE VARS, RESPONSE VAR, SUCCESS CLASS
plan <- designTreatmentsC(prepData, 
                          informativeFeatures,
                          targetVariable, 
                          successClass)

# Apply the plan
# If you apply prepare to the donors dataframe you trigger a warning about over fitting.  Overall not a huge deal in class but something to avoid in real application
# WARNING's dont really impact us in class but this is caused since we use the original designTreatment data and are now preparing it so the a priori rule is broken
treatedData <- prepare(plan, nonPrepData)

# Lots more appended vars; still need to drop redundant flags but much faster and robust!
summary(treatedData)

# Start over 
rm(list=ls())

# Now perform designTreatmentsN
# Read in the data
donors<- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/D_DM_Workflow/data/fakeDonorBureau_v2.csv')
set.seed(2023)
idx         <- sample(1:nrow(donors),.1*nrow(donors))
prepData    <- donors[idx,]
nonPrepData <- donors[-idx,]

# Notice that no "success" input is needed for designTreatmentsN
plan <- designTreatmentsN(prepData,
                          names(prepData)[3:19],
                          'Y2_DonatedAmt')
treatednonPrepData <- prepare(plan, nonPrepData)
head(treatednonPrepData)

# Start over 
rm(list=ls())

# Data
donors <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/D_DM_Workflow/data/fakeDonorBureau_v2.csv')

# Fictitious Data Enrichment
thirdPartyData <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/D_DM_Workflow/data/fakeDataEnrichment.csv')

# Examine
head(thirdPartyData)

# Perform a join to the existing data
# Bring new data to the 3120 donors
leftData <- left_join(donors, thirdPartyData, by = c("uniqueID"))

# 10% of the JOINED data used to prepare; prepare _after_ its all together
set.seed(2023)
idx         <- sample(1:nrow(leftData),.1*nrow(leftData))
prepData    <- leftData[idx,]
nonPrepData <- leftData[-idx,]


## A taste of whats to come...for those in the know, yes we are skipping a lot of steps.
plan <- designTreatmentsC(prepData,
                          names(leftData)[c(3:19, 22,23)],
                          'Y1_Donation',
                          'Yes')
treatedLeftData <- prepare(plan, nonPrepData)
fit             <- glm(as.factor(Y1_Donation) ~ ., treatedLeftData, family='binomial')

# Our first model!
summary(fit)

# Make some real predictions
donationProbability <- predict(fit, treatedLeftData, type='response')
head(donationProbability)

# let's examine some records
resultsDF <- data.frame(actualOutcome  = treatedLeftData$Y1_Donation,
                        modelProbabilities = donationProbability)
head(resultsDF, 10)

# visualize the separation ie the distribution of probabilities for YES and NO groups.  This shows _small_ lift bc the "YES" is to the right of the "NO" distribution
ggplot(data = resultsDF, aes(x = modelProbabilities, group = actualOutcome, fill = actualOutcome)) + 
  geom_density(adjust = 1.5, alpha= 0.4) + theme_gdocs()

# Create a cutoff
resultsDF$predictedClass <- ifelse(resultsDF$modelProbabilities >= 0.5, "Yes", "No")
head(resultsDF, 10)

# When did the actual and predicted class agree?
agreement <- ifelse(resultsDF$actualOutcome==resultsDF$predictedClass, T, F)
sum(agreement) / nrow(resultsDF)

#### of course there are better ways to look at this using a confusion matrix, Accuracy, ROC (covered later) and in business context for the top demi-decile accuracy

# An example of top decile, meaning when the model is MOST sure how accurate is it?
# get top demi-decile
deciles <- quantile(resultsDF$modelProbabilities, probs = seq(.05,.95, by = .05))
deciles
deciles[19]

topDemi <- subset(resultsDF, resultsDF$modelProbabilities >=deciles[19])
demiDecileAgreement <- ifelse(topDemi$actualOutcome==topDemi$predictedClass, T, F)
sum(demiDecileAgreement) / nrow(topDemi)

# End
