# Library
library(devtools)
library(vtreat)

# Get custom function
source_url('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/E_Regressions/scripts/betaForms/beta_renderLogisticForm_with_aesthetics.R')

# Create a logistic regression model
bank <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/E_Regressions/data/bank-downSampled.csv')

# Just select a few columns for this example & change to 0/1
keeps          <- c('age', 'education', 'month', 'duration', 'Class')
training       <- bank[,names(bank) %in% keeps]
training$Class <- ifelse(training$Class=='yes',1,0)

plan <- designTreatmentsC(training, 
                          names(training)[1:4],
                          names(training)[5],
                          1)
treatedTrain <- prepare(plan, training)

# Drop the engineered vars bc it's out of scope at the moment
treatedTrain <- treatedTrain[,-grep('_cat', names(treatedTrain))]

# glm
fit <- glm(Class~., treatedTrain, family = 'binomial')
fit <- step(fit, direction = 'backward') # drop some multi-colinear & nonsignificant vars

# Organize the df object
varType <- ifelse(grepl('lev_x', names(coefficients(fit))), 'dummy',
                  ifelse(grepl('\\(Intercept\\)', names(coefficients(fit))), 'intercept', 'numeric'))
df <- data.frame(x    = names(fit$coefficients),
                 beta = fit$coefficients,
                 type = varType,
                 stringsAsFactors = F, 
                 row.names = NULL)
df

# Create the form & save it
renderLogisticForm(df, 
                   backgroundColor       = 'grey', 
                   fontColor             = 'white', 
                   submitButtonColor     = 'blue', 
                   submitButtonFontColor = 'orange', 
                   font                  = 'arial', 
                   fileName = '~/Desktop/Harvard_DataMining_Business_Student/Lessons/E_Regressions/scripts/betaForms/exampleLogistic.html')
