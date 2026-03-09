#' Ted Kwartler
#' Oct 23, 2023
#' Purpose: Build predictive models that can predict which booking is going to be canceled & choose the most accurate one
#' Source: https://www.kaggle.com/datasets/mariyamalshatta/inn-hotels-group

# library & options - ggplot, ggthemes, rpart, randomForest, caret (if you want to try other models), MLmetrics, vtreat
library(_)
library(_)
library(_)
library(_)
library(_)
library(_)
library(_)
options(scipen = 999)

# SAMPLE
hotelData <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/F_Tree_RF/challenge!/INNHotelsGroup.csv')

# Let's take 10% for vtreat & split 80/20 for the remaining
variableTreatmentIdx <- sample(1:nrow(hotelData), nrow(hotelData)*.1)
vtreatRows           <- hotelData[variableTreatmentIdx,]
remainingData        <- _________[-___________________,]
trainingIDX          <- sample(1:nrow(remainingData), nrow(remainingData)*.8)
training             <- remainingData[trainingIDX,]
validation           <- _____________[-__________,]

# EXPLORE - Obviously do more here
head(_________)
ggplot(________, aes(x=no_of_adults, color = type_of_meal_plan)) + 
  geom_density() + 
  facet_wrap(vars(type_of_meal_plan)) + 
  theme_gdocs()

# Try to adjust this to other functions and formulas
aggregate(avg_price_per_room ~ booking_status,training, mean)

# MODIFY - you could also see about engineering variables - make your design treatment plan
plan <- _________________(dframe      = _______,
                          varlist     = names(training)[2:18], # the "x" variables
                          outcomename = "_______________",
                          outcometarget = "Not_Canceled")

# Prepare data and change Y to 0/1 so it works with logistic regression
treatedTrain      <- _______(____, __________)
treatedValitation <- prepare(plan, _________)
treatedTrain$booking_status       <- ifelse(treatedTrain$_____________=='Not_Canceled',1,0)
treatedValitation$booking_status  <- ifelse(____________$booking_status=='Not_Canceled',1,0)

# To keep things simple we're removing the automatic pre engineered vars
treatedTrain      <- treatedTrain[,!grepl('_cat', names(treatedTrain)) ]
treatedValitation <- treatedValitation[,!grepl('_cat', names(treatedValitation)) ]

# MODEL - logistic regression, rpart and rf
logFit   <- ___(______________~., __________, family = '________')
rpartFit <- _____(as.factor(booking_status)~., 
                  _____________,
                  control = rpart.control(cp = 0.05)) # you can try different cp values
rfFit    <- randomForest(as.factor(___________)~., 
                         treatedTrain, _____=250) # you can try different ntree values

# ASSESS - get predictions for all datasections and all models
logTrainingPreds     <- _______(logFit,treatedTrain, type = 'response')
logValidationPreds   <- _______(logFit,treatedValitation, type = 'response')
rpartTrainingPreds   <- _______(rpartFit,____________)
rpartValidationPreds <- _______(rpartFit,_________________)
rfTrainingPreds      <- _______(rfFit,____________, type = 'prob')
rfValidationPreds    <- _______(rfFit, _________________, type = 'prob')

# Organize training
trainingPredDF <- data.frame(actual             = treatedTrain$booking_status,
                             logTrainingPreds   = logTrainingPreds,
                             rpartTrainingPreds = rpartTrainingPreds,
                             rfTrainingPreds    = rfTrainingPreds)
head(trainingPredDF) # review the outputs next to actual

# Logistic Confusion Matrix
logisticCutoff <- ifelse(trainingPredDF$logTrainingPreds>=0.5,1,0) #play with these 0.5 cutoffs to see how it changes the table outcomes and accuracy
table(trainingPredDF$actual, logisticCutoff)

# Logistic Accuracy
Accuracy(y_pred = logisticCutoff, y_true = trainingPredDF$actual)

# RPart Conf Matrix
rpartCutoff <- ifelse(__________________$rpartTrainingPreds.1>=0.5,1,0) #play with these 0.5 cutoffs to see how it changes the table outcomes and accuracy
table(trainingPredDF$actual, _____________)

# Rpart Accuracy
Accuracy(y_pred = ___________, y_true = trainingPredDF$_______)

# RF Conf Matrix
rfCutoff <- ifelse(________________$rfTrainingPreds.1>=0.5,1,0) #play with these 0.5 cutoffs to see how it changes the table outcomes and accuracy
table(trainingPredDF$actual, _________)

# RF Accuracy
Accuracy(y_pred = ________, y_true = trainingPredDF$_______)


### Now redo using the validation set and compare.  Look for consistency between training and validation AND select the most accurate.


################# Not really part of the lesson!! ###################
### For those that are interested here is a quick way to get a web form of the logistic model
# Get custom function
devtools::source_url('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/F_Tree_RF/A_logisticRegression_catchUP/ZZ_betaForms/beta_renderLogisticForm_with_aesthetics.R')

# Get the coefficients and betas
# Organize the df object
varType <- ifelse(grepl('lev_x', names(coefficients(logFit))), 'dummy',
                  ifelse(grepl('\\(Intercept\\)', names(coefficients(logFit))), 'intercept', 'numeric'))
df <- data.frame(x    = names(logFit$coefficients),
                 beta = logFit$coefficients,
                 type = varType,
                 stringsAsFactors = F, 
                 row.names = NULL)
df

# Create the form & save it; be sure to change the path to your personal file folder
renderLogisticForm(df, 
                   backgroundColor       = 'grey', 
                   fontColor             = 'white', 
                   submitButtonColor     = 'blue', 
                   submitButtonFontColor = 'orange', 
                   font                  = 'arial', 
                   fileName = '~/Desktop/Harvard_DataMining_Business_Student/personalFiles/challengeLogistic.html')

# You can test this form by having 4 adults, in the year 2020 with 2 special requests.  It should give you a non 0/1 probability.


# End