#' Prof K
#' 2-27-2019
#' 7.2

# Libs
library(caret)
library(vtreat)
library(class)

# WD
setwd('____')

# Data
df <- read.csv("UniversalBank.csv")

# EDA
table(df$Personal.Loan)

# Drop per directions
df$ID       <- ____  
df$ZIP.Code <- NULL

# In this example EDU is actually a factor!
df$Education <- as.factor(df$Education)
nlevels(df$Education)

# Partition per directions
set.seed(1234)
splitPercent <- round(nrow(df) %*% __)
totalRecords <- 1:nrow(___)
idx <- sample(_______, splitPercent)

trainDat <- df[idx,]
testDat <- df[-___,]

# Treatment to account for dummies, although its a pretty clean data set, EDU could be a dummy but is still ordinal...so?
xVars <- c("Age", "Experience", "Income", "Family", "CCAvg",
           "Education", "Mortgage", "Securities.Account",
           "CD.Account", "Online", "CreditCard" )
yVar <- '____'
plan <- designTreatmentsC(trainDat, ____,____,_)

# Prepare
treatedTrain <- prepare(___, ____)

# Fit - WARNING!!!  
knnFit <- train(______ ~ ., 
                data = _____, method = "___", 
                preProcess = c("center","scale"), tuneLength = 10)

# Make sure you know your data problem...its classification!  NOte the difference with the previous call.
knnFit <- train(as.factor(____) ~ ., 
                data = treatedTrain, method = "___", 
                preProcess = c("center","scale"), tuneLength = 10)

# 7.2A
newCustomer <- data.frame(Age                = __,
                          Experience         = __,
                          Income             = __,
                          Family             = _,
                          CCAvg              = _, 
                          Education          = _,
                          Mortgage           = _, 
                          Securities.Account = _,
                          CD.Account         = _, 
                          Online             = _,
                          CreditCard         = _)

newCustomer$Education <- as.factor(___________)
treatedNewCU          <- prepare(plan, _______)

# this is the version with a higher K
predict(____, _____)

# Since 7.2a demands k=1, we make a single model bc caret's implementation starts at 5.
allData    <- full_join(df, newCustomer)
treatedAll <- prepare(plan, allData)
scaleAll   <- scale(treatedAll[,1:__], scale = T, center=T)

specialK   <- knn(train = scaleAll[1:5000,1:__],
                  test  = scaleAll[____,1:__],
                  cl = as.factor(df$Personal.Loan), k =____)
specialK

# Did the person accept the personal loan offer?
# Answer: 

# 7.2B
______
plot(_____)
# The most balanced K is:
# Answer: 

# 7.2C
# Prep the validation set
treatedTest <- prepare(plan, _____)
testClasses <- predict(knnFit, _______)
confusionMatrix(as.factor(testDat$Personal.Loan),________)

# 7.2D
# Make another new customer data frame.  Prepare it.  Then use the knnFit to make a prediction.
# Did the person accept the personal loan offer?
# Answer: 


# 7.3E 
# Now redo your partitions into 3 parts.  Go back to our script examples for this code.  Make predictions, construct the confusion matrices and review to answer the question.

# End