# Library
library(devtools)

# Custom Function for simple web form
source_url('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/E_Regressions/scripts/betaForms/beta_renderRegressionForm.R')


# Create a linear model
houses <-read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/E_Regressions/data/BostonHousing.csv')
houses$MEDV <- houses$MEDV*1000 #multiple by 1k to make it more sensible

fit <- lm(MEDV~., houses[,1:13])
df <- data.frame(x    = names(fit$coefficients),
                 beta = fit$coefficients,
                 type = c('intercept',rep('numeric', length(names(fit$coefficients))-1)),
                 stringsAsFactors = F, 
                 row.names = NULL)
df


# Create the form & save it
renderRegressionForm(df, 
                     fileName = '~/Desktop/Harvard_DataMining_Business_Student/Lessons/E_Regressions/scripts/betaForms/exampleSimpleForm.html')
