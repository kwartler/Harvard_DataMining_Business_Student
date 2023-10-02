
# Libs
library(dplyr)
library(vtreat)
library(ModelMetrics)
library(reshape2)

# Data Munging
x <- read.csv("advertising_Newspaper.csv")
y <- read.csv("advertising_Radio.csv")
z <- left_join(x, y, by = 'periodID')
tv <- read.csv('advertising_TV_Y.csv')
df <- left_join(z, tv, by = 'periodID')

# SAMPLE Let's partition our data to avoid overfitting
prepDataIDX <- sample(1:nrow(df), nrow(df)*.1)
prepData    <- df[prepDataIDX,]
leftOver <- df[-prepDataIDX,]
partitioning <- sample(1:nrow(leftOver), nrow(leftOver)*.8)

# Final Data
training <- leftOver[partitioning,]
validation <- leftOver[-partitioning,]

# EXPLORE
summary(training)

# have to reshape the data to get it onto one graph
plotDF <- melt(training)

# Box plot, median is bold line, lower quartile q1 is low bound, upper to q3 is top
# whiskers are min/max and dots are outlier
ggplot(plotDF, aes(x = variable, y = value)) + 
  geom_boxplot()

# MODIFY: Not really needed bc the data is already good but let's prepare our data anyway
plan <- designTreatmentsN(prepData, 
                          varlist     = c('Newspaper', 'Radio', 'TV'),
                          outcomename = 'Sales')

# Modeling Sets
training   <- prepare(plan, training)
validation <- prepare(plan, validation)

# MODEL: Simple linear model
fit <- lm(Sales ~ ., training)
fit

# Make some predictions
inSamplePredictions <- predict(fit, training)
ooSamplePredictions <- predict(fit, validation)

# ASSESS: Quick Evaluation
rmse(actual = training$Sales,
     predicted = inSamplePredictions)
rmse(actual = validation$Sales,
     predicted = ooSamplePredictions)
