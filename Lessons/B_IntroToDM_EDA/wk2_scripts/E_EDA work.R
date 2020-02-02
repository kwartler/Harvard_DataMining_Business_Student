#' Author: Ted Kwartler
#' Date: 2-6-2019
#' Purpose: Cereal EDA
#' 

# libs
library(radiant.data)
library(DataExplorer)

# Set WD
setwd("/cloud/project/Lessons/B_DataExploration_EDA/wk2_Data")

# Data
cereal <- read.csv('Cereals.csv')

# What's the overall structure  & dimensions of the data?
str(     )
dim(     )

# Data set class
class(     )

# Classes for each column
sapply(cereal, class)

# Look at the top 6 rows
head(     )

# How many differnt brand names?
nlevels(     )

# What are the column names?
names(     )

# Summary stats for each vector
summary(      )

# What's the relationship between protein and sugar?
cor(cereal$    , cereal$   )

# How many unique manufacturers?
unique(     )

# Avg calories?
mean(     )

# Number missing values?
colSums(is.na(     ))


# Sampling 5 row example (nonsense w/data this size but good to know how):
set.seed(123)
idx <- sample(1:nrow(cereal),  5 )
cereal[idx, ]

# Sample 10 rows
# What is the first name with seed 1234
set.seed(    )
idx <- sample(1:nrow(     ),   )
cereal[    , ]

# DataExplorer
plot_str(cereal)
plot_missing(cereal)
#plot_histogram(cereal) #time consuming w/o more RAM
#plot_density(cereal) #time consuming w/o more RAM
#plot_scatterplot(cereal, by='rating') #time consuming w/o more RAM

# radiant.data
radiant.data::radiant.data()


# End