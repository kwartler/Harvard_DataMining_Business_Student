#' Author: Ted Kwartler
#' Date: 9-11-2020
#' Purpose: Cereal EDA
#' 

# libs
library(radiant.data)
library(DataExplorer)

# Set WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/B_IntroToDM_EDA/data")

# Data
cereal <- ----.---('Cereals.csv')

# What's the overall structure  & dimensions of the data?
str(   )
dim(   )

# Data set class
class(    )

# Classes for each column
sapply(     , class)

# Look at the top 6 rows
head(   )

# How many differnt brand names?
nlevels( as.factor(      ))
# Or
length(unique(     ))

# What are the column names?
names(     )

# Summary stats for each vector
summary(      )

# What's the relationship between protein and sugar?
cor(   ,   , use = 'complete.obs')

# Who are the unique manufacturers?
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
idx <- sample(1:nrow(_____   ),   1)
     [    , ]

# DataExplorer
plot_str(cereal)
plot_missing(cereal)
plot_histogram(cereal$protein) 
plot_density(cereal$calories) 
plot_histogram(cereal)#time consuming 
plot_density(cereal)#time consuming 
plot_scatterplot(cereal, by='rating') #time consuming 

# radiant.data
# example video: https://radiant-rstats.github.io/radiant.data/
radiant.data::radiant.data()

# End