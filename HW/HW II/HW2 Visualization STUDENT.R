# HW 2 ggplot viz
# Sources https://www.kaggle.com/datasets/julienjta/twitter-mentions-volumes
# https://www.kaggle.com/datasets/jpmiller/employee-attrition-for-healthcare

# Directions: Please follow the commented instructions to create various visualizations.  Then turn in your R script

# Libraries
library(readr)
library(ggplot2)
library(ggthemes)
library(tidyr)

# Obtain the data from the repo
healthcare <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/HW/HW%20II/watson_healthcare_modified.csv')
twitterVol <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/HW/HW%20II/twitter%20volume.csv')

# Get to know your data by calling summary and head on the data sets
summary(_______)
head(__________)
summary(_______)
head(__________)

#1.
# Make a ggplot box plot with healthcare
# Use aes x = HourlyRate and fill = Attrition with a layer 
# add the layer geom_boxplot() 
ggplot(_______, aes(x = ________, fill = _______)) + geom_boxplot() 
# Is there a difference between hourly wage for people that leave versus stay?  You may have to lookup how to interpret a boxplot

#2.
# Build a ggplot density plot with healthcare
# Use aes x = TotalWorkingYears and fill = 'red'
# Add a layer geom_density 
# Add a layer with  theme_gdocs()
ggplot(_________, aes(x = __________, fill = ___)) + _____________ + ______________
# What do you observe?  What is the average tenure?

# Now call median() on the TotalWorkingYears column of the data 
median(______$________)
# Are they similar to the what the visual is telling you?


#3.
# Rebuild the density plot but add fill = Attrition  in the aes() call
# add a geom_density()  layer with alpha = 0.5 inside
# Add a layer with  theme_gdocs()
# Add a title with ggtitle('Work Yrs distribution by Attrition')
ggplot(______________, aes(_____________)) + geom_density(___________) + ____________() + ggtitle(_________)
# What do you observe inthe visual, is there a difference in tenure and total working years?
# Can you calculate the median by the group?  There are multiple ways to do this, one can subset, or use group_by from dplyr.

#4.
# Create a data frame called df using data.frame(table(dataSet$columnName)) referring to the MaritalStatus
df <- _______________________________

# Create a geom_col bar chart with the df object.  If done properly the aes should be x = Var1 and y = Freq
# Add theme_hc()
ggplot(df, aes(_________________)) + __________() + _________()
# What is the most common marital status?

#5.
# Rebuild df as a data.frame, with a two-way tally with  MaritalStatus & Attrition like this: table(healthcare$MaritalStatus, healthcare$Attrition)
df <- data.frame(__________________________________)
# Examine the df object by calling it in your console and notice the difference between the original 1-way tally

# Build a stacked ggplot bar chart using the new df data
# aes should be x = Var1, y = Freq, fill = Var2
# add a layer geom_bar with position = 'stack' and stat = 'identity'
# add a layer theme_hc() 
ggplot(__, aes(___________________) + geom_bar(_________________) + ____________()
# What do you observe with attriting employees and the relationship to martical status? 

#6.
# Create an object twitter100 by applying head() with the additional parameter 100 to get the first 100 rows, or you can use indexing
twitter100 <- head(twitterVol,___)
# Use the cor() function on two columns, $Apple, and $Salesforce noting the correlation between twitter volume within this data
cor(__________, _______________)
# Using twitter100, create a ggplot scatter plot 
# aes should be x = Apple, y = Salesforce, and alpha = 0.5
# the geom_point layer
# make it theme_few
# add a smoothing line with geom_smooth with parameter method = 'lm' to help the audience understand the relationship
ggplot(_____________, __________________) + ____________ + ___________ + ____________(method="lm")
# Describe the relationship either positive or negative of the tweet volumes for these two companies?


#7.
# Create a ggplot timeline of the apple twitter volume
# aes should be x = timestamp and  y = Apple
# Add a layer of geom_smooth()
# Add a smoothing line with method = 'auto' within geom_smooth()
# Add a theme theme_fivethirtyeight layer
# add a title with ggtitle and 'apple twitter volume'
ggplot(___________, ___________) + ___________() + ___________(method = _________) + _______________() + ____________('apple twitter volume')

#8.
# First look at the twitter100 data with head()
_______(__________)
# Next execute the code scaffold to pivot the data longer 
longTwitterVolume <- twitter100 %>%
  pivot_longer(!timestamp, names_to = "company", values_to = "twitterVolume")
# Review the change with head(); notice the data is the same just rearranged
_______(____________)
# subset the data using the code scaffold below to just Facebook and Google
twoCompanies <- subset(longTwitterVolume, longTwitterVolume$company == 'Facebook' | longTwitterVolume$_______ == __________)
# Plot the two lines with ggplot and the twoCompanies data
# aes should be x = timestamp, y = twitterVolume, group = company and color = company
# add a geom_line()
# make the theme gdocs 
ggplot(__________, aes(_______________) + _________() + ___________()


# End
