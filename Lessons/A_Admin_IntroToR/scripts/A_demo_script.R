#' Title: A_demo_script.R
#' Purpose: Get some R time!
#' Author: TK
#' Date: Aug 23-2020

# Step 1 Set the working directory: where's the fruit?
setwd("~/Documents/Harvard_DataMining_Business_Student/Lessons/A_Admin_IntroToR/data")

# Step 2 Load some libraries: Make R a customized piece of software
options(scipen = 999)
library(ggplot2)
library(ggthemes)

# Step 3 Bring in some data: Go get our banana!
scenesDF   <- read.csv('forceAwakens_definedScenes.csv')

# Step 4 Apply functions: Perform the task we want on our data: Cut & Peel our banana!

# Use the names function to review the names of scenesDF
names(scenesDF)

# Review the bottom 6 records of scenesDF
tail(scenesDF)

# Clean up the raw data with a "global substitution"
scenesDF$id <- gsub('/xray/scene/', "", scenesDF$id)
tail(scenesDF)

# Change ID class from string to numeric
scenesDF$id <- as.numeric(scenesDF$id)

# Remove a column
scenesDF$fictionalLocation <- NULL

# Make a new column & review
scenesDF$length <- scenesDF$end - scenesDF$start 
head(scenesDF$length)

# Basic statistics
summary(scenesDF) 

# Step 5: Consume our results: Plot/Eat the banana!
ggplot(scenesDF, aes(colour=scenesDF$name)) + 
  geom_segment(aes(x=scenesDF$start, xend=scenesDF$end,
                   y=scenesDF$id, yend=scenesDF$id),size=3) +
  geom_text(data=scenesDF, aes(x=scenesDF$end, y=scenesDF$id,  label = scenesDF$name), size = 2.25,color = 'black', alpha = 0.5, check_overlap = TRUE) + 
  theme_gdocs() + theme(legend.position="none")
# End