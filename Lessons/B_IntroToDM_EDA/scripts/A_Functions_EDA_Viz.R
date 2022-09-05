#' Author: Ted Kwartler
#' Data: 9-05-2022
#' Purpose: Load data, explore it and visualize it

## Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

## Load the libraries; 1st time use install.packages('ggplot2')
library(ggplot2)
library(ggthemes)
library(rbokeh)
library(readr)

## Bring in some data
screenTime <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/B_IntroToDM_EDA/data/on_screen_time.csv')
scenes     <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/B_IntroToDM_EDA/data/force_awakens_scenes.csv')
characters <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/B_IntroToDM_EDA/data/force_awakens_character_info.csv')

# For new R programmers, let's change from a "tibble" to a simple data frame, this isn't really needed but simplifies things for class
screenTime <- as.data.frame(screenTime)
scenes     <- as.data.frame(scenes)
characters <- as.data.frame(characters)

## Exploratory Data Analysis, and indexing
head(screenTime)

# New Column
screenTime$length <- screenTime$end - screenTime$start  # add new column by taking the difference between vectors
head(screenTime)

# Dimensions
dim(scenes) #dimensions: rows by columns

# Tally by factor
table(screenTime$character) 

# Indexing
characters$char.story[5] #5th entry in a single column

characters[7,] #returns all columns from 7th row

characters[21,3] #21st row, 3rd column

subset(characters,characters$char.name=='Admiral Ackbar')

# Random sample
set.seed(1234) #just for consistency in class
idx <- sample(1:nrow(screenTime),5) # Take 5 rows from the data set, notice the nested function
sampledData <- screenTime[idx,]

# Create a new column for each scene
scenes$length <- scenes$end - scenes$start # same as before but different data frame

# More EDA
summary(scenes$length) #base summary stats, quartile of a named vector

# Sort to find longest scenes
# Example in two lines
reorderedIndex <- order(scenes$length, decreasing=T) # get the numeric re-order
reorderedIndex # examine results
scenes <- scenes[reorderedIndex,] #remember "rows, then columns" so place as the row index left of the comma

# Example in 1 line nesting functions
scenes <- scenes[order(scenes$length, decreasing=T),] #reorder the data frame by the new length column

# Examine the 10 longest scenes, more EDA
head(scenes,10) # object and number to return, default is 6 rows

# Examine the 8 shortest scenes ONLY a vector
tail(scenes$defined.scenes,8) 

## Visuals
# Summarise character appearances
table(screenTime$character)
characterTally <- as.matrix(table(screenTime$character)) #nesting functions operate inside out

# Examine
head(characterTally)

# Reorder the rows
characterTally <- characterTally[order(characterTally, decreasing=T),] # order the data frame
head(characterTally)

# Base plots
barplot(characterTally[1:5], 
        main='Force Awakens: Character Scene Tally', 
        las = 2)

plot(characterTally, main='Force Awakens: Character Scene Tally')

# Save a basic plot to disk in the personal folder since it was set as the "working directory"
png("characterTally_plot.png")
plot(characterTally, main='Force Awakens: Character Scene Tally')
dev.off()

# BACK TO PPT FOR EXPLANATION
# ggplot2: Commented layers with ggplot to make a line plot
ggplot(screenTime, 
       aes(colour=character)) + # data frame then aesthetics
  geom_segment(aes(x    = start, 
                   xend = end, 
                   y    = character, 
                   yend = character), size=3) + #add layer of segments & declare x/y 
  theme_gdocs() + #add a default "theme"
  theme(legend.position="none") # turn off the need for a legend
ggsave("character_scenes.pdf")
ggsave("character_scenes.png")
ggsave("character_scenes.jpg")

# HTMLwidgets:rbokeh; 
# note the tidy style programming is recommended by the author but not necessary
p <- figure(legend_location = NULL) %>%
  ly_points(start, character, data = screenTime,
            color = character, hover = list(character, start, end))
p

# End
