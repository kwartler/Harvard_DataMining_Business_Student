#' Author: Ted Kwartler
#' Data: 9-11-2019
#' Purpose: Load data, explore it and visualize it

## Set the working directory
setwd("/cloud/project/Lessons/B_DataExploration_EDA/wk2_Data")

## Load the libraries; 1st time use install.packages('ggplot2')
library(ggplot2)
library(ggthemes)
library(rbokeh)

## Bring in some data
screenTime <- read.csv('on_screen_time.csv')
scenes     <- read.csv('force_awakens_scenes.csv')
characters <- read.csv('force_awakens_character_info.csv',stringsAsFactors = F)

## Exploratory Data Analysis, and indexing
head(screenTime)

# New Column
screenTime$duration <- screenTime$end - screenTime$start
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
set.seed(1234) #just for consistency
num <- sample(1:nrow(screenTime),5) # Take 5 rows from the data set, notice the nested function
screenTime[num,]

# Oversample Example
s <- sample(1:nrow(screenTime), 25, 
            prob = ifelse(screenTime$duration > mean(screenTime$duration), 0.9, 0.01))

# scenes
scenes$length <- scenes$end - scenes$start # add new column by taking the difference between vectors

summary(scenes$length) #base summary stats, quartile

# Sort to find longest scenes
scenes <- scenes[order(scenes$length, decreasing=T),] #reorder the data frame by the new length column

# Examine the 10 longest scenes
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
order(characterTally, decreasing=T)
characterTally <- characterTally[order(characterTally, decreasing=T),] # order the data frame
head(characterTally)

# Base plots
barplot(characterTally[1:5], 
        main='Force Awakens: Character Scene Tally', 
        las = 2)

plot(characterTally, main='Force Awakens: Character Scene Tally')

# BACK TO PPT FOR EXPLANATION
# ggplot2: Commented layers with ggplot to make a line plot
ggplot(screenTime, 
       aes(colour=screenTime$character)) + # data frame then aesthetics
  geom_segment(aes(x=screenTime$start, 
                   xend=screenTime$end, 
                   y=screenTime$character, 
                   yend=screenTime$character), 
               size=3) + #add a layer of segments and devlare x/y 
  theme_gdocs() + #add a default "theme"
  theme(legend.position="none") # turn off the need for a legend


# ggplot2: Nonsense result but concise ggplot w/geom_point
p <- ggplot(scenes, aes(colour=scenes$defined.scenes)) +
  geom_point(aes(x=scenes$start, y=scenes$end), size=3) + 
  theme_gdocs() + 
  theme(legend.position="none") 

p


# HTMLwidgets:rbokeh; note the tidy style programming is recommended by the author but not necessary
p <- figure(legend_location = NULL) %>%
  ly_points(start, character, data = screenTime,
            color = character, hover = list(character, start, end))
p



# End
