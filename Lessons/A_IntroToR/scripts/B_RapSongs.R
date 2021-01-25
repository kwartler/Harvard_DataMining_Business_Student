#' Title: Rap Songs
#' Purpose: Rate of speech for hip/hop; Build a plot of the rate of change for lyrics
#' Author: Ted Kwartler
#' License: GPL>=3
#' Date: Dec 28 2020
#'

# Set wd
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/A_IntroToR/data/z_rap_songs")


# Options
options(stringsAsFactors = F, scipen = 999)

# libs
library(stringr)
library(ggplot2)
library(ggthemes)
library(pbapply)

# Multiple files as a list
tmp <- list.files(pattern = '*.csv')
allSongs <- pblapply(tmp, read.csv)
names(allSongs) <- gsub('csv','', tmp)

# Basic Exploration
allSongs$BEST.ON.EARTH..Bonus...Explicit..by.Russ.feat..BIA.

## Length of each song
songLength <- sapply(allSongs, function(x){ max(x[,1])}) 
songLength <- round((songLength /1000)/60, 2)

## Avg words in song
singleWords <- list()
for(i in 1:length(allSongs)){
  print(names(allSongs)[i])
  x <- strsplit(as.character(allSongs[[i]][,3])," ")
  singleWords[[i]] <- data.frame(song = names(allSongs)[i],
                                 totalWords  = length(unlist(x)))
}
singleWords <- do.call(rbind, singleWords)
head(singleWords)

# Find the specific locations of terms
sapply(allSongs, function(x) grep('trippin', x[,3], ignore.case = T))
sapply(allSongs, function(x) grep('money', x[,3], ignore.case = T))
allSongs$Lucky.You..feat..Joyner.Lucas...Explicit..by.Eminem.feat..Joyner.Lucas.[44,3]

# Find the presence of terms
sapply(allSongs, function(x) grepl('money', x[,3], ignore.case = T))

# Or find them at the song level
searchTerm <- 'money'
termExist <- list()
for(i in 1:length(allSongs)){
  x <- paste(allSongs[[i]][,3], collapse = ' ')
  x <- grepl(searchTerm, x, ignore.case = T)
  termDF <- data.frame(song  = names(allSongs[i]),
                       exist = x)
  names(termDF)[2] <- paste0(searchTerm, '_exists')
  termExist[[i]] <- termDF
}
termExist <- do.call(rbind, termExist)

## stricount words
countWords <- function(docDF, termVector){
  response <- list()
  for(i in 1:length(termVector)){
    x <- tolower(docDF[,3])
    x <- sum(str_count(x, termVector[i]))
    response[[i]] <- x 
  }
  
  response <- do.call(cbind, response)
  colnames(response) <- termVector
  return(response)
}

# Apply to one song as example
countWords(allSongs[[1]],c('trippin', 'money'))

# Apply to list
wordCheck <- lapply(allSongs, countWords, c('trippin', 'money'))
wordCheck <- data.frame(song = names(wordCheck),
                        do.call(rbind, wordCheck))
wordCheck

# Calculate the cumulative sum
wordCountList <- list()
for(i in 1:length(allSongs)){
  x <- allSongs[[i]]
  wordCount <- str_count(x$text, "\\S+") #count the space character
  y <- data.frame(x$endTime, 
                  cumulativeWords = cumsum(wordCount),
                  song = names(allSongs[i]),
                  lyric = x$text)
  names(y)[1] <- 'endTime'
  wordCountList[[i]] <- y
}

# Get the timeline of a song
songTimeline  <- do.call(rbind, wordCountList)
head(songTimeline)

# Get the last values for each song (total words but now with time)
totalWords <- lapply(wordCountList, tail,1)
totalWords <- do.call(rbind, totalWords)

# Make a plot of the speech cadence
ggplot(songTimeline,  aes(x     = endTime,
                          y     = cumulativeWords, 
                          group = song, 
                          color = song)) +
  geom_line(alpha = 0.25) +
  geom_point(data =totalWords, aes(x     = endTime,
                                   y     = cumulativeWords, 
                                   group = song, 
                                   color = song), size = 2) +
  geom_text(data  = totalWords, aes(label=song),
            hjust = "inward", vjust = "inward", size = 3) + 
  theme_tufte() + theme(legend.position = "none")

# Two clusters, let's see Em vs all
songTimeline$eminem <- grepl('eminem', 
                             songTimeline$song, 
                             ignore.case = T)
totalWords$eminem <- grepl('eminem', 
                           totalWords$song, 
                           ignore.case = T)
ggplot(songTimeline,  aes(x     = endTime,
                          y     = cumulativeWords, 
                          group = song, 
                          color = eminem)) +
  geom_line(alpha = 0.25) +
  geom_point(data =totalWords, aes(x     = endTime,
                                   y     = cumulativeWords, 
                                   group = song, 
                                   color = eminem), size = 2) +
  geom_text(data  = totalWords, aes(label=song),
            hjust = "inward", vjust = "inward", size = 3) + 
  theme_few() + theme(legend.position = "none")


# Fit a linear model to each song and extract the x-coefficient
# Poached: https://stackoverflow.com/questions/40284801/how-to-calculate-the-slopes-of-different-linear-regression-lines-on-multiple-plo
library(tidyr)
library(purrr)
library(dplyr)
doModel  <- function(dat) {lm(cumulativeWords ~ endTime + 0, dat)}
getSlope <- function(mod) {coef(mod)[2]}
models <- songTimeline %>% 
  group_by(song) %>%
  nest %>% #tidyr::Nest Repeated Values In A List-Variable.
  mutate(model = map(data, doModel)) %>% 
  mutate(slope = map(model, coefficients)) 

# Avg words per second by song
wordsSecs <- data.frame(song = names(allSongs),
                        wordsPerSecond= (unlist(models$slope) * 1000)) #adj for milliseconds
wordsSecs[order(wordsSecs$wordsPerSecond, decreasing = T),]

# End