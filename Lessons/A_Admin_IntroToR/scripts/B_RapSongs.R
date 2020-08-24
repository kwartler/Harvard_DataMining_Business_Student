#' Title: Rap Songs
#' Purpose: Rate of speech for hip/hop; Build a plot of the rate of change for lyrics
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Aug 23-2020
#'

# Options
options(stringsAsFactors = F, scipen = 999)

# libs
library(stringr)
library(ggplot2)
library(ggthemes)
library(pbapply)

# Multiple files as a list
tmp <- list.files(path = "/cloud/project/Lessons/A_Admin_IntroToR/data/z_rap_songs",
                  pattern = '*.csv')
allSongs        <- pblapply(tmp, read.csv)
names(allSongs) <- gsub('csv','', tmp)

# Basic Exploration
allSongs$BEST.ON.EARTH..Bonus...Explicit..by.Russ.feat..BIA.

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
  theme_hc() + theme(legend.position = "none")
# End