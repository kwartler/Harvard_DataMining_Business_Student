#' Title: Rap Songs
#' Purpose: Rate of speech for hip/hop; Build a plot of the rate of change for lyrics
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Sep-2-2020
#'

# Options - this is to turn off scientific notation
options(stringsAsFactors = F, scipen = 999)

# libs - let's load out libraries!
library(stringr)
library(ggplot2)
library(ggthemes)
library(pbapply)

# Multiple files as a list - instead of a single file, we create a list, think of this like a workbook in Excel with multiple worksheets.  In R these are "data frames"
tmp <- list.files(path = "~/Documents/Harvard_DataMining_Business_Student/Lessons/A_IntroToR/data/z_rap_songs_revised/",
                  pattern = '*.csv', full.names = T)
allSongs        <- pblapply(tmp, read.csv)
names(allSongs) <- gsub('csv','', tmp)

# Basic Exploration - The revised version removes all lyrics but instead has a simple word count for each song and the cumulative sum of words spoken by millisecond. 
head(allSongs[[1]])


# Get the last values for each song (total words but now with time) - within the list, the "tail" function grabs the last row of each song.  Think of this as grabbing the last single row of each worksheet and capturing that informaiton in a separate worksheet.
totalWords <- lapply(allSongs, tail,1)
totalWords <- do.call(rbind, totalWords)
rownames(totalWords) <- NULL
head(totalWords)

# Get the timeline of a song - This function combines ("row-bind") all the data frames in the list to one object.  Notice how "allsongs" in your environment has 30 elements (dataframes) while now "songTimeline" has 2476 rows and 3 variables.  Think of this like a copy/paste of each workbook underneath to form one long running single work sheet. 
songTimeline  <- do.call(rbind, allSongs)

# Make a plot of the speech cadence - Now we build a plot, ggplot is complext but essentially we are building the visualization layer by layer and have complete control over each layer.  First a blank plot layer, then the lines are added, then the end point is added, then the text and finally some aethetics like a theme and removing the legend.  Dont worry about this section as we will cover more visuals in detail and entire courses are spent on visualization.
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
