#' Author: Ted Kwartler
#' Date: 10-30-2019
#' Purpose: Demonstrate getting XML API information

# Libraries
library(jsonlite)
library(stringr)
library(plyr)

# WD
setwd("/cloud/project/Lessons/K_DataSources_Reporting/data")

# Youtube URL
#https://www.youtube.com/watch?v=Q-wRhzWaCac
youtubeCaption <- 'https://www.youtube.com/api/timedtext?v=Q-wRhzWaCac&asr_langs=de%2Cen%2Ces%2Cfr%2Cit%2Cja%2Cko%2Cnl%2Cpt%2Cru&caps=asr&xorp=true&hl=en&ip=0.0.0.0&ipbits=0&expire=1587455128&sparams=ip%2Cipbits%2Cexpire%2Cv%2Casr_langs%2Ccaps%2Cxorp&signature=AE5BA8DDD26AE023739D2D70861D8CD4D801057A.210AD5CA5F12A23888F9422A02D4107961EB37FE&key=yt8&kind=asr&lang=en&fmt=json3&xorb=2&xobt=3&xovt=3'

# Go get the data
dat <- fromJSON(youtubeCaption)

# closed captioning data
dat$events$tStartMs
dat$events$dDurationMs
dat$events$segs

# Get each first column called utf8
rawTxt <- lapply(dat$events$segs, "[", 'utf8') 

# organize just the single column
rawTxt <- do.call(rbind, rawTxt)

# Drop line returns "\n"
rawTxt <- gsub('[\r\n]','',rawTxt[,1])

# Now there are entries that are empty so they need to be dropped
head(rawTxt,10)
rawTxt <- rawTxt[nchar(rawTxt) != "0"]
head(rawTxt,10)

# Get rid of extra spacing on certain words
rawTxt <- str_squish(rawTxt)
head(rawTxt,10)
rawTxt <- paste(rawTxt, collapse = ' ')

# Save as a text file
#writeLines(rawTxt, 'sometext.txt')

# If you want to retain the meta data
timedTxt <- lapply(dat$events$segs, "[", 'utf8')

allTxt <- list()
for (i in 1:length(timedTxt)){
  x <- paste(timedTxt[[i]][,1], collapse ='')
  allTxt[[i]] <- x
}

# Organize
textDF <- data.frame(startTime = dat$events$tStartMs/1000,
                     duration  = dat$events$dDurationMs/1000,
                     text = unlist(allTxt))

# Drop line breaks if it causes no text rows; otherwise use text<-str_replace_all(text, "[\n]" , "") to drop while preserving text on line
textDF<-  textDF[!grepl('[\n]', textDF$text),]

# Examine to make sure format is ok
head(textDF, 10)

# Save
#write.csv(textDF, 'timedText2.csv', row.names = F)
# End
