#' Author: Ted Kwartler
#' Date: 11-30-2020
#' Purpose: Demonstrate getting XML API information

# Libraries
library(jsonlite)
library(stringr)
library(plyr)

# Options; google api returns UTF-8 text
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# WD
setwd("/cloud/project/Lessons/L_DataSources_Reporting/data")

# Youtube URL
#https://www.youtube.com/watch?v=Q-wRhzWaCac
youtubeCaption <- 'https://www.youtube.com/api/timedtext?v=Q-wRhzWaCac&asr_langs=de%2Cen%2Ces%2Cfr%2Cit%2Cja%2Cko%2Cnl%2Cpt%2Cru&caps=asr&xorp=true&xoaf=5&hl=en&ip=0.0.0.0&ipbits=0&expire=1606807242&sparams=ip%2Cipbits%2Cexpire%2Cv%2Casr_langs%2Ccaps%2Cxorp%2Cxoaf&signature=C2899F3F69DDF222DE8FDBEF6B4F65822D893A2C.ACAEE548314AA2357A2590F8AB2C4AE6BBCF0F0C&key=yt8&kind=asr&lang=en&fmt=json3&xorb=2&xobt=3&xovt=3'

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

# Drop line breaks embedded within text and standalone rows
text<-str_replace_all(allTxt, "[\n]" , "")

# Organize
textDF <- data.frame(startTime = dat$events$tStartMs/1000,
                     duration  = dat$events$dDurationMs/1000,
                     text = text)

# 
textDF<-  textDF[nchar(as.character(textDF$text)) !=0,]

# Examine to make sure format is ok
head(textDF, 10)

# Save
#write.csv(textDF, 'timedText2.csv', row.names = F)
# End
