#' Title: Grab Youtube JSON
#' Purpose: Demonstrate f12 in Chrome for API
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Oct 20, 2021
#'

# Libraries
library(jsonlite)
library(stringr)
library(plyr)

# Options; google api returns UTF-8 text
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# WD
setwd("~/Desktop/LUX_NLP_student/lessons/oct21/data")

# Youtube URL
#https://www.youtube.com/watch?v=Q-wRhzWaCac
youtubeCaption <- 'https://www.youtube.com/api/timedtext?v=5F-o2_AC-Wo&asr_langs=de%2Cen%2Ces%2Cfr%2Cid%2Cit%2Cja%2Cko%2Cnl%2Cpt%2Cru%2Ctr%2Cvi&caps=asr&exp=xftt%2Cxctw&xoaf=5&hl=en&ip=0.0.0.0&ipbits=0&expire=1634845852&sparams=ip%2Cipbits%2Cexpire%2Cv%2Casr_langs%2Ccaps%2Cexp%2Cxoaf&signature=0CC45CE748F42330F037FB99E63A9F3FCD6E227C.D85D8EDE42E6581A8A9AE4449E49B8487BC139F6&key=yt8&kind=asr&lang=en&fmt=json3&xorb=2&xobt=3&xovt=3'

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
textDF<-  textDF[nchar(textDF$text) !=0,]

# Examine to make sure format is ok
head(textDF, 10)

# Save
#write.csv(textDF, 'timedText2.csv', row.names = F)
# End
