#' Title: Grab Youtube JSON
#' Purpose: Demonstrate f12 in Chrome for API
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Apr 15, 2024
#'

# Libraries
library(jsonlite)
library(stringr)
library(plyr)

# Options; google api returns UTF-8 text
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")


# Youtube URL
# https://www.youtube.com/watch?v=K5Rly83zfuI&ab_channel=TheDailyShowwithTrevorNoah
# https://www.youtube.com/watch?v=sal78ACtGTc&ab_channel=SequoiaCapital
youtubeCaption <- 'https://www.youtube.com/api/timedtext?v=sal78ACtGTc&ei=lq8dZuWBGf2elu8PiqSwsAc&caps=asr&opi=112496729&xoaf=5&hl=en&ip=0.0.0.0&ipbits=0&expire=1713246726&sparams=ip%2Cipbits%2Cexpire%2Cv%2Cei%2Ccaps%2Copi%2Cxoaf&signature=55E66BC809219CC2083BA0F982A701EEFCCBFEF1.593D4205317852ABA74C4BF3A18901C2CF6DE04D&key=yt8&kind=asr&lang=en&fmt=json3&xorb=2&xobt=3&xovt=3&cbrand=apple&cbr=Chrome&cbrver=123.0.0.0&c=WEB&cver=2.20240415.01.00&cplayer=UNIPLAYER&cos=Macintosh&cosver=10_15_7&cplatform=DESKTOP'

# Go get the data
dat <- fromJSON(youtubeCaption) # you can even pass in a URL to go to a webpage

# closed captioning data
dat$events$tStartMs
dat$events$dDurationMs
dat$events$segs[1:10]

# Get each first column called utf8
rawTxt <- lapply(dat$events$segs, "[", 'utf8') 

# organize just the single column
rawTxt <- do.call(rbind, rawTxt)

# Drop line returns "\n"
rawTxt <- gsub('[\r\n]',' ',rawTxt[,1])

# Sometimes there are entries that are empty so they need to be dropped
head(rawTxt,10)
rawTxt <- rawTxt[nchar(rawTxt) != "0"]

# Sometimes, there is extra spacing from the gsub
rawTxt <- str_squish(rawTxt)

# If you want it as a single chunk
oneChunk <- paste(rawTxt, collapse = ' ')

# If you want to retain the meta data
tmpText <- lapply(dat$events$segs, "[", 'utf8')
tmpTextList <- list()
for(i in 1:length(tmpText)){
  if(is.null(tmpText[[i]])){
    tmp <- 'NULL'
  } else {
    tmp <- apply( tmpText[[i]], 2, paste, collapse = ' ')
    tmp <- trimws(tmp)
  }
  tmpTextList[[i]] <- tmp
  
}


textDF <- data.frame(startTime = dat$events$tStartMs/1000,
                     duration  = dat$events$dDurationMs/1000,
                     text = unlist(tmpTextList))

# Examine to make sure format is ok
head(textDF, 10)

# End
