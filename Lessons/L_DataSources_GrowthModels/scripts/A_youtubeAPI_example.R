#' Title: Grab Youtube JSON
#' Purpose: Demonstrate f12 in Chrome for API
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 27, 2022
#'

# Libraries
library(jsonlite)
library(stringr)
library(plyr)

# Options; google api returns UTF-8 text
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# WD
setwd("~/Desktop/GSERM_Text_Remote_student/student_lessons/E_SyntacticParsing_DataSources/data")


# Youtube URL
# https://www.youtube.com/watch?v=K5Rly83zfuI&ab_channel=TheDailyShowwithTrevorNoah
youtubeCaption <- "https://www.youtube.com/api/timedtext?v=K5Rly83zfuI&caps=asr&xoaf=5&hl=en&ip=0.0.0.0&ipbits=0&expire=1669689178&sparams=ip%2Cipbits%2Cexpire%2Cv%2Ccaps%2Cxoaf&signature=6389500C8EE1DA2CC36A72E6896511C1BEFB0B26.DE6A626AC2C1AC7A8F18574D7CAEF6B78FAB38D5&key=yt8&lang=en-US&fmt=json3&xorb=2&xobt=3&xovt=3&cbrand=apple&cbr=Chrome&cbrver=105.0.0.0&c=WEB&cver=2.20221122.06.00&cplayer=UNIPLAYER&cos=Macintosh&cosver=10_15_7&cplatform=DESKTOP"

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
textDF <- data.frame(startTime = dat$events$tStartMs/1000,
                     duration  = dat$events$dDurationMs/1000,
                     text = unlist(lapply(dat$events$segs, "[", 'utf8') ))

# Examine to make sure format is ok
head(textDF, 10)

# End
