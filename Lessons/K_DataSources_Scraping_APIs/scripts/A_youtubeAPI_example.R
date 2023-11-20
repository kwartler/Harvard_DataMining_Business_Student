#' Title: Grab Youtube JSON
#' Purpose: Demonstrate f12 in Chrome for API
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 19, 2023
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
youtubeCaption <- 'https://www.youtube.com/api/timedtext?v=K5Rly83zfuI&ei=rclaZZ60F_CI_9EPtpGY4AE&caps=asr&opi=112496729&xoaf=4&hl=en&ip=0.0.0.0&ipbits=0&expire=1700473885&sparams=ip%2Cipbits%2Cexpire%2Cv%2Cei%2Ccaps%2Copi%2Cxoaf&signature=0CAC75781745CA2A32AA5A2056B85AE074838E17.6C344D4CE558888818E0C202A95B7C016E7901A3&key=yt8&lang=en-US&fmt=json3&xorb=2&xobt=3&xovt=3&cbrand=apple&cbr=Chrome&cbrver=119.0.0.0&c=WEB&cver=2.20231117.01.04&cplayer=UNIPLAYER&cos=Macintosh&cosver=10_15_7&cplatform=DESKTOP'

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
