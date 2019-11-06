#' Author: Ted Kwartler
#' Date: 10-30-2019
#' Purpose: Demonstrate getting XML API information

# Libraries
library(xml2)
library(stringr)
library(rvest)

# WD
setwd("/cloud/project/Lessons/I_APIs_NewDataSources_ReportingAutomation/data")

# Youtube URL
#https://www.youtube.com/watch?v=Q-wRhzWaCac
youtubeCaption <- 'https://www.youtube.com/api/timedtext?v=NYL-wPVzL64&asr_langs=de%2Cen%2Ces%2Cfr%2Cit%2Cja%2Cko%2Cnl%2Cpt%2Cru&caps=asr&hl=en&ip=0.0.0.0&ipbits=0&expire=1572505909&sparams=ip%2Cipbits%2Cexpire%2Cv%2Casr_langs%2Ccaps&signature=17FAA1DE8B165BCFA99A5DAD82706D4F25D12178.1184F0ED35424F3426C49AC592C497E1AD24AA0C&key=yt8&lang=en&name=en&fmt=srv3&xorb=2&xobt=3&xovt=3'

# Go get the data
dat <- read_xml(youtubeCaption)

# Extract text, remove carriage returns, remove special characters
text <- xml_text(dat)
text <- str_replace_all(text, "[\r\n]" , "")
text <- iconv(text, "latin1", "ASCII", sub="")

# Save as a text file
writeLines(text,'someNews.txt')

# Or to organize all of the information, XML "nodes" to list 
pNodes    <- xml_find_all(dat, "//p")
startTime <- xml_attr(pNodes, 't')
duration  <- xml_attr(pNodes, 'd')
text      <- xml_text(pNodes)

# Organize
textDF <- data.frame(startTime, duration, text)

# Drop line breaks if it causes no text rows; otherwise use text<-str_replace_all(text, "[\n]" , "") to drop while preserving text on line
textDF<-  textDF[!grepl('[\n]', textDF$text),]

# Examine to make sure format is ok
head(textDF, 10)

write.csv(textDF, 'timedText2.csv', row.names = F)

# End
