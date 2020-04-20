#' Author: Ted Kwartler
#' Date: 10-30-2019
#' Purpose: Demonstrate getting XML API information

# Libraries
library(xml2)
library(stringr)
library(rvest)

# WD
setwd("/cloud/project/lessons/9_Apr3_APIs_ReportingAutomation/wk9_data")

# Youtube URL
#https://www.youtube.com/watch?v=Q-wRhzWaCac
youtubeCaption <- 'https://www.youtube.com/api/timedtext?caps=asr&expire=1556702117&sparams=asr_langs%2Ccaps%2Cv%2Cxoaf%2Cxorp%2Cexpire&key=yttt1&hl=en&v=Q-wRhzWaCac&xoaf=1&asr_langs=ko%2Cde%2Cpt%2Cja%2Cen%2Ces%2Cit%2Cnl%2Cfr%2Cru&signature=4C0D5CE17D7F098835E1BDBFCE10D0C6BD886B69.B1ED8D8871AEE2A5DDEC57B4EE7EF8EE11F1D491&xorp=True&kind=asr&lang=en&fmt=srv3'

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
