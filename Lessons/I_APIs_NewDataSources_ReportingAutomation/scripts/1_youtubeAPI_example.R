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
# https://www.youtube.com/watch?v=NYL-wPVzL64
youtubeCaption <- 'https://www.youtube.com/api/timedtext?v=NYL-wPVzL64&asr_langs=de%2Cen%2Ces%2Cfr%2Cit%2Cja%2Cko%2Cnl%2Cpt%2Cru&caps=asr&hl=en&ip=0.0.0.0&ipbits=0&expire=1572487648&sparams=ip%2Cipbits%2Cexpire%2Cv%2Casr_langs%2Ccaps&signature=B3D22F5B5C27A7FB46FDB8570ADF88E237F47B78.EC7130F83C6DDA3D40703E563E81E2145714B57C&key=yt8&lang=en&name=en&fmt=srv3&xorb=2&xobt=3&xovt=3'

# Go get the data
dat <- read_xml(youtubeCaption)

# Extract text, remove carriage returns, remove special characters
text <- xml_text(dat)
text <- str_replace_all(text, "[\r\n]" , "")
text <- iconv(text, "latin1", "ASCII", sub="")

# Save as a text file
writeLines(text,'someNews.txt')

# Or to organize all of the information, XML "nodes" to list 
xmlData <- as_list(dat)

# Extract the text
text   <- do.call(rbind, xmlData$timedtext$body)
text   <- unlist(text)

# Extract and organize the timing info
timing <- lapply(xmlData$timedtext$body, attributes)
timing <- lapply(timing, unlist)
timing <- do.call(rbind, timing)
timing <- apply(timing, 2,as.numeric)

# Organize into DF
df <- data.frame(timing, text)

# Improve names
names(df)[1:2] <- c('startTimeOnScreen','Duration')

# Examine ot make sure format is ok
head(df, 10)

write.csv(df, 'timedText.csv', row.names = F)

# End


