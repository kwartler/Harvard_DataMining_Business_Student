#' Title: String Search Manipulation
#' Purpose: Learn some basic string manipulation functions
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 23-2020
#'

# Set the working directory
setwd("~/Documents/Harvard_DataMining_Business_Student/Lessons/K_textMining_documentClassification/data")

# Libs
library(stringi)
library(data.table)

# Options & Functions
options(stringsAsFactors = FALSE, scipen = 999) #text strings will not be factors of categories
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

# Get Data
text <- fread('M_Oct2020.csv', nrows = 10000)

# Logical T/F vector that a string appears at least ONCE
cavs  <- grepl("cavaliers", text$text, ignore.case=TRUE)
curry <- grepl("curry", text$text, ignore.case=TRUE)

# Review Logical Output
head(curry, 10)

# Find the row positions of a specific word appearing at least ONCE
#this shows the difference between grep and grepl
grep("lakers", text$text, ignore.case=TRUE)

# Grep for indexing
idx <- grep('rockets', text$text)[2]
text[idx,2]

# Logical T/F for one word OR another appears at least ONCE
keywordsOR  <-"houston|rockets|chris paul"
idxOR         <- grepl(keywordsOR, text$text,ignore.case=TRUE)
head(text$text[idxOR], 1)

# Logical Search AND operator, regular expression
keywordsAND <- "(?=.*lebron)(?=.*james)"
idxAND      <- grepl(keywordsAND, text$text,perl=TRUE)
head(text$text[idx],1)

# Calculate the % of times among all tweets
sum(cavs) / nrow(text)
sum(curry) / nrow(text)
sum(idxOR) / nrow(text)
sum(idxAND) / nrow(text)

# Count occurences of words per tweet
text$text[2]
theCount    <- stri_count(text$text, fixed ="the")
theCountGrep <- stri_count(text$text, regex ="\\bthe\\b") #anchored, nearly equivalent
identical(theCount, theCountGrep)
theCount[2] #"RT @thetruthsburner:..."
theCountGrep[2] #"the" is not present as a standalone
sum(theCount) / nrow(text) #value greater than 1

# Suppose you want to make regular expression substitutions
idxCavs <- grep("cavs", text$text)
originalCavs <- text[idxCavs,]
originalCavs[47,2]
gsub('cavs', 'cavaliers', originalCavs[47,2])

# BE VERY CAREFUL! Sometimes Anchors matter!! Let's remove the RT (retweets)
exampleTxt <- 'RT I love the Statue of Liberty'
gsub('rt','', exampleTxt)
gsub('rt','', exampleTxt, ignore.case = T)
gsub('^RT','' ,exampleTxt) #another type of anchor
gsub('\\bRT\\b','' ,exampleTxt) # escaped "\b" is actually a "backspace" thus its only looking for that

# End

