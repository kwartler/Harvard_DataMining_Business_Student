#' Title: Intro: Keyword Scanning
#' Purpose: Learn some basic string manipulation
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 20, 2023
#'

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(stringi)

# Options & Functions
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

# Get Data
text <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/J_Text_Mining/data/coffee.csv')
text <- as.data.frame(text)

# Logical T/F vector that a string appears at least ONCE
coffee    <- grepl("coffee", text$text, ignore.case=TRUE)
starbucks <- grepl("starbucks", text$text, ignore.case=TRUE)

# Find the row positions of a specific word appearing at least ONCE
#this shows the difference between grep and grepl
grep("mug", text$text, ignore.case=TRUE)

# Grep for indexing
text[grep('mug', text$text),2]

# Logical T/F for one word OR another appears at least ONCE
keywords    <-"mug|glass|cup"
mugGlassCup <-grepl(keywords, text$text,ignore.case=TRUE)

# Calculate the % of times among all tweets
sum(coffee) / nrow(text)
sum(starbucks) / nrow(text)
sum(mugGlassCup) / nrow(text)

# Count occurrences of words per tweet
theCoffee <- stri_count(text$text, fixed="the")
sum(theCoffee) / nrow(text)

# Example data organization
keywordScans <- data.frame(text, coffee, starbucks, mugGlassCup)
keywordScans

# End
