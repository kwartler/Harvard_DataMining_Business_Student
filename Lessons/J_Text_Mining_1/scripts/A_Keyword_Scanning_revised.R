#' Title: Intro: Keyword Scanning
#' Purpose: Learn some basic string manipulation
#' Author: Ted Kwartler
#' Date: Mar 12, 2023
#'

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(stringi)

# Options & Functions
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

# Get Data
text <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/J_Text_Mining_1/data/coffeeVector.csv')
head(text$x)


# Logical T/F vector that a string appears at least ONCE
coffee    <- grepl("coffee", text$x, ignore.case=TRUE)
starbucks <- grepl("starbucks", text$x, ignore.case=TRUE)

# Find the row positions of a specific word appearing at least ONCE
#this shows the difference between grep and grepl
grep("mug", text$x, ignore.case=TRUE)

# Grep for indexing; better when you have more information like author and time
text[grep('mug', text$x),]

# Logical T/F for one word OR another appears at least ONCE
keywords    <-"mug|glass|cup"
mugGlassCup <-grepl(keywords, text$x,ignore.case=TRUE)

# Calculate the % of times among all tweets
sum(coffee) / nrow(text)
sum(starbucks) / nrow(text)
sum(mugGlassCup) / nrow(text)

# Count occurrences of words per tweet
theCoffee <- stri_count(text$x, fixed="the")
sum(theCoffee) / nrow(text)

# Example data organization
keywordScans <- data.frame(text = text$x, coffee, starbucks, mugGlassCup)
keywordScans

# End
