#' Make a DTM, get frequency, and association of unexpected terms
#' TK
#' May 22, 2024
#' 

# libraries
library(tm)
library(stringi)
library(ggplot2)
library(ggthemes)
library(dplyr)

# Custom Functions
# Try to lower
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# clean Corpus
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


# Create custom stop words
customStopwords <- c(stopwords('english'), 'coffee')

# data
text <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/J_Text_Mining_1/data/coffeeVector.csv')
head(text)

# Find the tweets that mention "mug"
idx <- grepl('mug', text$x, ignore.case = T)
onlyMugs <- text[idx,]

# Row numbers
idx <- grep('mug', text$x, ignore.case = T)
onlyMugs <- text[idx,]

# Any substitutions? Starbuck's to Starbucks
text$x <- gsub('Starbuck\'s','Starbucks', text$x)

# How many tweets contain the term starbucks?
sum(grepl('starbucks',text$x, ignore.case = T)) /nrow(text)

# Make a Corpus from a Vector Source
coffeeCorp <- VCorpus(VectorSource(text$x))

# Clean the Corpus
coffeeCorp <- cleanCorpus(coffeeCorp, customStopwords)

# Examine the fourth tweet, so we know what has changed
text$x[4]
content(coffeeCorp[[4]])

# Extract a cleaned copy of the text and save it to the personal files folder
cleanText <- sapply(coffeeCorp, content)
write.csv(cleanText, '~/Desktop/ICPSR/personalFiles/cleanCoffee.csv', row.names = F)

# Create a DTM and change it to a simple matrix
coffeeDTM  <- DocumentTermMatrix(coffeeCorp)
coffeeDTMm <- as.matrix(coffeeDTM)

# Find the column names that have "mug" anchored within grep within the DTM
mugColumns <- grep('mug', colnames(coffeeDTMm))

# Review the first 6 rows and the index of "mug" previously created
coffeeDTMm[1:6,mugColumns]

# Most Frequent Terms
coffeeWFM <- colSums(coffeeDTMm)

# Make a data frame
coffeeWFM <- data.frame(word = names(coffeeWFM),
                        frequency = coffeeWFM,
                        row.names = NULL)

# Order by freq
coffeeWFM <- coffeeWFM[order(coffeeWFM$frequency, decreasing = T),]

# build a bar chart of the top 10 terms
topTen <- coffeeWFM[1:10,]

ggplot(topTen, aes(x = word, y = frequency)) +
  geom_col(fill ='darkred') + 
  theme_wsj() +
  ggtitle('frequent terms for coffee twitter')

# End