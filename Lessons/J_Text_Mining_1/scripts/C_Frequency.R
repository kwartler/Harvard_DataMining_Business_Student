#' Title: Intro: Frequency Count 
#' Purpose: obtain the frequent terms and visualize
#' Author: Ted Kwartler
#' Date: Mar 12, 2023
#'

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(tm)
library(qdapRegex)
library(ggplot2)
library(ggthemes)

# Options & Functions
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y         = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error')){}
  y         = tolower(x)
  return(y)
}

cleanCorpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
customStopwords <- c(stopwords('english'), 'lol', 'smh', 'beer', 'amp')

# Data 
text <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/J_Text_Mining/data/sivbVector.csv')
 

# Build a volatile corpus
txtCorpus <- VCorpus(VectorSource(text$x))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

# Make TDM
tweetDTM  <- DocumentTermMatrix(txtCorpus)
tweetDTMm <- as.matrix(tweetDTM)
dim(tweetDTMm)

tweetFreq <- colSums(tweetDTMm)
tweetFreq <- data.frame(word=names(tweetFreq),
                       frequency=tweetFreq, 
                       row.names = NULL)

# Simple barplot; values greater than 50 
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 50) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

############ Back to PPT

# Inspect word associations
associations <- findAssocs(tweetDTM, 'offline', 0.10)
associations

# Organize the word associations
tweetDF <- data.frame(terms = names(associations[[1]]),
                       value = unlist(associations),
                       row.names = NULL)
tweetDF$terms <- factor(tweetDF$terms, levels=tweetDF$terms)
tweetDF

# Make a dot plot
ggplot(tweetDF, aes(y=reorder(terms,value))) +
  geom_point(aes(x=value), data=tweetDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" )

# End

