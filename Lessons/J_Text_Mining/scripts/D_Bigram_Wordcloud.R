#' Title: Intro: Simple Wordcloud
#' Purpose: Learn about wordclouds and make one
#' Author: Ted Kwartler
#' Date: Mar 12, 2023
#' 

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(tm)
library(qdapRegex)
library(wordcloud)
library(RColorBrewer)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
customStopwords <- c(stopwords('english'), 'lol', 'smh', 'sivb')

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
  }
  
# Data
text <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/J_Text_Mining/data/sivbVector.csv')

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text$x))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
tweetTDM  <- DocumentTermMatrix(txtCorpus, 
                               control=list(tokenize=bigramTokens))
tweetTDMm <- as.matrix(tweetTDM)

# See a bi-gram
idx <- grep('silicon valley', colnames(tweetTDMm))
tweetTDMm[1:6,idx]

# Get Row Sums & organize
tweetTDMmVec <- sort(colSums(tweetTDMm), decreasing = TRUE)
wordFreqDF   <- data.frame(word      = names(tweetTDMmVec), 
                           freq      = tweetTDMmVec, 
                           row.names = NULL)

# Review all Pallettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]


# Make simple word cloud
# Reminder to expand device pane
wordcloud(wordFreqDF$word,
          wordFreqDF$freq,
          max.words=50,
          random.order=FALSE,
          colors=pal)

# End