#' Title: Commonality Cloud
#' Purpose: Given two corpora find words in common and visualize
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Jan 18 2022
#'

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y <- NA
  tryError <- tryCatch(tolower(x), error = function(e) e)
  if (!inherits(tryError, 'error'))
    y <- tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'amp', 'chardonnay', 'coffee')

# Read in multiple files as individuals
txtFiles <- list.files(pattern = 'chardonnay|coffee', full.names = T)

# Make a list of DFs
txtLst <- lapply(txtFiles, read.csv)
sapply(txtLst, nrow)

#  Apply steps to each list element
for(i in 1:length(txtLst)){
  print(paste('working on',i, 'of', length(txtLst)))
  tmp <- paste(txtLst[[i]]$text, collapse = ' ')
  tmp <- VCorpus(VectorSource(tmp))
  tmp <- cleanCorpus(tmp, stops)
  tmp <- sapply(tmp, content)
  txtLst[[i]] <- tmp
}

# FYI
sapply(txtLst, length)

# Name list elements programmatically
txtFiles
txtNames      <- sapply(strsplit(txtFiles, '/'), tail, 1)
names(txtLst) <- txtNames

# Combine the subject documents into a corpus of *2* documents
allDrinks <- unlist(txtLst)
allDrinks <- VCorpus((VectorSource(allDrinks)))

# How many docs now?
allDrinks

# Make TDM
drinkTDM  <- TermDocumentMatrix(allDrinks)
drinkTDMm <- as.matrix(drinkTDM)

# Make sure order is correct!
colnames(drinkTDMm) <- txtNames

# Examine
head(drinkTDMm)


commonality.cloud(drinkTDMm, 
                  max.words=150, 
                  random.order=FALSE,
                  colors='blue',
                  scale=c(3.5,0.25))


# End

