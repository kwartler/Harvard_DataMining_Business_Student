#' Title: Comparison Cloud
#' Purpose: Given two corpora find disjoint words and visualize
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Jan 18 2022

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/K_textMining_part2/data")

# Options
options(scipen = 999)

# Libs
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)
library(pbapply)

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
  #corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'amp', 'chardonnay', 'beer', 'coffee')

# Read in multiple files as individuals
txtFiles <- list.files(pattern = 'beer|chardonnay|coffee', full.names = T)

# Make a list of DFs
txtLst <- lapply(txtFiles, read.csv)

#  Apply steps to each list element
for(i in 1:length(txtLst)){
  print(paste('working on',i, 'of', length(txtLst)))
  tmp <- paste(txtLst[[i]]$text, collapse = ' ')
  tmp <- VCorpus(VectorSource(tmp))
  tmp <- cleanCorpus(tmp, stops)
  tmp <- sapply(tmp, content)
  txtLst[[i]] <- tmp
}

# Name list elements programmatically
txtFiles
txtNames      <- sapply(strsplit(txtFiles, '/'), tail, 1)
names(txtLst) <- txtNames

# Combine the subject documents into a corpus of *3* documents
allDrinks <- unlist(txtLst)
allDrinks <- VCorpus((VectorSource(allDrinks)))

# Make TDM with a different control parameter
# Tokenization `control=list(tokenize=bigramTokens)`
# You can have more than 1 ie `control=list(tokenize=bigramTokens, weighting = weightTfIdf)`
ctrl      <- list(weighting = weightTfIdf)
drinkTDM  <- TermDocumentMatrix(allDrinks, control = ctrl)
drinkTDMm <- as.matrix(drinkTDM)

# Make sure order is the same as the c(objA, objB) on line ~80
colnames(drinkTDMm) <- txtNames

# Examine
head(drinkTDMm)

# Make comparison cloud
comparison.cloud(drinkTDMm, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=brewer.pal(ncol(drinkTDMm),"Dark2"),
                 scale=c(3,0.1))

# End

