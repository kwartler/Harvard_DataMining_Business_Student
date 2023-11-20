#' Title: Intro: Frequency Count & Dendrogram
#' Purpose: Learn about and visualize a dendrogram
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 20, 2023
#'

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(tm)
library(qdap)
library(ggplot2)
library(ggthemes)
library(ggdendro)

# Options & Functions
options(stringsAsFactors = FALSE)
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
text <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/J_Text_Mining/data/beer.csv', encoding = "Latin1")

# As of tm version 0.7-3 tabular was deprecated
names(text)[1] <- 'doc_id' 

# Build a volatile corpus
txtCorpus <- VCorpus(DataframeSource(text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

# Make TDM
beerTDM  <- TermDocumentMatrix(txtCorpus)
beerTDMm <- as.matrix(beerTDM)

# Frequency Data Frame
beerFreq <- rowSums(beerTDMm)
beerFreq <- data.frame(word=names(beerFreq),
                       frequency=beerFreq, 
                       row.names = NULL)

# Simple barplot; values greater than 90 
topWords      <- subset(beerFreq, beerFreq$frequency >= 90) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)

# qdap version, will not work if there is a java issue
plot(freq_terms(text$text, top=35, at.least=2, stopwords = customStopwords))
x <- freq_terms(text$text, top=35, at.least=2, stopwords = customStopwords)
head(x)

############ Back to PPT

# Inspect word associations
associations <- findAssocs(beerTDM, 'zombie', 0.30)
associations
t(t(unlist(associations)))

# Organize the word associations
zombieDF <- data.frame(terms = names(associations[[1]]),
                       value = unlist(associations),
                       row.names = NULL)
zombieDF$terms <- factor(zombieDF$terms, levels=zombieDF$terms)
zombieDF

# Make a dot plot
ggplot(zombieDF, aes(y=terms)) +
  geom_point(aes(x=value), data=zombieDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" )

############ Back to PPT

# Reduce TDM
beerTDM2 <- removeSparseTerms(beerTDM, sparse=0.97) #shoot for ~50 terms;  i.e.  3% of cells have a non-zero value in a row.  If the row has 100 columns (terms) then 3 or more have a value.  Lowering to 0.96 sparsity means 4 or more cells have a value and so on.  
beerTDM2

# Organize the smaller TDM
beerTDM2 <- as.data.frame(as.matrix(beerTDM2))

# Basic Hierarchical Clustering
hc <- hclust(dist(beerTDM2))
plot(hc,yaxt='n')

# Improved visual
ggdendrogram(hc, rotate=FALSE) + ggtitle('Dendrogram - word frequency clusters')

# End
