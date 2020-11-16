#' Title: Intro: Other Wordclouds
#' Purpose: Make other types of word clouds
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: 2020-Arp-13
#'

# Set the working directory
setwd("/cloud/project/Lessons/J_textMining/data")

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

cleanCorpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
customStopwords <- c(stopwords('english'), 'lol', 'smh', 
                     'amp','drink', 'chardonnay', 'beer','coffee')

# Read in multiple files as individuals
txtFiles<-c('chardonnayVector.csv','coffeeVector.csv','beerVector.csv') #use list.files() for a lot

for (i in 1:length(txtFiles)){
  assign(txtFiles[i], read.csv(txtFiles[i]))
  cat(paste('read completed:',txtFiles[i],'\n'))
} 

# If you are ok with lists, use lapply so each is a list element
all <- pblapply(txtFiles,read.csv)
rm(all)

# Vector Corpus
beer       <- VCorpus(VectorSource(beerVector.csv))
chardonnay <- VCorpus(VectorSource(chardonnayVector.csv))
coffee     <- VCorpus(VectorSource(coffeeVector.csv))

# Cleaning
beer       <- cleanCorpus(beer)
chardonnay <- cleanCorpus(chardonnay)
coffee     <- cleanCorpus(coffee)

# Extract plain clean text out of each corpus same as calling content() on an individual tweet
beer       <- unlist(sapply(beer, `[`, "content"))
chardonnay <- unlist(sapply(chardonnay, `[`, "content"))
coffee     <- unlist(sapply(coffee, `[`, "content"))

# Combine all subject matter tweets (1000) into single document encompassing all tweets
beer       <- paste(beer, collapse=" ")
chardonnay <- paste(chardonnay, collapse=" ")
coffee     <- paste(coffee, collapse=" ")

# To make it clear we now have a single document of all drink tweets
beer
length(beer)

# Make a combined corpus of 3 subject matters
allDrinks   <- c(beer, chardonnay, coffee)
drinkCorpus <- VCorpus(VectorSource(allDrinks))
drinkCorpus

# Make TDM
drinkTDM  <- TermDocumentMatrix(drinkCorpus)
drinkTDMm <- as.matrix(drinkTDM)

# Label the new TDM, remember the order of subjects from lines 80,81, and 82!
colnames(drinkTDMm) = c("Beer", "Chardonnay", "Coffee")
drinkTDMm[50:55,1:3]

# Pallette
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make commonality cloud
commonality.cloud(drinkTDMm, 
                  max.words=150, 
                  random.order=FALSE,
                  colors=pal,
                  scale=c(3.5,0.25))

# Make comparison cloud
comparison.cloud(drinkTDMm, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=brewer.pal(ncol(drinkTDMm),"Dark2"),
                 scale=c(3,0.1))

# End
