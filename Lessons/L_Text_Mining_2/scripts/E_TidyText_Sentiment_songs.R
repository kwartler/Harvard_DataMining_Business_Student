#' Title: Intro: TidyText Sentiment
#' Purpose: Sentiment nonsense
#' Author: Ted Kwartler
#' Date: May 12, 2024
#'

# Libs
library(tidytext)
library(dplyr)
library(tm)
library(radarchart)
library(textdata)
library(ggplot2)
library(tidyr)

# Custom Functions
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

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
customStopwords <- c(stopwords('english'))

# Read in multiple files as individuals
txtFiles<-c( 'https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/L_Text_Mining_2/data/in_your_eyes.txt', 
             'https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/L_Text_Mining_2/data/pharrell_williams_happy.txt',
             'https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/L_Text_Mining_2/data/starboy.txt') 
documentTopics <- c("in_your_eyes.txt", "pharrell_williams_happy.txt", "starboy.txt") 

# Read in as a list
all <- lapply(txtFiles,readLines)

# This could be made more concise but we're going to do it within a loop
cleanTibbles <- list()
for(i in 1:length(all)){
  x <- VCorpus(VectorSource(all[i])) #declare as a corpus
  x <- cleanCorpus(x, customStopwords) #clean each corpus
  x <- DocumentTermMatrix(x) #make a DTM
  x <- tidy(x) #change orientation
  x$document <- documentTopics[i]
  cleanTibbles[[documentTopics[i]]] <- x #put it into the list
}

# Examine
cleanTibbles$in_your_eyes.txt
dim(cleanTibbles$in_your_eyes.txt)

# Organize into a single tibble
allText <- do.call(rbind, cleanTibbles)

# Get bing lexicon
# "afinn", "bing", "nrc", "loughran"
bing <- get_sentiments(lexicon = c("bing"))
bing

# Perform Inner Join
bingSent <- inner_join(allText,
                       bing, 
                       by=c('term'='word'))
bingSent

# Quick Analysis - count of words
bingResults <- aggregate(count~document+sentiment, bingSent, sum)
bingResults <- pivot_wider(bingResults, names_from = document, values_from = count)
as.data.frame(bingResults)

# Get afinn lexicon
afinn <- get_sentiments(lexicon = c("afinn")) 
afinn

# Word Sequence
allText$idx       <- as.numeric(ave(allText$document, 
                                    allText$document, FUN=seq_along))
# Perform Inner Join
afinnSent <- inner_join(allText,
                        afinn, 
                        by=c('term'='word'))
afinnSent

# Calc
afinnSent$ValueCount <- afinnSent$value * afinnSent$count 
afinnSent

# Get nrc lexicon,notice that some words can have multiple sentiments
nrc <- lexicon_nrc()

# Perform Inner Join
nrcSent <- inner_join(allText,
                      nrc, 
                      by = c('term' = 'word'),
                      relationship = "many-to-many")
nrcSent

# Drop pos/neg leaving only emotion
nrcSent <- nrcSent[-grep('positive|negative',nrcSent$sentiment),]

# Unique polarized term tally
table(nrcSent$sentiment,nrcSent$document) #unique polarized words 

# Sum of the polarized words
# Now lets use this for a radar chart
nrcSentRadar <- aggregate(count~document + sentiment, nrcSent, sum) %>%
  pivot_wider(names_from = document, values_from = count) %>% as.data.frame()
nrcSentRadar

# Normalize for length; prop.table by column is "2"
tmp <- as.matrix(nrcSentRadar[,2:ncol(nrcSentRadar)])
tmp <- prop.table(tmp,2)
tmp
colSums(tmp) #quick check to see what prop table did

# Organize
plotDF <- data.frame(labels = nrcSentRadar[,1],
                     tmp,
                     row.names = NULL)
plotDF

# Chart
chartJSRadar(scores = plotDF, labelSize = 10, showLegend = T)
# End
