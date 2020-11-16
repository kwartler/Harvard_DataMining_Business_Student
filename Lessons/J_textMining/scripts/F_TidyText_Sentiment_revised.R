#' Title: Intro: TidyText Sentiment
#' Purpose: Sentiment nonsense
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 16 2020
#'

# Set the working directory
setwd("/cloud/project/Lessons/J_textMining/data")

# Libs
library(tidytext)
library(dplyr)
library(qdap)
library(tm)
library(radarchart)
library(textdata)
library(ggplot2)

# Options & Functions
options(stringsAsFactors = FALSE)

# Custom Functions
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}


# Data
text <- c(readLines('starboy.txt'), 
          readLines('in_your_eyes.txt'),
          readLines('pharrell_williams_happy.txt'))
text

docNames <- c("starboy", "eyes", "happy") 

# Create custom stop words
customStopwords <- c(stopwords('english'))

# Clean Corpus
txtCorpus <- VCorpus(VectorSource(text))
txtCorpus <- cleanCorpus(txtCorpus)

# DTM
txtDTM    <- DocumentTermMatrix(txtCorpus)
txtDTM
dim(txtDTM)

# Tidy
tidyCorp <- tidy(txtDTM)
tidyCorp
dim(tidyCorp)

# Get bing lexicon
# "afinn", "bing", "nrc", "loughran"
bing <- get_sentiments(lexicon = c("bing"))
head(bing)

# Perform Inner Join
bingSent <- inner_join(tidyCorp,bing, by=c('term'='word'))
bingSent

# Quick Analysis - unique words
table(bingSent$document,bingSent$sentiment)

# Quick Analysis - count of words
aggregate(count~document+sentiment, bingSent, sum)

# Compare with qdap::Polarity
polarity(text[1])
polarity(text[2])
polarity(text[3])

# Get afinn lexicon
#afinn <- get_sentiments(lexicon = c("afinn")) # causes a crash on rstudio.cloud, lexicon provided in data folder
afinn <- read.csv('/cloud/project/Lessons/J_textMining/data/AFINN/afinn.csv')
head(afinn)

# Word Sequence
tidyCorp$idx       <- as.numeric(ave(tidyCorp$document, 
                                      tidyCorp$document, FUN=seq_along))
# Perform Inner Join
afinnSent <- inner_join(tidyCorp,afinn, by=c('term'='word'))
afinnSent

# Calc
afinnSent$ValueCount <- afinnSent$value * afinnSent$count 
afinnSent

# Recode to documents
afinnSent$document <- recode_factor(afinnSent$document, 
                                    `1` = docNames[1], 
                                    `2` = docNames[2],
                                    `3` = docNames[3])

# Visualization, keep in mind these are words in alphabetical order, some analysis would use time
ggplot(afinnSent, aes(idx, ValueCount, fill = document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~document, ncol = 2, scales = "free_x")

# If you did have a timestamp you can easily make a timeline of sentiment using this code
# The idx here is not temporal but this is an example if you were tracking over time instead of alpha
plotDF <- subset(afinnSent, afinnSent$document=='happy')
ggplot(plotDF, aes(x=idx, y=ValueCount, group=document, color=document)) +
  geom_line()

# Get nrc lexicon, again causes probs on rstudio cloud
#nrc <- textdata::lexicon_nrc() # should download it
nrc <- read.csv('/cloud/project/Lessons/J_textMining/data/NRC/nrc.csv')
head(nrc)

# Perform Inner Join
nrcSent <- inner_join(tidyCorp,nrc, by=c('term' = 'word'))
nrcSent

# Drop pos/neg leaving only emotion
nrcSent <- nrcSent[-grep('positive|negative',nrcSent$sentiment),]

# Quick chk
table(nrcSent$sentiment,nrcSent$document)

# Manipulate for radarchart
nrcSentRadar <- as.data.frame.matrix(table(nrcSent$sentiment, nrcSent$document))
nrcSentRadar
colnames(nrcSentRadar) <- docNames

# Normalize for length; prop.table needs a "matrix" class...annoying!
nrcSentRadar <- prop.table(as.matrix(nrcSentRadar),2)
nrcSentRadar
colSums(nrcSentRadar)

# Organize
nrcSentRadar <- data.frame(labels = rownames(nrcSentRadar),
                           nrcSentRadar)
rownames(nrcSentRadar) <- NULL
nrcSentRadar

# Chart
chartJSRadar(scores = nrcSentRadar, labelSize = 10, showLegend = T)
# End