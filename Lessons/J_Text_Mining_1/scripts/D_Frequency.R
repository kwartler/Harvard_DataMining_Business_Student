#' Title: Intro: Frequency Count 
#' Purpose: obtain the frequent terms and visualize
#' Author: Ted Kwartler
#' Date: Apr 28, 2024
#'

# Libs
library(tm)
library(qdapRegex)
library(ggplot2)
library(ggthemes)
library(dplyr)

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
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
customStopwords <- c(stopwords('english'), 'bank', 'money', 'account')

# Data
text <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_2024/main/lessons/Day1_intoR_NLP/data/allComplaints.csv')

# Substitutions
text$Consumer.complaint.narrative <- gsub('(X{2}\\/X{2}\\/X{4})|(X{2}\\/X{2}\\/[0-9]{2,4})|([0-9]{2}\\/[0-9]{2}\\/[0-9]{2,4})', '', text$Consumer.complaint.narrative, perl = T)
text$Consumer.complaint.narrative <- gsub('X+', 
                                          '', 
                                          text$Consumer.complaint.narrative)
 

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text$Consumer.complaint.narrative))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

# Make a DTM & convert
txtDtm  <- DocumentTermMatrix(txtCorpus)
txtDtmM <- as.matrix(txtDtm)

# Construct the WFM
termFreq <- colSums(txtDtmM)
termFreq <- data.frame(word      = names(termFreq),
                       frequency = termFreq, 
                       row.names = NULL)

# Let's examine the WFM
dim(termFreq)
head(termFreq)

# Simple barplot, adjust number for bars
topWords <- top_n(termFreq, 12)

# Chg to factor so bars are ordered by value not alphabetical
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=frequency, y=reorder(word, frequency))) + 
  geom_col(fill ='darkred') + theme_gdocs() +
  geom_text(aes(label=frequency), colour="grey",hjust=1.25, size=3.0)


############ Back to PPT

# Inspect word associations; takes about 45 seconds
associations <- findAssocs(txtDtm, c('called'), 0.3)
associations

# Organize the word associations
assocDF <- data.frame(terms     = names(associations[[1]]),
                      value     = unlist(associations),
                      row.names = NULL)

# Chg to factor so bars are ordered by value not alphabetical
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
assocDF

# Make a dot plot
ggplot(assocDF, aes(x=value, y=reorder(terms,value))) +
  geom_point(col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="#c00c00",hjust="inward", vjust ="inward" )

# End

