#' Title: Commonality Cloud
#' Purpose: Given two corpora find words in common and visualize
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: May 12, 2024
#'

# Data Input, locally you can use list.files()
chardonnay <- 'https://github.com/kwartler/Harvard_DataMining_Business_Student/raw/refs/heads/master/Lessons/K_Text_Mining_2/data/chardonnay.csv'
coffee     <- 'https://github.com/kwartler/Harvard_DataMining_Business_Student/raw/refs/heads/master/Lessons/K_Text_Mining_2/data/coffee.csv'
txtFiles <- c(chardonnay, coffee)

# Topic names
topicNames <- c('chardonnay','coffee')

# Libs
library(tm)
library(wordcloud)
library(RColorBrewer)

# Custom functions
# Robust to lower
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Cleaning
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
stops <- c(stopwords('english'), 'lol', 'amp', 'chardonnay', 'coffee')

# Read in the files
for (i in 1:length(txtFiles)){
  assign(topicNames[i], read.csv(txtFiles[i]))
  cat(paste('read completed:',txtFiles[i],'\n'))
}

# Vector Corpus; omit the meta data
chardonnay$text <- stringi::stri_encode(chardonnay$text, "", "UTF-8")
coffee$text <- stringi::stri_encode(coffee$text, "", "UTF-8")
chardonnay <- VCorpus(VectorSource(chardonnay$text))
coffee     <- VCorpus(VectorSource(coffee$text))

# Clean up the data
chardonnay <- cleanCorpus(chardonnay, stops)
coffee     <- cleanCorpus(coffee, stops)

# Extract the cleaned text
chardonnay <- sapply(chardonnay, NLP::content)
coffee     <- sapply(coffee, NLP::content)

# FYI
length(chardonnay)

# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
chardonnay <- paste(chardonnay, collapse = ' ')
coffee     <- paste(coffee, collapse = ' ')

# FYI pt2
length(chardonnay)

# Combine the subject documents into a corpus of *2* documents
allDrinks <- c(chardonnay, coffee)
allDrinks <- VCorpus((VectorSource(allDrinks)))

# How many docs now?
allDrinks

# Make TDM
drinkDTM  <- DocumentTermMatrix(allDrinks)
drinkDTMm <- as.matrix(drinkDTM)

# Make sure order is correct!
rownames(drinkDTMm) <- topicNames

# Examine
drinkDTMm[,1:10]

# Plot the frequent & in common terms; must be a TDM!
commonality.cloud(t(drinkDTMm),
                  max.words=150,
                  random.order=FALSE,
                  colors='blue',
                  scale=c(3.5,0.25))


# End

