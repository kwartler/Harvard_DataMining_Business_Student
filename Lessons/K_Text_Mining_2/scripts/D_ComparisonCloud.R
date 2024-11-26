#' Purpose: Given two corpora find disjoint words and visualize
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
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
coffee$text <- stringi::stri_encode(coffee$text, "", "UTF-8")
chardonnay$text <- stringi::stri_encode(chardonnay$text, "", "UTF-8")
coffee     <- VCorpus(VectorSource(coffee$text))
chardonnay <- VCorpus(VectorSource(chardonnay$text))

# Clean up the data
coffee     <- cleanCorpus(coffee, stops)
chardonnay <- cleanCorpus(chardonnay, stops)

# Another way to extract the cleaned text
coffee     <- sapply(coffee, NLP::content)
chardonnay <- sapply(chardonnay, NLP::content)

# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
coffee     <- paste(coffee, collapse = ' ')
chardonnay <- paste(chardonnay, collapse = ' ')

# Combine the subject documents into a corpus of *2* documents
allDrinks <- c(chardonnay, coffee)
allDrinks <- VCorpus((VectorSource(allDrinks)))

# Make TDM with a different control parameter
# Tokenization `control=list(tokenize=bigramTokens)`
# You can have more than 1 ie `control=list(tokenize=bigramTokens, weighting = weightTfIdf)`
ctrl      <- list(weighting = weightTfIdf)
drinkDTM  <- DocumentTermMatrix(allDrinks, control = ctrl)
drinkDTMm <- as.matrix(drinkDTM)

# Make sure order is the same as the c(objA, objB) 
rownames(drinkDTMm) <- c('chardonnay', 'coffee')

# Make comparison cloud; requires TDM!
comparison.cloud(t(drinkDTMm),
                 max.words=75,
                 random.order=FALSE,
                 title.size=0.5,
                 colors=c('tomato','goldenrod'),
                 scale=c(3,0.1))

# End
