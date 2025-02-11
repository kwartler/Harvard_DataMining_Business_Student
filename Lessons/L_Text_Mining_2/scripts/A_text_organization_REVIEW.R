#' Purpose: Learn some basic cleaning functions & term frequency
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 11, 2024
#'

# Libs
library(tm)

# File path
filePath <- 'https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/K_Text_Mining_2/data/studentLoan_2024.csv'

# Custom functions
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
stops <- c(stopwords('english'), 'loan', 'student')

# Data
text <- read.csv(filePath)

# Substitutions
text$Consumer.complaint.narrative <- gsub('(X{2}\\/X{2}\\/X{4})|(X{2}\\/X{2}\\/[0-9]{2,4})|([0-9]{2}\\/[0-9]{2}\\/[0-9]{2,4})', '', text$Consumer.complaint.narrative, perl = T)
text$Consumer.complaint.narrative <- gsub('X+', 
                                          '', 
                                          text$Consumer.complaint.narrative)

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text$Consumer.complaint.narrative))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Make a DTM & convert
txtDtm  <- DocumentTermMatrix(txtCorpus)
txtDtmM <- as.matrix(txtDtm)

# Examine a portion so it's salient for us
dim(txtDtmM)
mostFreq <- colSums(txtDtmM)
mostFreq[which.max(mostFreq)]
txtDtmM[35:40,grep('payment', colnames(txtDtmM))[10:13]]
# End