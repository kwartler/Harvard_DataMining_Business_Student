#' Title: Intro: Cleaning and Frequency Count
#' Purpose: Learn some basic cleaning functions & term frequency
#' Author: Ted Kwartler
#' Date: Mar 12, 2023
#'

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(tm)
library(qdapRegex)

# Options & Functions
Sys.setlocale('LC_ALL','C')

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
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
customStopwords <- c(stopwords('english'), 'lol', 'smh')

# Data
text <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/J_Text_Mining/data/coffeeVector.csv')
head(text$x)

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text$x))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

# Check Meta Data; brackets matter!!
text$x[4]
txtCorpus[[4]]
meta(txtCorpus[[4]])
content(txtCorpus[[4]])

# Need to plain text cleaned copy?
df <- data.frame(originalText = text$x,
                 cleanText    = unlist(sapply(txtCorpus, `[`, "content")),
                 stringsAsFactors=F)
#write.csv(df,'plain_coffee.csv',row.names = F)

# Compare a single tweet
df[4,]

# Make a Document Term Matrix or Term Document Matrix depending on analysis
txtDtm  <- DocumentTermMatrix(txtCorpus)
txtTdm  <- TermDocumentMatrix(txtCorpus)
txtDtmM <- as.matrix(txtDtm)
txtTdmM <- as.matrix(txtTdm)

# Examine to get used to the object
dim(txtDtmM)
dim(txtTdmM)

mugSection <- grep('mug', colnames(txtDtmM))
txtDtmM[1:6,mugSection]
txtTdmM[mugSection,1:6]

# End
