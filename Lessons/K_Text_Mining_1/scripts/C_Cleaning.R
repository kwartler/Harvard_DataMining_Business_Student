#' Title: Intro: Cleaning and Frequency Count
#' Purpose: Learn some basic cleaning functions & term frequency
#' Author: Ted Kwartler
#' Date: Apr 28, 2024
#'

# Libraries
library(tm)
library(qdapRegex)

# Custom Functions
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
stopwords('english')
customStopwords <- c(stopwords('english'), 'bank', 'money', 'account', 'lol')

# Data
text <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/K_Text_Mining_1/data/allComplaints.csv')
text$Consumer.complaint.narrative[1]

# Global substitutions with more complex regex; Redacted dates
# We have to double slash \\ to tell R that we're searching for the / in the dates
# (X{2}\\/X{2}\\/X{4}) 2x's/2x's/4x's
# (X{2}\\/X{2}\\/[0-9]{2,4}) 2x's/2x's/2-4 length digits; this will also cover XX/XX/XX since first section looks for 2x's/2x's anyway
# ([0-9]{2}\\/[0-9]{2}\\/[0-9]{2,4}) 2 digits/2digits/2-4 length digits
text$Consumer.complaint.narrative <- gsub('(X{2}\\/X{2}\\/X{4})|(X{2}\\/X{2}\\/[0-9]{2,4})|([0-9]{2}\\/[0-9]{2}\\/[0-9]{2,4})', '', text$Consumer.complaint.narrative, perl = T)

# Global substitutions for words of any length capital X
text$Consumer.complaint.narrative <- gsub('X+', 
                                          '', 
                                          text$Consumer.complaint.narrative)

# Let's review our substitutions:
text$Consumer.complaint.narrative[1]

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text$Consumer.complaint.narrative))

# This is a corpus class object now
class(text$Consumer.complaint.narrative)
class(txtCorpus)

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

# Check Meta Data; brackets matter!!
# Raw text record
text$Consumer.complaint.narrative[1]

# In the Corpus object
txtCorpus[[1]]

# What meta data is associated with it?
meta(txtCorpus[[1]])

# What is the content after processing?
content(txtCorpus[[1]])

# Cleaning takes a long time so it's a good idea to save a version of processed text
cleanText <- sapply(txtCorpus, content)

# You can add back to the original or save separately
#df <- cbind(text, cleanText)
#write.csv(df,'plain_coffee.csv',row.names = F)

# Make a Document Term Matrix or Term Document Matrix, to avoid confusion we're just doing DTM
txtDtm  <- DocumentTermMatrix(txtCorpus)

# How big is this?
dim(txtDtm)

# This is a specific, efficient object class from library slam
# Sparse simple triplet matrices don't store 0s, only the column names and values.
# Its hard for new programmers to learn 
class(txtDtm)
str(txtDtm) #i=document, j=term index, v=frequency/count value 
??col_sums #example function from library(slam)

# Often its easier to fill in 0s and arrange as rows/columns
txtDtmM <- as.matrix(txtDtm)

# Let's examine a portion
txtDtmM[5319:5322,grep('marcus',colnames(txtDtmM))]
# End
