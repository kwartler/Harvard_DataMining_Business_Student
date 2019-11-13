#' Title: Text Mining Homework
#' Purpose: Identify what reviews associate with good and bad reviews
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2019-11-13
#' Instructions:
#' Fill in the rest of the script, and answer the questions inline.  Then upload to canvas.

# Set the working directory
setwd("___________________________")

# Libs
library(__) #tm package 
library(qdap)
library(____) # package for wordclouds
library(RColorBrewer)
library(tidytext) # for those not able to load qdap
library(dplyr) # for those not able to load qdap

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
  print('removed urls')
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) #new: isn't to is not
  print('replace contractions')
  corpus <- tm_map(corpus, removePunctuation)
  print('remove punctuation')
  corpus <- tm_map(corpus, stripWhitespace)
  print('stip whitespace')
  corpus <- tm_map(corpus, removeNumbers)
  print('remove numbers')
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  print('making lowercase')
  corpus <- tm_map(corpus, removeWords, customStopwords)
  print('DONE!')
  return(corpus)
}

# Get your data
text <- read.csv(______________)

# 1. How many comments are there BEFORE the sample step?
# Answer:

# Get a sample of 1000 to speed up time
set.seed(1234)
idx <- sample (1:nrow(text),1000)
text <- text[idx, ]

# Examine
dim(____)
text$comments[1]

# Review the last 6 comments

# Define custom stopwords, add "apartment" and "boston" to the list
customStopwords <- c(stopwords('english'), 'place', '_______', '__________')

# Make the VECTOR of text called $comments into a volatile corpus 
txtCorpus <- VCorpus(VectorSource(text$comments))

# Apply the cleaning function to the volatile corpus; takes a moment!!
txtCorpus<-_____________(txtCorpus)

# Make a Document Term Matrix 
txtDTM <- _____________(txtCorpus)

# 2. How many terms are in this DTM?
# Answer: 

# Convert TDM to a simple matrix; takes a moment
txtDTMm <- _____________(txtDTM)

# Get column Sums and sort decreasing (hint its a T or F)
txtDTMv <- sort(______(txtDTMm),decreasing = )

# Organize the row sums
txtDF <- data.frame(word = names(txtDTMv),freq=txtDTMv)

# Examine the first 6 rows
head(txtDF)

# 3. What is the third most frequent term used in the reviews?
# Answer: 

# Choose the "Purples" color & drop light ones in the color palette
pal <- brewer.pal(8, "_____")
pal <- pal[-(1:2)]

# Make a simple word cloud of all terms with 75 words with your palette object
set.seed(1234)
wordcloud(txtDF$word,txtDF$freq, max.words=__, random.order=FALSE, colors=___)

# 4. Overall, are comments using positive or negative terms frequently?  Be sure to expand your plots pane to ensure most words are shown
# Answer: 

# Now that you have examined the entire corpus, time to split it based on polarity(); this take a while!
polarityScores <- ______(text$comments)

# 5. What is the average polarity score for the comments?
# Answer: 

# Using each document's polarity score segregate the comments into positive and negative corpora
justScores    <- polarityScores$all$polarity # just the polarity values
documentClass[is.na(documentClass)] <- 0 # clean up any NA values
documentClass <- ifelse(justScores>0,TRUE,FALSE) # change to binary
table(documentClass)

# 6. What is the number of positive reviews versus negative reviews?
# Answer: 

# Now you can subset the original documents by their positive or negative nature and transpose them to TDM
documentClass <- which(documentClass == 1)  
posReviews    <- text$comments[documentClass]
negReviews    <- text$comments[-documentClass]

# Collapse them into a single corpus for each class
posReviews <- paste(posReviews, _________ = ' ')
negReviews <- paste(negReviews, _________ = ' ')

# Declare each as a volatile corpus from a vector source
posReviews <- _______(VectorSource(posReviews))
negReviews <- VCorpus(___________(negReviews))

# Clean each of the corpora; takes awhile
posReviews<-___________(posReviews)
negReviews<-___________(negReviews)

# Collapse the cleaned segregated corpora
posReviews <- _____(posReviews, ________ = " ")
negReviews <- _____(negReviews, ________ = " ")

# Make a combined corpus of pos/neg
allReviews <- c(posReviews, negReviews)

# Now combine into a VCorpus, using a VectorSource
allReviewsCorpus <- ________(__________(allReviews))

# 7. How many documents are in this corpus?
# Answer: 

# Make TDM and change to a simple matrix
allReviewsTDM <- ____________(allReviewsCorpus)
allReviewsTDMm <- as.matrix(allReviewsTDM)

# Label the new TDM, remember the order of subjects
colnames(allReviewsTDMm) = c("Positive", "Negative")

# Make comparison cloud with 75 words, random.order = FALSE, and title.size = 0.5
set.seed(1234)
comparison.cloud(allReviewsTDMm, max.words=__, random.order=_____,
                 title.size=___,colors=brewer.pal(ncol(allReviewsTDMm),"Dark2"))

# 8. Based on this visual (remember its only a sample so may not be accurate).  
# What are the 4 most frequent terms used in positive reviews?  Does this indicate a host matters?
# Answer: 

# End



