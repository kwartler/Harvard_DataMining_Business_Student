#' Title: Text Mining Homework
#' Purpose: Identify what reviews associate with good and bad reviews
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2018-11-24
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
library(radarchart) # for those not able to load qdap

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

# 1. How many comments are there BEFORE the sample?
# Answer:

# Get a sample of 1000 to speed up time
set.seed(1234)
idx <- sample (1:nrow(text),1000)
text <- text[idx, ]

# Examine
dim(____)
text$comments[1]

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

# Get column Sums and sort decreasing =TRUE
txtDTMv <- sort(______(txtDTMm),decreasing=TRUE)

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

# 5B For those of you that can't install qdap.  Perform an inner_join with the lexicon shown in class and then use table.

# Make it Tidy
tidyCorp <- ____(txtDTM)

# Get a sentiment lexicon that has positive & negative
bing     <- get_sentiments(lexicon = c("____"))

# Perform the correct type of join
bingSent <- _________(tidyCorp, bing, by=c('term'='word'))

# Tabulate the sentiment retuls
_____(bingSent$sentiment)

# 5B. For those that can load QDAP: What is ratio of positive to negative words (positive / negative)?
# Answer: 

# Now get the NRC lexicon to constuct a radatChart
nrc <- get_sentiments(lexicon = c("___"))

# Perform Inner Join
nrcSent <- inner_join(_____, ___, by=c('term'='word'))

# Quick Analysis
table(____$sentiment)
emos <- data.frame(table(nrcSent$____))
emos <- emos[-c(6,7),] #drop the higher level positive/negative to get explicit emotion

# Review
emos

# Make a radar plot
chartJSRadar(scores=____)

# 6. How many joyful words were identified?
# Answer:

# 7. How many anger words were identified?
# Answer:

# 8. Add up the number of trust, joy, & anticipation words.  Add up the sadness, fear & anger terms.  Divide the two summed values (positive / negative).   
# Answer:

# End



