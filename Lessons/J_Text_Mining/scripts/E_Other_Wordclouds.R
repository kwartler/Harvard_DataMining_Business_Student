#' Title: Intro: Other Wordclouds
#' Purpose: Make other types of word clouds
#' Author: Ted Kwartler
#' Date: Mar 12, 2023
#'

# Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(tm)
library(wordcloud)
library(qdapRegex)
library(RColorBrewer)

# Options & Functions
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
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
customStopwords <- c(stopwords('english'), 'lol', 'smh', 
                     'amp','sivb', 'bank', 'sequoia')

# Read in multiple files as individuals
txtFiles<-c('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/J_Text_Mining/data/sivbVector.csv',
            'https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/J_Text_Mining/data/Sequoia%20CapitalVector.csv',
            'https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/J_Text_Mining/data/hashtagBankCrashVector.csv') 
documentTopics <- c('siliconValley','Sequoia', 'bankCrash')

# Read in as a list
all <- lapply(txtFiles,read.csv)

# This could be made more concise but we're going to do it within a loop
cleanText <- list()
for(i in 1:length(all)){
  x <- VCorpus(VectorSource(all[i])) #declare as a corpus
  x <- cleanCorpus(x, customStopwords) #clean each corpus
  x <- unlist(sapply(x, `[`, "content")) #extract out each cleaned text using content()
  x <- paste(x, collapse = ' ') #collapse all text from 1000 tweet documents to 1 blob of text by subject
  cleanText[[documentTopics[i]]] <- x #put it into the list
}

# To make it clear we now have a single document of all tweets for a subject
length(all[[1]]$x)
length(cleanText$siliconValley)


# Make a combined corpus of 3 subject matters
allTopics <- unlist(cleanText)
allTopics <- VCorpus(VectorSource(allTopics))
allTopics

# Make TDM
topicDTM  <- DocumentTermMatrix(allTopics)
topicDTMm <- as.matrix(topicDTM)

# Label the new TDM, remember the order of subjects from lines 80,81, and 82!
rownames(topicDTMm) <- documentTopics
topicDTMm[1:3,50:55]

# Pallette
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# commonality.cloud requires rows as terms and columns as documents.
# Thus, it really needs a TermDocumentMatrix but to demonstrate that its still the same data, we just transpose it.
# So, its a simple transposition here but really should be adjusted above in DocumentTermMatrix to TermDocumentMatrix & 
# colnames() section too
dev.off()
commonality.cloud(t(topicDTMm), 
                  max.words=150, 
                  random.order=FALSE,
                  colors=pal,
                  scale=c(3.5,0.25))

# Make comparison cloud
dev.off()
comparison.cloud(t(topicDTMm), 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=c('goldenrod', 'tomato', 'green'),
                 scale=c(3,0.1))

# End
