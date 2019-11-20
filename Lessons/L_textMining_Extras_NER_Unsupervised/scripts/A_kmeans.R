#' Title: K Means
#' Purpose: apply k-means
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2019-11-19
#'

# Wd
setwd("/cloud/project/Lessons/L_textMining_Extras_NER_Unsupervised/data")

# Libs
library(tm)
library(clue)
library(cluster)
library(fst)
library(wordcloud)

# This is an orphaned lib which gives us plotcluster:
#https://www.rdocumentation.org/packages/fpc/versions/2.1-11.2
#library(fpc)

# Bring in our supporting functions
source('/cloud/project/Lessons/L_textMining_Extras_NER_Unsupervised/scripts/Z_plotCluster.R')

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

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

# Stopwords
customStopwords  <- c(stopwords('SMART'), 'work')

# Read & Preprocess
txt <- read.csv('1yr_plus_final4.csv')
txt$text[1]

names(txt)[1] <- 'doc_id'
txtCorpus     <- VCorpus(DataframeSource(txt))
txtCorpus     <- cleanCorpus(txtCorpus, customStopwords)
txtDtm        <- DocumentTermMatrix(txtCorpus, control = list(weighting =  weightTfIdf))
txtMat        <- as.matrix(txtDtm)

txtMat    <- scale(txtMat) #subtract mean  & divide by stDev
set.seed(1234)
txtKMeans <- kmeans(txtMat, 3)
txtKMeans$size
barplot(txtKMeans$size, main = 'k-means')

plotcluster(cmdscale(dist(txtMat)),txtKMeans$cluster)

dissimilarityMat <- dist(txtMat)
silPlot          <- silhouette(txtKMeans$cluster, dissimilarityMat)
plot(silPlot, col=1:max(txtKMeans$cluster), border=NA)


#calculate indices of closest document to each centroid
idx <- vector()
for (i in 1:max(txtKMeans$cluster)){
  
  # Calculate the absolute distance between doc & cluster center
  absDist <- abs(txtMat[which(txtKMeans$cluster==i),] -  txtKMeans$centers[i,])
  
  # Check for single doc clusters
  if(is.null(nrow(absDist))==F){
    absDist <- rowSums(absDist)
    minDist <- subset(absDist, absDist==min(absDist))
  } else {
    minDist <- txtKMeans$cluster[txtKMeans$cluster==i]
  }
  idx[i] <- as.numeric(names(minDist))
}

# Notification of closest doc to centroid
cat(paste('cluster',1:max(txtKMeans$cluster),': centroid doc is ', idx,'\n'))

txt$text[34]
txt$text[41]
txt$text[40]

# End
