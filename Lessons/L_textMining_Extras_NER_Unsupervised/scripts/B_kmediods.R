#' Title: K Means
#' Purpose: apply k-mediods
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2019-11-19
#' https://cran.r-project.org/web/packages/kmed/vignettes/kmedoid.html

# Wd
setwd("/cloud/project/Lessons/L_textMining_Extras_NER_Unsupervised/data")

# Libs
library(kmed)
library(tm)
library(clue)
library(cluster)
library(wordcloud)

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

names(txt)[1] <- 'doc_id'
txtCorpus     <- VCorpus(DataframeSource(txt))
txtCorpus     <- cleanCorpus(txtCorpus, customStopwords)
txtDtm        <- DocumentTermMatrix(txtCorpus, control = list(weighting =  weightTfIdf))
txtMat        <- as.matrix(txtDtm)

# Remove empty docs w/TF-Idf
txtMat <- subset(txtMat, rowSums(txtMat) > 0)

# Use a manhattan distance matrix; default for kmed
manhattanDist <- distNumeric(txtMat, txtMat, method = "mrw")

# Calculate the k-mediod
txtKMeds <- fastkmed(manhattanDist, ncluster = 5, iterate = 5)

# Number of docs per cluster
table(txtKMeds$cluster)
barplot(table(txtKMeds$cluster), main = 'k-mediod')

# Visualize separation
plotcluster(manhattanDist, txtKMeds$cluster, pch = txtKMeds$cluster)

# Two silhouette functions...kmed
silKMed <- kmed::sil(manhattanDist, txtKMeds$medoid, txtKMeds$cluster)

# Examine results
silKMed$result[c(1:3,22:26, 47:50),]
silKMed$plot

## Rerun w 2 clusters ie good/not good to see a more usual silouette plot 

# 2nd silhouette functions...kmed
silPlot <- cluster::silhouette(txtKMeds$cluster, manhattanDist)
plot(silPlot, border=NA)

# Median centroid documents:
txtKMeds$medoid

txt$text[40]
txt$text[24]
txt$text[10]
txt$text[4]
txt$text[47]

# End