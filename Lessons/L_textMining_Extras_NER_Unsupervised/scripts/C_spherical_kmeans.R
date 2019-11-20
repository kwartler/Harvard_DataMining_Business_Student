#' Title: K Means
#' Purpose: apply spherical k-means
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2019-11-19
#'

# Wd
setwd("/cloud/project/Lessons/L_textMining_Extras_NER_Unsupervised/data")

# Libs
library(skmeans)
library(tm)
library(clue)
library(cluster)
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
#txt <- read.csv('1yr_plus_final4.csv')# worth looking to see better clustering
txt <- read_fst("jeopardyArchive_1000.fst")
View(txt[1:2,])

names(txt)[1] <- 'doc_id'
names(txt)[2] <- 'text'
txtCorpus     <- VCorpus(DataframeSource(txt))
txtCorpus     <- cleanCorpus(txtCorpus, customStopwords)
txtDtm        <- DocumentTermMatrix(txtCorpus, control = list(weighting =  weightTfIdf))
txtMat        <- as.matrix(txtDtm)

# Remove empty docs w/TF-Idf
txtMat <- subset(txtMat, rowSums(txtMat) > 0)

# Apply Spherical K-Means
txtSKMeans <- skmeans(txtMat, 5, m = 1, control = list(nruns = 5, verbose = T))
barplot(table(txtSKMeans$cluster), main = 'spherical k-means')

# Plot cluster to see separation
plotcluster(cmdscale(dist(txtMat)),txtSKMeans$cluster)

# Silhouette plot
sk <- silhouette(txtSKMeans)
plot(sk, col=1:3, border=NA)

# ID protypical terms
protoTypical           <- t(cl_prototypes(txtSKMeans))
colnames(protoTypical) <- paste0('cluster_',1:ncol(protoTypical))
comparison.cloud(protoTypical, max.words =50, scale = c(.25,.75), 
                 title.size = 1)

clusterWC <- list()
for (i in 1:ncol(protoTypical)){
  jsWC <- data.frame(term = rownames(protoTypical),
                     freq = protoTypical[,i])
  jsWC <- jsWC[order(jsWC$freq, decreasing = T),]
  clusterWC[[i]] <- wordcloud2::wordcloud2(jsWC[1:35,])
  print(paste('plotting',i))
}

clusterWC[[1]]
clusterWC[[2]]
clusterWC[[3]]

# Examine a portion of the most prototypical terms per cluster
nTerms <- 5
(clustA <- sort(protoTypical[,1], decreasing = T)[1:nTerms]) #
(clustB <- sort(protoTypical[,2], decreasing = T)[1:nTerms]) #
(clustC <- sort(protoTypical[,3], decreasing = T)[1:nTerms]) #

# End