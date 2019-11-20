#' Title: UD Syntactic Parsing
#' Purpose: Apply syntactic parsing
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2019-11-19
#'

# Libs
library(udpipe)
library(tm)
library(pbapply)
library(qdap)
library(reshape2)

# wd
setwd("/cloud/project/Lessons/L_textMining_Extras_NER_Unsupervised/data")

# Inputs
datPth          <- 'tweets_jairbolsonaro.csv'
testing         <- T
nonBasicStops   <- c('segura', 'seguro')

# Options
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

# Bring in the cleaning functions
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

# Get a language model to the server
# https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html
udModel <- udpipe_download_model(language = "portuguese-gsd", 
                                 model_dir = getwd())

# Load into the space
udModel <- udpipe_load_model('portuguese-gsd-ud-2.4-190531.udpipe')
#udModel <- udpipe_load_model(file.choose())


# Bring in data & organize
textData <- read.csv(datPth)
text     <- data.frame(doc_id = 1:nrow(textData),
                       text   = textData$text)

# Convert
#Encoding(text$text)
#iconvlist()
#stri_enc_detect(text$text)
text$text <- iconv(text$text, "latin1", "ISO-8859-1", sub="")

# Apply the cleaning function, then get the plain text version
textCorp <- VCorpus(DataframeSource(text))
textCorp <- cleanCorpus(textCorp, c(stopwords('portuguese'),nonBasicStops))
text     <- pblapply(textCorp, content)


text[[1]] #Uh oh!
text     <- pblapply(textCorp, bracketX)
text[[1]] #better

# Re-organize to keep track of doc_id
text <- data.frame(doc_id = 1:nrow(textData), 
                   text   = unlist(text))

# Reduce for testing
nDocs            <- ifelse(testing ==T, 10, nrow(text))
syntatcicParsing <- udpipe(text[1:nDocs,], object = udModel)
head(syntatcicParsing)
tail(syntatcicParsing)

# ID and replace any non-lemma terms
syntatcicParsing$cleanTxt <- ifelse(is.na(syntatcicParsing$lemma), 
                                    syntatcicParsing$token, 
                                    syntatcicParsing$lemma)

# Aggregate back to the document level
lemmaText        <- aggregate(syntatcicParsing$cleanTxt,
                              list(syntatcicParsing$doc_id), paste, collapse=" ")

names(lemmaText) <- c('doc_id', 'text')

# Compare original to lemmatized text
text[1,2]
lemmaText[1,]

# From here you can reapply to get a lemmatized version of a DTM, using tokenization etc
# You can also get dense data about the document, without a agg function default is count
reshape2::dcast(syntatcicParsing, doc_id ~  xpos)

# End
