#' Purpose: Build a simple word cloud with bi-grams
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 12, 2024
#'

# Declare the data path
filePath  <- 'https://github.com/kwartler/Harvard_DataMining_Business_Student/raw/refs/heads/master/Lessons/L_Text_Mining_2/data/chardonnay.csv'

# Libs
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggwordcloud)

# Custom functions
# Robust to lower
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Cleaning
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "),
         use.names = FALSE)}

# Create custom stop words
stops <- c(stopwords('english'), 'chardonnay', 'lol')

# Data
text <- read.csv(filePath)

# Fix the encoding for some students
text$text <- stringi::stri_encode(text$text, "", "UTF-8")

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text$text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
txtDTM  <- DocumentTermMatrix(txtCorpus,
                               control=list(tokenize=bigramTokens))
txtDTMm <- as.matrix(txtDTM)

# See a bi-gram
exampleTxt <- grep('wine country', colnames(txtDTMm))
txtDTMm[870:871, (exampleTxt-2):(exampleTxt)]

# Get Column Sums & organize
txtSums <- colSums(txtDTMm)
txtFreq <- data.frame(word=names(txtSums),
                      frequency=txtSums,
                      row.names = NULL)


# Order
topWordsA <- txtFreq[order(txtFreq$frequency, decreasing=F),]

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(topWordsA$word,
          topWordsA$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

pdf('~/Desktop/Harvard_DataMining_Business_Student/personalFiles/exampleWC.pdf')
wordcloud(topWordsA$word,
          topWordsA$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))
dev.off()

# More common ggplot interface, scale continuous colors
plotDF <- topWordsA[order(topWordsA$frequency, decreasing = T)[1:100],]
ggplot(plotDF, aes(label = word, size = frequency, color = frequency)) +
  geom_text_wordcloud() +
  theme_minimal() +
  scale_color_gradient(low = "black", high = "red") #scale_color_gradient2(low = 'grey',high='black') will make it a spectrum

# More common ggplot interface, Single Color
ggplot(plotDF, aes(label = word, size = frequency, color = 'red')) +
  geom_text_wordcloud() +
  theme_minimal()


# End
