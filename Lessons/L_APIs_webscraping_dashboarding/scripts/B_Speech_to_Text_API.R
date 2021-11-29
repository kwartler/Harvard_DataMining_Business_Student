#' Title: Speech to Text
#' Purpose: Use an API to perform speech to text
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Oct 20, 2021
#' googleLanguageR is a package to perform the some of the same functions you've learned
#' but there are other APIs
#' Refs: https://cran.r-project.org/web/packages/googleLanguageR/vignettes/setup.html
#' http://code.markedmondson.me/googleLanguageR/index.html
#' IT COSTS MONEY, SO BE CAREFUL

# libs
library(googleLanguageR)

#wd
setwd("~/Desktop/LUX_NLP_student/lessons/oct21/data")

# Authenticate
#gl_auth('~/Documents/googleCreds/speech2txt-25cb48408ae7.json')


#### General NLP API
# Google NLP - Named Entity Analysis (R has this for free w/library openNLP)
# Google NLP - Part of Speech Tagging (R has this for free w/library UDpipe)
# Google NLP - Sentiment (R has this for free w/multiple libs and approaches)
# Google NLP - Document Tagging (R *could* do this as a multi-class problem)
texts     <- paste(readLines('C05791318.txt'), collapse = ' ')
#nlpResult <- gl_nlp(texts)
#saveRDS(nlpResult,'nlpResult.rds')
nlpResult <-readRDS('nlpResult.rds')

# POS tagging & meta
nlpResult$sentences
nlpResult$tokens
nlpResult$language

# Sentiment
nlpResult$documentSentiment$score

# Tagged Topic
nlpResult$classifyText

#### Google Translation API
text <- "Text Mining in Practice with R. It's the math of talking, your two favorite things! "

## translate British into Danish
#translatedTxt <- gl_translate(text, target = "fr")
#saveRDS(translatedTxt, 'translatedTxt_FR.rds')
translatedTxt <- readRDS('translatedTxt.rds')
translatedTxt$translatedText

#### Speech to Text
#http://www.voiptroubleshooter.com/open_speech/american.html
#speechToTxt <- gl_speech('trimmed.wav', sampleRateHertz = 8000)
#saveRDS(speechToTxt, 'speechToTxt.rds')
speechToTxt <- readRDS('speechToTxt.rds')

# Results
speechToTxt$transcript

# Timed Text
speechToTxt$timings

# Text to Audio;requires the text-to-speech api enabled
gl_talk_player(gl_talk(text, 
               output = 'someAudio.wav',
               gender = 'FEMALE'))

# End