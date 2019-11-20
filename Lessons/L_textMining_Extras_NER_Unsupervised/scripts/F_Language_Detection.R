#' Title: Language detection
#' Purpose: Use textcat to ID which language is used
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2019-11-20
#'

# Set the working directory
setwd("/cloud/project/Lessons/L_textMining_Extras_NER_Unsupervised/data")

# Libs
library(textcat)

# Options & Functions
testing <- T
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

# Data
if(testing == T){
  unknownLanguageOne <- read.csv("tweets_Haddad_Fernando.csv", 
                                 nrows = 100)
} else {
  unknownLanguageOne <- read.csv("tweets_Haddad_Fernando.csv")
  
}

# Example languages supported
t(t(names(TC_byte_profiles)))
     
# Options for the profiles
attr(TC_char_profiles, "options")[c("n", "size", "reduce", "useBytes")]

# Categorize the language
txtLanguage <- textcat(unknownLanguageOne$text)

# Review; overall OK!
head(txtLanguage, 10)

# Problematic texts; perhaps cleaning or longer passages would help
unknownLanguageOne$text[3]
unknownLanguageOne$text[4]
unknownLanguageOne$text[9]

# Most frequent
table(txtLanguage)

# End