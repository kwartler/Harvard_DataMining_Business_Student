#' Title: Dealing with Emoji
#' Author: Ted Kwartler
#' email: edward.kwartler@fas.harvard.edu
#' Date: May 21, 2024
#'

# Libs
library(tm) # displays the emojis correctly
library(emoji) # Get the emoji lexicon or load one manually
library(emojifont) # another emoji library to explore
library(textclean) #another one!
library(mgsub) #used for substitutions
library(qdapRegex)
#library(qdap) #emoticons functions

# Examine built in lexicon
head(emojis)
tail(emojis)
nrow(emojis)

# What's really going on?
emojis$emoji[2]

# 8 bits (4 * 2 character codes)
charToRaw(emojis$emoji[2]) #The four bytes f0, 9f, 98, and 83 correspond to the four "code units" of the UTF-8 encoding of the emoji.

# 16 bits (4 * 16 binary codes)
intToBits(charToRaw(emojis$emoji[2]))

# Convert it back
rawToChar(charToRaw(emojis$emoji[2]))

# Read in some data
unicorns <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/refs/heads/master/Lessons/K_Text_Mining_2/data/unicorns.csv')

# Small sample
unicorns$text[c(720, 804)]

# smile face halo
emojis[14,1:2]
grep(emojis$emoji[14], unicorns$text)

# OPTION 1: REMOVE EMOJIS, FAST!
# 1. Remove: GSUB all non-ascii character; this removes all non-English too
# Regex Exaplantion "^" anything but the 1-127th ASCII character is sub'ed to ""
# Yes I had to look that up :)
gsub("[^\x01-\x7F]", "", unicorns$text[c(720, 804)])

# Remove all
st <- Sys.time()
rmTxt <- gsub("[^\x01-\x7F]", "", unicorns$text)
Sys.time() - st #0.01secs for 1k tweets
rmTxt[c(720, 804)]

# 1A Remove: qdapRegex, removes text based emoticons only!  Special characters remain
x <- c("are :-)) it 8-D he XD on =-D they :D of :-) is :> for :o) that :-/",
       "as :-D I xD with :^) a =D to =) the 8D and :3 in =3 you 8) his B^D was")

rm_emoticon(x)
ex_emoticon(x)


# OPTION 2: SUBSTITUE EMOJIS, SLOW!
# 2. Substitute them with the lexicon
# Remember mgsub library is text, pattern then replacement, which is different than normal gsub
# Namespace conflict with textclean, qdap & mgsub
mgsub::mgsub(unicorns$text[c(720, 804)], emojis$emoji, emojis$name)

# Since emojis are often without spaces:
st <- Sys.time()
subTxt <- mgsub::mgsub(unicorns$text[1:1000],
                       emojis$emoji,
                       paste0(' ', emojis$name,' '))
Sys.time() - st #10x longer for 1000 tweets, imagine more! ~2.5min
subTxt[c(720, 804)]

# Converts to ASCII so not always helpful, but works in some common emojis
textclean::replace_emoji(unicorns$text[804])

# Example problem for default inputs
unicorns$text[9]
textclean::replace_emoji(unicorns$text[9])

# End

