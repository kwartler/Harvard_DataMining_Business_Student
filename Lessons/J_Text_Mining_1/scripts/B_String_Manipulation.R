#' Title: Intro: Keyword Scanning
#' Purpose: Learn some basic string manipulation
#' Author: Ted Kwartler
#' Date: Apr 28, 2024
#'

# Libraries
library(stringi)
library(stringr)

# Get Data
text <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_2024/main/lessons/Day1_intoR_NLP/data/allComplaints.csv')

# Let's look at the colnames & a portion of the data
names(text)
head(text,2)

# For now let's focus just on Consumer.complaint.narrative
head(text$Consumer.complaint.narrative)

# Logical T/F vector that a string appears at least ONCE
savings   <- grepl("savings", text$Consumer.complaint.narrative, ignore.case=TRUE)
checking  <- grepl("checking", text$Consumer.complaint.narrative, ignore.case=TRUE)

# Find the row positions of a specific word appearing at least ONCE
# this shows the difference between grep and grepl
cc <- grep("credit card", text$Consumer.complaint.narrative, ignore.case=TRUE)
head(cc)

# Grep for indexing; regular expressions can be very complex.
mentionCC <- text[cc,]

# Logical T/F for one word OR another appears at least ONCE
keywords    <-"checking|checking acount|saving|savings|savings account"
checkingSavingAccounts <-grepl(keywords, text$Consumer.complaint.narrative,ignore.case=TRUE)

# Logical T/F for one word AND another
matches <- grep("(?=.*told)(?=.*called)(?=.*back)", 
                text$Consumer.complaint.narrative, 
                perl = TRUE, 
                ignore.case = TRUE)
text$Consumer.complaint.narrative[matches[1]]

# Calculate the % of times among all tweets
sum(checking) / nrow(text)
sum(savings) / nrow(text)
sum(checkingSavingAccounts) / nrow(text)

# Count occurrences of words per tweet
frequencyCheck <- stri_count(text$Consumer.complaint.narrative, fixed="debt")
head(frequencyCheck)
sum(frequencyCheck) / nrow(text)

# Sometimes patterns overlap, for example "credit" and "creditor".  
# So searching "credit" counts both.  
# If you want something specific in all these functions, you anchor the pattern.
sum(stri_count(text$Consumer.complaint.narrative, regex ="the")) #the, their, there
sum(stri_count(text$Consumer.complaint.narrative, regex ="\\bthe\\b")) 

# Another anchoring example; retweets!
exampleTxt <- 'RT I love the Statue of Liberty'
gsub('rt','', exampleTxt)
gsub('rt','', exampleTxt, ignore.case = T)
gsub('^RT','' ,exampleTxt) #another type of anchor
gsub('\\bRT\\b','' ,exampleTxt) # escaped "\b" is actually a "backspace" thus its only looking for that

# Let's just do some substitutions
# Review
text$Consumer.complaint.narrative[997]

# Let's substitute the first instance of AppleCard to Apple Card
sub('AppleCard','Apple Card', text$Consumer.complaint.narrative[997])

# How does this change the results
sum(grepl('Apple Card',text$Consumer.complaint.narrative)) #original

# Make subs to unify terms
substituteApple <- sub('AppleCard','Apple Card', text$Consumer.complaint.narrative)
sum(grepl('Apple Card',substituteApple))

# Now let's try another substitution and do all instances
sum(grepl('Goldman Sachs', text$Consumer.complaint.narrative)) #original

# Now instead of the first term, let's sub all patterns found
substituteGS <- gsub('GS Bank|GSBank','Goldman Sachs', text$Consumer.complaint.narrative)
sum(grepl('Goldman Sachs',substituteGS))

# Let's make things lowercase, this is a step to unify terms for BOW analysis
textA <- 'BAG OF WORDS ANALYSIS STYLE DOCUMENTS.'
textB <- 'bag of words analysis style documents.'
identical(textA, textB)

# Apply to lower
tolower(textA)
identical(tolower(textA), textB)

# Within a document, sometimes its important to find a section or text location
oneComplaint   <- text$Consumer.complaint.narrative[1]
whereIsPattern <- stri_locate_all(oneComplaint, fixed = 'Account')
whereIsPattern #note it's a list

# Can also use regex patterns
whereIsPattern <- stri_locate_all(oneComplaint, regex = 'Account', case_insensitive=TRUE)
whereIsPattern[[1]]

# You can then write a loop to expand this window instead of just the word
charWindow <- 10
allWindows <- vector()
for(i in 1:nrow(whereIsPattern[[1]])){
  start <- whereIsPattern[[1]][i, 1] -charWindow
  end <- whereIsPattern[[1]][i, 2] + charWindow
  patternWindow <- substr(oneComplaint, start, end)
  allWindows[i] <- patternWindow 
}
allWindows

# You can also split strings are specific sections
allText <- c("This is an example sentence...The next sentence needs to be split.",
             "The weather outside is nice...The sky is clear and the sun is shining.",
             "She is going to the concert...He is staying home to study.")

# Split on a pattern, then you need to perform some basic data operations to organize it.
splitDocs <- str_split(allText, "[...]")
splitDocs

# Change the capitalization
str_to_upper(allText, locale = "en")
str_to_lower(allText, locale = "en")
str_to_title(allText, locale = "en")
str_to_sentence(allText, locale = "en")

# There will also be additional white space at the beginning and end
allText <- c("This is an example sentence. The next sentence needs to be split.    ",
             "     The weather outside is nice. The sky is clear and the sun is shining.     ",
             "     She is going to the concert. He is staying home to study. ")
str_trim(allText, side = c("both", "left", "right")) #'left', 'right'

# Middle of string spaces can be cleaned with regex and gsub
gsub("\\s+", " ", '    Example      text to be cleaned    ')
# End
