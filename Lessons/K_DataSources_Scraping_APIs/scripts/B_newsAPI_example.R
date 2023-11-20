#' Title: https://newsapi.org
#' Purpose: Get data from JSON
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 19, 2023
#'

# Libraries
library(jsonlite)
library(stringi) # fixes some formatting issues

# Options
options(stringsAsFactors = F)

# www.newsapi.org Key
#apiKey <- 'XXXX'

# Top headlines in the US endpoint:
usURL <- paste0('https://newsapi.org/v2/top-headlines?country=us&apiKey=', apiKey)
usURL

# The documentation has other sources
# Get last weeks information from Al Jazeera English
to   <- Sys.Date()-1
from <- to-8 

# Let's get Al Jazeera Top Headlines
apiURL <- paste0('https://newsapi.org/v2/top-headlines?sources=',
                'al-jazeera-english', # get from the sources endpoint
                '&from=', from,
                '&',
                'to=', to,
                '&sortBy=popularity&apiKey=',
                apiKey)
apiURL

# Let's get the text
newsInfo <- fromJSON(apiURL)

# Organize the API response and save
newsInfo$status           <- NULL
newsInfo$totalResults     <- NULL
newsInfo$articles$source  <- NULL

finalContent <- newsInfo[[1]]
finalContent$content <- stri_unescape_unicode(finalContent$content)
finalContent

# End
