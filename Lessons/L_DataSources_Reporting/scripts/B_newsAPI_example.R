#' Author: Ted Kwartler
#' Date: 11-30-2020
#' Purpose: Demonstrate getting json API information

# Libraries
library(jsonlite)
library(pbapply)
#install.packages(“newsanchor“)# now there is a package but its worth exploring manually to learn APIs

# Options
options(stringsAsFactors = F)

# www.newsapi.org Key
apiKey <- 'b6d9f96c78b34ee98bd84a23d3f74bfb'

# Top headlines in the US endpoint:
usURL <- paste0('https://newsapi.org/v2/top-headlines?country=us&apiKey=', apiKey)
usURL

# Endpoint for all news sources
#https://newsapi.org/v2/sources?apiKey=e5ad9f536b3a47139d1c009cd7628a68

# Get last weeks information
to   <- Sys.Date()
from <- to-7

# Let's get Al Jazeera Top Headlines
apiURL <- paste0('https://newsapi.org/v2/top-headlines?sources=',
                'al-jazeera-english', # get from the sources endpoint
                '&from=', from,
                '&',
                'to=', to,
                '1&sortBy=popularity&apiKey=',
                apiKey)
apiURL

# Let's get the text
newsInfo <- fromJSON(apiURL)

# Organize the API response and save
newsInfo$status           <- NULL
newsInfo$totalResults     <- NULL
newsInfo$articles$source  <- NULL

finalContent <- newsInfo[[1]]
finalContent
#write.csv(finalContent, '~/finalNewsContent.csv', row.names = F)

# End
