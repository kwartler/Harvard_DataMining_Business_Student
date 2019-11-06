#' Author: Ted Kwartler
#' Date: 4-3-2019
#' Purpose: Demonstrate getting json API information

# Libraries
library(jsonlite)
library(pbapply)

# Options
options(stringsAsFactors = F)

# www.newsapi.org Key
apiKey <- 'c6166540623e4588b2b8b53143c1299a'

# Top headlines in the US endpoint:
usURL <- paste0('https://newsapi.org/v2/top-headlines?country=us&apiKey=', apiKey)
usURL

# Endpoint for all news sources
allNewsSources <- paste0('https://newsapi.org/v2/sources?apiKey=', apiKey)
allNewsSources <- fromJSON(allNewsSources)

# Get names and ID 
newsSourceCodes <- list()
for (i in 1:length(allNewsSources$sources$id)){
  x <- allNewsSources$sources$id[i]
  y <- allNewsSources$sources$name[i]
  z <- data.frame(id  =  x, name = y)
  newsSourceCodes[[i]] <- z
}

newsSourceCodes <- do.call(rbind, newsSourceCodes)
head(newsSourceCodes)
newsSourceCodes[4,1]

# Get last weeks information
to   <- Sys.Date()
from <- to-7


# Let's get Al Jazeera Top Headlines
apiURL <- paste0('https://newsapi.org/v2/top-headlines?sources=',
                newsSourceCodes[4,1],
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
write.csv(finalContent, '~/finalNewsContent.csv', row.names = F)

# End
