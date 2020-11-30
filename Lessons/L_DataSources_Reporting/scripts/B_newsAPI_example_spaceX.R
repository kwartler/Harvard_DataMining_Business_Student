#' Author: Ted Kwartler
#' Date: 4-3-2019
#' Purpose: Demonstrate getting json API information

# Libraries
library(jsonlite)
library(data.table)

# Options
options(stringsAsFactors = F)
testing <- T

# Search Q
searchQ <- 'spaceX'

# www.newsapi.org Key
apiKey <- 'b6d9f96c78b34ee98bd84a23d3f74bfb'

# Get All news sources
allSources <- fromJSON(paste0('https://newsapi.org/v2/sources?apiKey=',apiKey))

# Construct
if(testing == T){
  maxNum <- 5
} else {
    if(maxNum <- nrow(allSources$sources) < 450){
      maxNum <- maxNum <- nrow(allSources$sources)
    } else {
      maxNum <- 450
    }
  }

topHeadlines <- list()
for(i in 1:maxNum){
  print(paste0('getting: ', allSources$sources$name[i]))
  apiURL <- paste0('https://newsapi.org/v2/top-headlines?q=', 
                   searchQ,'&sources=',allSources$sources$id[i],
                   '&apiKey=',apiKey)
  tops <- fromJSON(apiURL)
  if(tops$totalResults == 0){
    content <- NULL
  } else {
    content <- tops$articles
    content$source <- NULL
  }
  #topHeadlines[[i]] <- content
  safeName <- make.names(allSources$sources$id[i])
  topHeadlines[[safeName]] <- content 
}
x <- topHeadlines
y <- rbindlist(x)

write.csv(y, paste0(searchQ,'_topHeadlines_',Sys.Date(),'.csv'), 
          row.names = F)


# Due to free tier can only get 100
everything <- paste0('https://newsapi.org/v2/everything?q=',
                     searchQ,
                     '&pageSize=100&apiKey=',
                     apiKey)
newsInfo <- fromJSON(everything)




# Organize the API response and save
newsInfo$status           <- NULL
newsInfo$totalResults     <- NULL
newsInfo$articles$source  <- NULL

finalContent <- newsInfo[[1]]
finalContent
#write.csv(finalContent, '~/finalNewsContent.csv', row.names = F)

# End
