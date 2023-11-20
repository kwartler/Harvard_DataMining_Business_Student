#' Title: https://newsdata.io/pricing
#' Purpose: Get data from JSON
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 19, 2023
#' 

# Library
library(jsonlite)

# www.newsapi.org Key
# apiKey <- 'XXXXXXXX'

# Search term
apiQuery <- URLencode('Harvard University')

# One example API endpoint
pg <- 'https://newsdata.io/api/1/news?apikey=pub_28206c846297a1def49b2298ab44279976513&q=Harvard%20University'

# Recent News Data 
apiResponse <- fromJSON(pg)

# 200 or "success" is what you're looking for with APIs
apiResponse$status

# How many articles were received? Articles are paginated so you have to refer to the next page in the query
apiResponse$totalResults

# Article responses returned in a data frame
str(apiResponse$results)

# One article
apiResponse$results[1,]


# End