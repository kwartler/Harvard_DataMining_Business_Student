#' Title: 538 Website API
#' Purpose: Review  numeric API
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: June 17, 2021
#'

# Libraries
library(jsonlite)
library(xts)
library(zoo)
library(dygraphs)
library(lubridate)

# Original page: there is one for Trump too
# https://projects.fivethirtyeight.com/biden-approval-rating/

# Developer Tab has 3 API endpoints
# Historical Comparisons
historicalURL <- 'https://projects.fivethirtyeight.com/biden-approval-rating/historical-approval.json'

# Current Averages & Forecasts
presURL <- 'https://projects.fivethirtyeight.com/biden-approval-rating/approval.json'

# Invidividual Pollsters
tableURL <-  'https://projects.fivethirtyeight.com/biden-approval-rating/polls.json'

# Get President
presApproval   <- fromJSON(presURL)
head(presApproval)
tail(presApproval)

# Subset to "All polls", and not future predictions & just estimates
subSurvey <- subset(presApproval, 
                    presApproval$subgroup == 'All polls' & 
                      presApproval$future == F)

# Grab the first date
head(subSurvey)

disapprove <- ts(subSurvey$disapprove_estimate, 
                 start     = c(2021, 23), 
                 frequency = 365)
approve    <- ts(subSurvey$approve_estimate, 
                 start     = c(2021, 23), 
                 frequency = 365)

# Data munge both series to an XTS
ratings <- cbind(disapprove, approve)
ratings <- as.zoo(ratings)
ratings <- as.xts(ratings, 
                  date_decimal(index(ratings)))

# Plot
dygraph(ratings, "Biden Approval") %>%
  dySeries("approve", label = "approve", color = 'green') %>%
  dySeries("disapprove", label = "disapprove", color = 'red') %>%
  dyRangeSelector()

# End
