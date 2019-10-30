#' Author: Ted Kwartler
#' Date: 10-30-2019
#' Purpose: Demonstrate getting json API information

# Libraries
library(jsonlite)
library(xts)
library(zoo)
library(dygraphs)
library(lubridate)

# Original page
# https://projects.fivethirtyeight.com/trump-approval-ratings/

# Developer Tab has two API endpoints
historicalURL <- 'https://projects.fivethirtyeight.com/trump-approval-ratings/historical-approval.json'

trumpURL      <- 'https://projects.fivethirtyeight.com/trump-approval-ratings/approval.json'


# Get Historical
approvalRatings <- fromJSON(historicalURL)
head(approvalRatings)

# Get Trump
trumpApproval   <- fromJSON(trumpURL)
head(trumpApproval)
tail(trumpApproval)

# Subset to "All polls", and not future predictions & just estimates
trumpApproval <- subset(trumpApproval, 
                        trumpApproval$subgroup == 'All polls' & 
                          trumpApproval$future == F)
disapprove <- ts(trumpApproval$disapprove_estimate, 
                 start = c(2017, 23), 
                 frequency = 365)
approve    <- ts(trumpApproval$approve_estimate, start = c(2017, 23), frequency = 365)

ratings <- cbind(disapprove, approve)
ratings <- as.zoo(ratings)
ratings <- as.xts(ratings, date_decimal(index(ratings)))

dygraph(ratings, "Trump Approva") %>%
  dySeries("approve", label = "approve", color = 'green') %>%
  dySeries("disapprove", label = "disapprove", color = 'red') %>%
  dyRangeSelector()

# End
