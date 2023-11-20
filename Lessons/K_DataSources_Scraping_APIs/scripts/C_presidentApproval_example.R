#' Title: 538 Website API
#' Purpose: Review  numeric API
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 19, 2023
#'

# Libraries
library(jsonlite)
library(xts)
library(zoo)
library(dygraphs)
library(lubridate)

# Original pages
# https://projects.fivethirtyeight.com/biden-approval-rating/adults/

# Developer Tab has 3 API endpoints
#historicalURL <- 'https://projects.fivethirtyeight.com/biden-approval-rating/historical-approval.json'
presURL       <- 'https://projects.fivethirtyeight.com/biden-approval-rating/approval.json'
#tableURL      <- 'https://projects.fivethirtyeight.com/biden-approval-rating/polls.json'

# Biden Approval
presApproval   <- fromJSON(presURL)
head(presApproval)
tail(presApproval)

# Enforce the Date class & reorder
presApproval$date <- as.Date(presApproval$date)
presApproval <- presApproval[order(presApproval$date),]
head(presApproval)

# Separate the approve and disapprove
disapprove <- ts(presApproval$disapprove_estimate, 
                 start = c(2021, 23), 
                 frequency = 365)
approve    <- ts(presApproval$approve_estimate, start = c(2021, 23), 
                 frequency = 365)

ratings <- cbind(disapprove, approve)
ratings <- as.zoo(ratings)
ratings <- as.xts(ratings, date_decimal(index(ratings)))

dygraph(ratings, "Biden Approval") %>%
  dySeries("approve", label = "approve", color = 'green') %>%
  dySeries("disapprove", label = "disapprove", color = 'magenta') %>%
  dyRangeSelector()

# End