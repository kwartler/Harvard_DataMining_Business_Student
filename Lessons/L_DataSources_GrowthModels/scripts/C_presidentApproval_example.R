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

# Original pages
# https://projects.fivethirtyeight.com/trump-approval-ratings/
# https://projects.fivethirtyeight.com/biden-approval-rating/adults/

# Developer Tab has two API endpoints
historicalURL <- 'https://projects.fivethirtyeight.com/biden-approval-rating/historical-approval.json'

presURL <- 'https://projects.fivethirtyeight.com/biden-approval-rating/approval.json'
# Table Info
#tableURL <-  'https://projects.fivethirtyeight.com/biden-approval-rating/polls.json'

# Get Historical
approvalRatings <- fromJSON(historicalURL)
head(approvalRatings)

# Get Biden
presApproval   <- fromJSON(presURL)
head(presApproval)
tail(presApproval)

# Subset to "All polls", and not future predictions & just estimates
subSurvey <- subset(presApproval, 
                    presApproval$subgroup == 'All polls' & 
                      presApproval$future == F)
disapprove <- ts(subSurvey$disapprove_estimate, 
                 start = c(2021, 23), 
                 frequency = 365)
approve    <- ts(subSurvey$approve_estimate, start = c(2021, 23), 
                 frequency = 365)

ratings <- cbind(disapprove, approve)
ratings <- as.zoo(ratings)
ratings <- as.xts(ratings, date_decimal(index(ratings)))

dygraph(ratings, "Biden Approval") %>%
  dySeries("approve", label = "approve", color = 'green') %>%
  dySeries("disapprove", label = "disapprove", color = 'red') %>%
  dyRangeSelector()

# End
