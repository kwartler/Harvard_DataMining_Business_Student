#' Title: Grab another website API JSON
#' Purpose: Demonstrate f12 in Chrome for API
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Apr 25, 2022
#'

# this is the original site
#https://covacglobal.com/

# Scrolling news ticker are supplied by riskline.com
#https://api.riskline.com/alerts/latest.json


# Simple GET request
library(jsonlite)
x <- fromJSON('https://api.riskline.com/alerts/latest.json')
x$alerts$title
x$alerts$created_at
x$alerts$country

# Simple organization
x <- do.call(cbind, x)

# End
