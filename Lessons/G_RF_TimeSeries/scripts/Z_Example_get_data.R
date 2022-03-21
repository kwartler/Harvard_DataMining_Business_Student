#' Author: Ted Kwartler
#' Data: Mar 21 2022
#' Purpose: Example get table data of time series

# Options
options(scipen=999)

# Libary
library(rvest)

# URL
stkURL <- 'https://ycharts.com/companies/AMZN/revenues'

# Left Hand Table
y <- stkURL %>% read_html() %>% html_nodes(xpath = '/html/body/main/div/div[4]/div/div/div/div[3]/div[1]/div/div[2]/div[2]/div[1]/table') %>% html_table()

# Right Hand Table
z <- stkURL %>% read_html() %>% html_nodes(xpath = '/html/body/main/div/div[4]/div/div/div/div[3]/div[1]/div/div[2]/div[2]/div[2]/table') %>% html_table()

# Examine
y[[1]]
z[[1]]

## Next steps is to perform data clean up, change date column with lubridate functions.  Use gsub & as.numeric to clean up Value column. Then combine and sort by date before saving.

# End
