#' Title: Webscraping multiple pages
#' Purpose: Scrape a page for all urls, then scrape each one in order
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 19, 2023
#' 

# Library
library(rvest)
library(stringi)

# Instructor Page
webpage <- 'https://gserm.org/instructors/'

# Get all links
getLinks <- read_html(webpage) %>% html_nodes('*') %>%
  html_attr('href')
getLinks[975:980]

# Extract the relevant instructor URLS;
# Regular Expression, /instructors/ plus any alpha-numeric afterwards
# this excludes three instances on the page of 
# https://gserm.org/instructors/
getLinks <- getLinks[grep('/instructors/*[0-9a-z]', getLinks)]

# Extract and clean names
instructorNames <- lapply(strsplit(getLinks, '/'), tail, 1)
instructorNames
instructorNames <- gsub("-", " ", instructorNames)
instructorNames <- stri_trans_totitle(instructorNames)
instructorNames

# Follow links to get more bio's
allBios <- list()
for (i in 1:length(instructorNames)){
  cat(i)
  # Progress Msg
  cat(instructorNames[i])
  pg <- read_html(getLinks[i])
  
  # Get the bio text
  x <- pg %>% 
    html_nodes(xpath = '/html/body/div[2]/section[2]/div/div/div/div/div') %>% html_text()
  cat('...')
  # Get the photo URL
  y <- pg %>% 
    html_nodes(".size-large") %>% html_attr("src")
  y <- y[!grepl('Logo', y)]
  cat('...')
  df <- data.frame(instructorName = instructorNames[i],
                   bio            = x,
                   photo          = y)
  cat('complete\n')
  allBios[[i]] <- df
}

# Arrange the list to a single data frame
allBios     <- do.call(rbind, allBios)

# Drop the line returns 
allBios$bio <- gsub("[\r\n\t]", "", allBios$bio)
allBios[15,]

# Still some issues need to be cleaned up as is often the case with webscraping.  Lots of cleanup!
allBios[26,]

# End
