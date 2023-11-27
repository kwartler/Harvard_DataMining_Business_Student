#' Title: Webscraping a single page
#' Purpose: Scrape a single page example
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 19, 2023
#'

# webpage to webscrape
pg <- 'https://rvest.tidyverse.org/articles/starwars.html'

# You can get one section with the explicit xpath code
firstSection <- read_html(pg) %>% html_nodes(xpath = '//*[@id="main"]/section[1]') %>% html_text()
cat(firstSection)

# If the CSS (cascading style sheet) uses names entities you can get them by name
allSections <- read_html(pg) %>% html_nodes('section')
allSections #notice the h2 header, lets get those!

# Movie names
movieNames <- allSections %>% html_node('h2') %>% html_text(trim = TRUE)
movieNames

# One release date example
read_html(pg) %>% html_nodes(xpath = '//*[@id="main"]/section[1]/p[1]')

# Needed gpt for this :)
# Since every section has a h2 name and the release date is next 
# you have to first search for the h2 header and get the very next node.
releaseDates <- read_html(pg) %>% html_nodes(xpath = '//h2/following-sibling::p[1]') %>% html_text(trim = TRUE)
releaseDates

# Similarly now to get all the text for each section
# //section is all sections
# //p is all paragraphs
# positions() > 1 means all paragraphs after the first because that's the release date
movieInfo <- read_html(pg) %>% html_nodes(xpath = '//section//p[position() > 1]') %>% html_text(trim = TRUE)

# Needs some cleanup
length(movieInfo)
length(releaseDates)

# Since each section has "Director" we can do organize the section text
idx <- cumsum(grepl("Director:", movieInfo)) # Get a running total of the split word
idx #these are now the section groups!
movieInfo <- data.frame(id = idx, text  = movieInfo)
movieInfo[1:2,]
movieInfo <- aggregate(text ~id, movieInfo, paste, collapse = ' ')
movieInfo[1,]

# Final organization
finalDF <- data.frame(name = movieNames, 
                      releaseDate = releaseDates,
                      movieInfo = movieInfo$text)
finalDF[1,]


# Odds & Ends that have helped me
# Get all unique named css nodes
read_html(pg) %>% html_nodes("*") %>% 
  html_attr("class") %>% 
  unique()

# Get all urls
read_html(pg) %>% html_nodes("a") %>% 
  html_attr("href") 

# Get a table of information
# https://en.wikipedia.org/wiki/List_of_S%26P_500_companies
sp500 <- read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>%
  html_nodes('#constituents') %>% html_table() %>% as.data.frame()
head(sp500)


# A bit more complex way of doing it if you're comfortable with a list
movieInfo2 <- read_html(pg) %>% 
  html_nodes(xpath = '//section') %>% # look for each section & then collapse the paragraphs
  lapply(function(section) { #make a list and collapse each one
    section %>% 
      html_nodes(xpath = './/p[position()>1]') %>% 
      html_text(trim = TRUE) %>% 
      paste(collapse = ' ')
  })
movieInfo2 <- unlist(movieInfo2)
movieInfo2

# End