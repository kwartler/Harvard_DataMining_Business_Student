#' Title: Webscraping a single page
#' Purpose: Scrape a single page example
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Apr 25, 2022
#'

# libraries
library(rvest)

# Get the webpage
movieURL <- 'https://www.imdb.com/title/tt0058331'
movie <- read_html(movieURL)
movie

# Numeric info
rating <- movie %>% 
  html_nodes(xpath = '/html/body/div[2]/main/div/section[1]/section/div[3]/section/section/div[3]/div[2]/div[1]/div[2]/div/div[1]/a/div/div/div[2]/div[1]/span[1]') %>%
  html_text() %>%
  as.numeric()
rating

# https://www.imdb.com/title/tt0058331/fullcredits
castURL <- paste0(movieURL,'/fullcredits')

# _somtimes_ helpful to see all nodes
castURL %>%
  read_html() %>%
  html_nodes("*") %>% 
  html_attr("class") %>% 
  unique()

# Get the cast names
cast <- castURL %>%
  read_html() %>%
  html_nodes('#fullcredits_content') %>%
  html_nodes("tr") %>%
  html_text()
cast

# Webscraping can be messy!
cast  <- gsub("[\r\n]", "", cast)
cast  <- lapply(cast, trimws)
cast  <- unlist(cast)
cast2 <- strsplit(cast, '[...]')
part  <- lapply(cast2, tail, 1) %>% unlist() %>% trimws()
actor <- lapply(cast2, head, 1) %>% unlist() %>% trimws()

df <- data.frame(part, actor)
head(df,12) #looks like first 5 rows are wrong
df <- df[-(1:5),]
head(df,12)

# What is the URL for the movie poster?
allURLS <- movie %>% html_nodes("a") %>% html_attr("href")
allURLS
mediaURLS <- allURLS[ grep('mediaviewer',allURLS, ignore.case = T)]
mediaURLS[1]
movieURL
postURL <- paste0(movieURL,gsub('/title/tt0058331','',mediaURLS[1]))
postURL

# Storyline
storyline <- movie %>%
  html_nodes(xpath = '/html/body/div[2]/main/div/section[1]/div/section/div/div[1]/section[6]/div[2]/div[1]/div[1]/div/text()') %>%
  html_text() 
storyline

# More mess!
movieGross <- movie %>%
  html_nodes(xpath = '/html/body/div[2]/main/div/section[1]/div/section/div/div[1]/section[10]/div[2]/ul/li[2]') %>% html_text()
movieGross

movieGross <- gsub('Gross US & Canada|[$]|,','',movieGross)
movieGross <- as.numeric(movieGross)
movieGross
# End