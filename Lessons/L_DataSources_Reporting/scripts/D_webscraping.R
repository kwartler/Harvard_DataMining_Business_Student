#' Author: Ted Kwartler
#' Date: 11-30-2020
#' Purpose: Getting data from the web

# libraries
library(rvest)

# Get the webpage
movieURL <- 'https://www.imdb.com/title/tt0058331'
movie <- read_html(movieURL)
movie

# In case you want to get all nodes you could explore
movie %>% 
  html_nodes("*") %>% 
  html_attr("class") %>% 
  unique()

# Numeric info
rating <- movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
rating

# Get the cast names
cast <- movie %>%
  html_nodes("#titleCast") %>% 
  html_nodes("tr") %>%
  html_text()
cast

# Webscraping is messy!
cast  <- gsub("[\r\n]", "", cast)
cast  <- lapply(cast, trimws)
cast  <- unlist(cast)
cast2 <- strsplit(cast, '[...]')
part  <- lapply(cast2, tail, 1) %>% unlist() %>% trimws()
actor <- lapply(cast2, head, 1) %>% unlist() %>% trimws()

df <- data.frame(part, actor)
df <- df[-1,]
df

# What is the URL for the movie poster?
poster <- movie %>%
  html_nodes(".poster img") %>%
  html_attr("src")
poster

# Storyline
storyline <- movie %>%
  html_nodes('#titleStoryLine') %>%
  html_text() 
storyline <- gsub("[\r\n]", "", storyline)
storyline <- strsplit(storyline, 'Written by')[[1]][1]
storyline

# More mess!
Moviegross <- movie %>%
  html_nodes('#titleDetails') %>% html_text()
Moviegross

Moviegross <- strsplit(Moviegross, 'Cumulative Worldwide Gross')[[1]][2]
Moviegross <- substr(Moviegross, 3,14)
Moviegross

# uggh!
as.numeric(gsub('[$]|,','',Moviegross))

# End