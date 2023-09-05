#' Title: Webscraping a single page
#' Purpose: Scrape a single page example
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Nov 27, 2022
#'

#### Reminder restart R because of readxl conflicts

# libraries
library(rvest)
library(jsonlite)

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

# Get the cast on the follow-up page; this returns all nodes so you can explore.  It's sometimes helpful
castURL <- paste0(movieURL,'/fullcredits')
# https://www.imdb.com/title/tt0058331/fullcredits
castURL %>%
  read_html() %>%
  html_nodes("*") %>% 
  html_attr("class") %>% 
  unique()

# Found the right one! fullcredits_content: Get the cast names
cast <- castURL %>%
  read_html() %>%
  html_nodes('#fullcredits_content') %>%
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
head(df,12) #looks like first 5 rows are wrong
df <- df[-(1:5),]
head(df,12)

# What is the URL for the movie poster?
allURLS <- movie %>% html_nodes("a") %>% html_attr("href")
allURLS
mediaURLS <- allURLS[ grep('mediaviewer',allURLS, ignore.case = T)]
mediaURLS[1]
movieURL
gsub('/title/tt0058331','',mediaURLS[1])
postURL <- paste0(movieURL,gsub('/title/tt0058331','',mediaURLS[1]))
justJPG <- postURL %>% read_html() %>% html_nodes(xpath = '/html/body/div[2]/main/div[2]/div[3]/div[4]/img') %>% html_attr("src")
justJPG
# download the file to the working directory
#download.file(justJPG,'downloadedImage.jpg', mode = 'wb')

# Storyline; this was moved to a graphDB and you could use rSelenium but this is faster for this exercise 
# Get it using "inspect" and "copy response" then saving as json
storyLine <- fromJSON('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/L_DataSources_GrowthModels/data/maryPoppins_storyline.json')

storyLine$data$title$summaries$edges$node$plotText$plaidHtml
storyLine$data$title$synopses$edges$node$plotText$plaidHtml

# End