#' Author: Ted Kwartler
#' Date: Apr 21-2021
#' Purpose: Complex scraping

# Lib
library(rvest)
library(fst)
library(stringr)

# Query
searchQ <- URLencode('Series A') #machine learning
savePth <- "~/Desktop/Harvard_DataMining_Business_Student/Lessons/L_APIs_webscraping_dashboarding/data/tmp/" # remember the trailing slash!
testing <- T
fileFormat <- 'csv' #or fst

# Initialize
pg <- read_html(paste0('https://www.prnewswire.com/search/news/?keyword=',searchQ,'&page=1&pagesize=100'))
totalResults <- pg %>% html_nodes('.alert-muted') %>% html_text()
totalResults <- lapply(strsplit(totalResults, '\nof '), tail, 1)
totalResults <- as.numeric(gsub('\\D+','', totalResults))

# Total Pages; they stop providing info at 99pgs*100 results  despite having tens of thousands :(
if(testing==T){
  totalPgs <- 5
} else {
  totalPgs <- ifelse(totalResults >9900,9900,totalResults)
}


parsedPR <- list()
for(i in 1:(totalPgs)-1){
  cat(paste('getting:',i, 'of', (totalPgs)-1))
  pg <- read_html(paste0('https://www.prnewswire.com/search/news/?keyword=',searchQ,
                         '&page=',i,'&pagesize=1'))
  prURL <- paste0('https://www.prnewswire.com',
                  pg %>% html_nodes('.news-release')  %>% html_attr("href"))
  prHeadline <- pg %>% html_nodes('.news-release') %>% html_text()
  prCard <- pg %>% html_nodes('.card')   %>% html_nodes("*") %>% 
    html_attr("class") %>% 
    unique()
  cat('...')
  txt <- pg %>% html_nodes('.card') %>% html_text()
  txt <- gsub('\n','',txt)
  txt <- subset(txt, nchar(txt)>0)
  txt <- paste(unique(txt), collapse = ' ')
  namedEntity <- unlist(lapply(strsplit(txt, 'More news about: '),tail, 1))
  ifelse(nchar(namedEntity)==0, ner <- NULL, ner <- namedEntity)
  cat('.sleeping 1 sec\n')
  Sys.sleep(1)
  ifelse(nchar(prURL)==0,       
         prURL <- NULL,       
         prURL <- prURL)
  
  ifelse(nchar(str_squish(prHeadline))==0,  
         prHeadline <- NULL,  
         prHeadline <- str_squish(prHeadline))
  
  ifelse(nchar(str_squish(txt))==0, 
         headlineTxt <- NULL, 
         headlineTxt <- str_squish(txt))
  
  ifelse(nchar(namedEntity)==0, 
         ner <- NULL, 
         ner <- namedEntity)
  
  tmpPR <- data.frame(prURL       = prURL,
                      prHeadline  = prHeadline,
                      headlineTxt = txt, 
                      namedEntity = ner)
  parsedPR[[i+1]] <- tmpPR
}

# Append the entire txt
allInfo <- list()
for(i in 1:length(parsedPR)){
  cat(paste('getting:',i, 'of', length(parsedPR)))
  xURL  <- parsedPR[[i]]$prURL
  charX <- as.character(xURL)
  # Make fault tolerant; should move to the top as a custom function
  tryPage <- function(x){
    pg = NA
    try_error = tryCatch(read_html(charX), error = function(e) e)
    if (!inherits(try_error, 'error'))
      pg = read_html(charX)
    return(pg)
  }
  pg <- tryPage(charX)
  if(!is.na(pg)){
    completeTxt  <- pg %>% html_node('.release-body') %>% html_text()
    completeTxt  <- gsub('\n','',completeTxt)
    completeTxt  <- str_squish(completeTxt)
    timestamp    <- pg %>% html_node('.mb-no') %>% html_text()
    newsProvider <- paste0('https://www.prnewswire.com',
                           pg %>% 
                             html_nodes(xpath = '//*[@id="main"]/article/header/div[3]/div[1]/a') %>%
                             html_attr("href")) 
    resp <- data.frame(source = parsedPR[[i]]$prURL,
                       timestamp,
                       newsProvider = newsProvider,
                       prHeadline = parsedPR[[i]]$prHeadline,
                       headlineTxt = parsedPR[[i]]$headlineTxt,
                       namedEntity = parsedPR[[i]]$namedEntity,
                       completeTxt = completeTxt)
  } else {resp <- data.frame(source = parsedPR[[i]]$prURL,
                       timestamp = 'NA',
                       newsProvider = 'NA',
                       prHeadline = parsedPR[[i]]$prHeadline,
                       headlineTxt = parsedPR[[i]]$headlineTxt,
                       namedEntity = parsedPR[[i]]$namedEntity,
                       completeTxt = 'NA')
  }

  
  
  allInfo[[i]] <- resp
  Sys.sleep(1)
  cat('...sleeping 1 sec\n')
}
allInfo <- do.call(rbind, allInfo)

if(fileFormat=='csv'){
  print('saving as csv')
  nam <- paste0('PRs for ', searchQ, ' at ',make.names(Sys.time()),'.csv')
  write.csv(allInfo, paste0(savePth, nam), row.names = F) 
} else {
  print('saving as fst')
  nam <- paste0('allPRs for term ', searchQ, ' captured ',Sys.time(),'.fst')
  write_fst(allInfo, paste0(savePth, nam)) 
}

# End

