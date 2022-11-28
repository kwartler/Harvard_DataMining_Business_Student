#' TK
#' Purpose Get any press release topic historically
#' June 16, 2022

# Lib
library(rvest)
library(fst)
library(stringr)

# Query
searchQ <- URLencode('machine learning')
savePth <- '~/Desktop/Harvard_DataMining_Business_Student/personalFiles/tmp/'# # remember the trailing slash!
testing <- T
fileFormat <- 'csv' #csv or fst

# Initialize
pg <- read_html(paste0('https://www.prnewswire.com/search/news/?keyword=',searchQ,'&page=1&pagesize=100'))
totalResults <- pg %>% html_nodes('.alert-muted') %>% html_text()
totalResults <- lapply(strsplit(totalResults, '\nof '), tail, 1)
totalResults <- as.numeric(gsub('\\D+','', totalResults))

# Total Pages; they stop providing info at 99pgs*100 results  despite having tens of thousands :(
if(testing==T){
  totalPgs <- 15
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
  #parsedPR[[i]] <- data.frame(prURL       = prURL,
  ifelse(nchar(prURL)==0,       prURL <- NULL,       prURL <- prURL)
  ifelse(nchar(str_squish(prHeadline))==0,  prHeadline <- NULL,  prHeadline <- str_squish(prHeadline))
  ifelse(nchar(str_squish(txt))==0, headlineTxt <- NULL, headlineTxt <- str_squish(txt))
  ifelse(nchar(namedEntity)==0, ner <- NULL, ner <- namedEntity)
  
  parsedPR <- data.frame(prURL       = prURL,
                         prHeadline  = str_squish(prHeadline),
                         headlineTxt = str_squish(txt), 
                         namedEntity = ner)
  write.csv(parsedPR,paste0(savePth, i,'_',paste0(Sys.time(), '.csv')), row.names = F)
}

# Append the entire txt
tmp <- list.files(path = savePth, pattern = '*.csv',full.names = T)
allInfo <- list()
for(i in 1:length(tmp)){
  cat(paste('getting:',i, 'of', length(tmp)))
  x <- read.csv(tmp[i])
  pg <- read_html(as.character(x$prURL))
  completeTxt  <- pg %>% html_node('.release-body') %>% html_text()
  completeTxt  <- gsub('\n','',completeTxt)
  completeTxt  <- str_squish(completeTxt)
  timestamp    <- pg %>% html_node('.mb-no') %>% html_text()
  newsProvider <- pg%>% html_nodes('.swaping-class-left') %>% html_text()
  
  resp <- data.frame(source = x$prURL,
                     timestamp,
                     newsProvider,
                     x$prHeadline,
                     x$headlineTxt,
                     x$namedEntity,
                     completeTxt)
  allInfo[[i]] <- resp
  cat('...sleeping 1 sec\n')
  Sys.sleep(1)
  write_fst(resp, make.names(paste0(Sys.time(),'.fst')))
}
allInfo <- do.call(rbind, allInfo)
names(allInfo) <- make.names(names(allInfo))
#nam <- paste0('allPRs for term ', searchQ, ' captured ',Sys.time(),'.fst')
#write_fst(allInfo, paste0(savePth, nam)) # could also use fst for large data
#write.csv(allInfo, 'test.csv', row.names = F)

# Examine
allInfo$source[1]
allInfo$timestamp[1]
allInfo$newsProvider[1]
allInfo$x.prHeadline[1]
allInfo$x.headlineTxt[1]
allInfo$x.namedEntity[1]
allInfo$completeTxt[1]

if(fileFormat=='csv'){
  print('saving as csv')
  nam <- paste0('allPRs for term ', searchQ, ' captured ',Sys.time(),'.csv')
  write.csv(allInfo, paste0(savePth, nam), row.names = F) 
} else {
  print('saving as fst')
  nam <- paste0('allPRs for term ', searchQ, ' captured ',Sys.time(),'.fst')
  write_fst(allInfo, paste0(savePth, nam)) 
}

# End
