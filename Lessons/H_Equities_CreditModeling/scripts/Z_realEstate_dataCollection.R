#' Author: Ted Kwartler
#' Date: Nov 6 2022
#' Purpose: Real Estate Data Collection
#' 
library(rvest)
library(readr)
library(dplyr)
options(scipen = 999)

# Top level page from the map view
searchPG <- 'https://www.redfin.com/city/36147/MA/Orleans/filter/min-beds=1,viewport=42.0719:41.62281:-69.8603:-70.58814,no-outline'

# How many total pages?
totPg <- searchPG %>% read_html() %>% html_nodes(xpath = '/html/body/div[1]/div[7]/div[2]/div[2]/div[4]/div/div/div[2]/div/span') %>% html_text()
totPg
totalPgs <- as.numeric(trimws(unlist(lapply(strsplit(totPg, 'of'), tail, 1))))
totalPgs

# Obtain the downloadable CSV **MAX seems to be 350**?!  Looks like default is to limit to last 40 days?
downloadAll <- searchPG %>% 
  read_html() %>% 
  html_nodes(xpath = '/html/body/div[1]/div[7]/div[2]/div[2]/div[4]/div/div/div[2]/div/a') %>% 
  html_attr('href')
downloadAll
downloadAll    <- paste0('https://www.redfin.com',downloadAll)
allSearchProps <- read_csv(downloadAll) %>% as.data.frame()

allProps <- list()
for(i in 1:nrow(allSearchProps)){
  print(i)
  singleProp <- allSearchProps$`URL (SEE https://www.redfin.com/buy-a-home/comparative-market-analysis FOR INFO ON PRICING)`[i] %>% read_html()
  propLst <- list(
    price = singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[1]/div/div[1]/div/div[1]/div/div/div/div/div/div[1]/div') %>% 
      html_text(),
    beds = singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[1]/div/div[1]/div/div[1]/div/div/div/div/div/div[2]/div') %>% 
      html_text(),
    bath = singleProp %>% 
      html_nodes(xpath = '//*[@id="content"]/div[10]/div[2]/div[1]/div/div[1]/div/div[1]/div/div/div/div/div/div[3]/div') %>% 
      html_text(),
    sqFt = singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[1]/div/div[1]/div/div[1]/div/div/div/div/div/div[4]/span') %>% 
      html_text(),
    addr = singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[1]/div/div[1]/div/div[1]/div/div/div/div/header/div/h1') %>% 
      html_text(),
    redfinEst = singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[7]/section/div/div/div/div/div/div[6]/div[3]/span[2]')%>% 
      html_text(),
    monthPayment = singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[7]/section/div/div/div/div/div/div[6]/div[2]/span[2]') %>% 
      html_text(),
    pricePerSqFt =  singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[7]/section/div/div/div/div/div/div[6]/div[4]/span[2]') %>% 
      html_text(),
    lotSize =  singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[7]/section/div/div/div/div/div/div[4]/div[7]/span[2]') %>% 
      html_text(),
    style =  singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[7]/section/div/div/div/div/div/div[4]/div[5]/span[2]') %>% 
      html_text(),
    propType = singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[7]/section/div/div/div/div/div/div[4]/div[3]/span[2]') %>% 
      html_text(),
    yrBuilt = singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[7]/section/div/div/div/div/div/div[4]/div[4]/span[2]') %>% 
      html_text(),
    daysListed = singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[7]/section/div/div/div/div/div/div[4]/div[2]/span[2]') %>% 
      html_text(),
    community = singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[7]/section/div/div/div/div/div/div[4]/div[6]/span[2]') %>% 
      html_text(),
    mlsNum = singleProp %>% 
      html_nodes(xpath = '/html/body/div[1]/div[10]/div[2]/div[7]/section/div/div/div/div/div/div[4]/div[8]/span[2]') %>% 
      html_text()
  )
  # Chk for missing return NA
  propLst       <- lapply(propLst, function(x) if(identical(x, character(0))) NA_character_ else x)
  propLst       <- do.call(cbind, propLst)
  allProps[[i]] <- propLst
}

allProps <- do.call(rbind, allProps)
allProps <- as.data.frame(allProps)
head(allProps)

# Combine
allPropsScrape <- cbind(allSearchProps, allProps)

# Clean up
allPropsScrape$SOLD.DATE <- NULL
names(allPropsScrape)[grep('URL', names(allPropsScrape), ignore.case = T)] <- 'URL'
allPropsScrape$NEXT.OPEN.HOUSE.START.TIME<- NULL
allPropsScrape$NEXT.OPEN.HOUSE.END.TIME<- NULL
allPropsScrape$SALE.TYPE <- NULL #univariate
allPropsScrape$STATUS <- NULL #univariate
allPropsScrape$FAVORITE<- NULL #univariate
allPropsScrape$INTERESTED<- NULL #univariate
allPropsScrape$pricePerSqFt <- allPropsScrape$PRICE / allPropsScrape$SQUARE.FEET # remake w/official data
allPropsScrape$sqFt <- NULL #dupe
allPropsScrape$lotSize <- NULL #dupe
allPropsScrape$beds<- NULL #dupe
allPropsScrape$bath<- NULL #dupe
allPropsScrape$addr<- NULL #dupe
allPropsScrape$yrBuilt <- NULL #dupe
allPropsScrape$propType <- NULL #dupe
allPropsScrape$community<- NULL #dupe
allPropsScrape$price<- NULL #dupe
#allPropsScrape$pricePerSqFt <- NULL #dupe
allPropsScrape$daysListed<- NULL #dupe
allPropsScrape$mlsNum<- NULL #dupe
allPropsScrape$X..SQUARE.FEET <- NULL

# Fix
allPropsScrape$redfinEst <- as.numeric(gsub('[$]|,','',allPropsScrape$redfinEst))
allPropsScrape$monthPayment <- as.numeric(gsub('[$]|,','',allPropsScrape$monthPayment))

# Rename
names(allPropsScrape) <- c('PROPERTY.TYPE', 'ADDRESS','CITY','STATE','ZIP','PRICE','BEDS','BATHS','Community','SQUARE.FEET',
                           'LOT.SIZE','YEAR.BUILT','DAYS.ON.MARKET', 'HOA.MONTH', 'URL', 'SOURCE', 'mlsNum','LAT','LON',
                           'redfinEst','monthPayment','style','pricePerSqFt')

# Impute scrape data issue
plot(density(allPropsScrape$redfinEst))
hist(allPropsScrape$redfinEst)
allPropsScrape$redfinEst <- ifelse(nchar(allPropsScrape$redfinEst)<=4,median(allPropsScrape$redfinEst),allPropsScrape$redfinEst)

# Reorder
allPropsScrape <- allPropsScrape %>%
  select(-PRICE,mlsNum,  URL, ADDRESS,CITY, STATE,  ZIP, LAT, LON,
         everything())

write.csv(allPropsScrape, 'capeCod_Nov2022.csv', row.names = F)

# End