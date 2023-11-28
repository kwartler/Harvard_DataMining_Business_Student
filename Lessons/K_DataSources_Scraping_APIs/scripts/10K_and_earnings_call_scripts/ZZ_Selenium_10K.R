# Need to make it performant but for now just run through it
# The result is a webpage so use rvest read_html() on each result
# Target
#https://www.sec.gov/edgar/search/#/dateRange=custom&entityName=AAPL&startdt=2022-01-01&enddt=2023-07-01&filter_forms=10-K


# Define the function to get the report
# symbol can be ticker or CIK code but would need some adjustment due to padding, for company CIK, can be less than 10 0s, leading 0s are added
# start_at  #query parameter
# end_at #query parameter
# savePth #where to save this, needs to be non-null even if not saving with save10k
# save10k Boolean to enable saving
# binaryDownloadChk # checks latest binaries every rsDriver call, should be adjusted from time to time
getRecent10KReport <- function(symbol='ALB', #'0000320193' SEC company code works too 
                               start_at='2022-01-01',
                               end_at='2023-07-01', 
                               savePth = '~/Desktop/HBS_execEDU/tmp/', 
                               save10k = F,
                               binaryDownloadChk  = F) { 
  # Libs
  library(RSelenium)
  library(rvest)
  library(stringr)
  
  
  
  # Start a new driver
  driver <- rsDriver(port= sample(7600)[1], browser=c("firefox"), chromever = NULL, verbose = F,
                     check = binaryDownloadChk)
  #driver$client$setImplicitWaitTimeout(20000) # set the wait time since the SEC is SSSSOOOO slow!
  
  # Go to the SEC Edgar search page for the specified symbol and date range
  baseURL  <- 'https://www.sec.gov/edgar/search/#/dateRange=custom&entityName='
  edgarURL <- paste0(paste0(baseURL, symbol, "&startdt=", start_at, "&enddt=", end_at, "&filter_forms=10-K"))
  driver$client$navigate(edgarURL)
  
  
  
  # Wait for the pages to load function; adds another 10 seconds of wait time per call
  waitForPageToLoad <- function(driver){
    for(i in 1:10){
      Sys.sleep(1)
      page_state <-  driver$executeScript('return document.readyState')
      if(page_state == "complete" ){
        break
      }
    }
  }
  waitForPageToLoad(driver$client)
  
  # Find the table of reports on the search results page
  trs <- driver$client$findElements(using = "xpath", "/html/body/div[3]/div[2]/div[2]/table/tbody/tr")
  if(length(trs)==0){
    driver$server$stop()
    stop('No search results were found, could be the ticker, or the date range did not have a 10k filing.')
  }
  
  # Get the most recent report
  title <- trs[[1]]$findElement(using = "xpath", '//*[@id="hits"]/table/tbody/tr[1]/td[4]')
  title <- title$getElementText()
  
  # Find the modal click
  click <- trs[[1]]$findChildElement(using = 'xpath', '//*[@id="hits"]/table/tbody/tr[1]/td[1]/a')
  click$clickElement()
  waitForPageToLoad(driver$client)
  
  # Get the link of the file to download
  link <- driver$client$findElement(using = 'id' , "open-file")
  link <- link$getElementAttribute('href')
  
  print(paste('working on', title))
  
  # Navigate to the report
  pg <- driver$client$navigate(link[[1]])
  waitForPageToLoad(driver$client)
  
  # Get the page source (HTML content) of the report
  pgSrc <- driver$client$getPageSource(pg)
  
  # Dynamic create file name
  title <- gsub('/','',title)
  nam <- gsub('[.]|[(]|[)]','', title)
  nam <- gsub(' ','_', nam)
  nam <- paste0(nam,'_.html')
  
  # Create path 
  fullName <- paste0(savePth, nam)
  
  if(save10k==T){
    # Save File
    writeLines(pgSrc[[1]], fullName)
    
  }
  # Stop the driver and close the browser
  driver$server$stop()
  return(pgSrc[[1]])
}

# Now make it robust
tryToGetRecent10KReport <- function(symbol='ALB', start_at='2022-01-01', end_at='2023-07-01', 
                                    savePth = '~/Desktop/HBS_execEDU/tmp/', save10k = F, 
                                    maxRetries = 3, binaryDownloadChk = F) {
  
  for(i in 1:maxRetries) {
    result <- tryCatch({
      getRecent10KReport(symbol, start_at, end_at, savePth, save10k, binaryDownloadChk)
    }, 
    error = function(e) {
      message(sprintf("Attempt %d: Error occurred while trying to get the 10K report: %s", i, e))
      NULL
    },
    warning = function(w) {
      message("Warning: ", w)
      NULL
    })
    
    if(!is.null(result)) {
      return(result)
    }
  }
  
  message(sprintf("Failed to get the 10K report after %d attempts", maxRetries))
  
  return(NULL)
}

# Test; SEC website has issues with consistent loading times.
# Performant
#x <- tryToGetRecent10KReport("ALB",  start_at='2022-01-01', end_at='2023-07-01')
# Singleton
#y <- getRecent10KReport('WRB', start_at='2022-01-01', end_at='2023-07-01')
# Applied to a vector of stocks
#z <- lapply(c('GOOG', 'AMZN','NVDA'),tryToGetRecent10KReport)
