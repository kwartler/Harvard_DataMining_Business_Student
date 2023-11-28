# Get some earnings call transcripts

# Transcripts supporting functions 
parse_bubble <- function(bubble) {
  # Extracts speaker, title, and message content from a single speech bubble.
  
  speaker <- bubble %>% html_element(css = '.transcript-line-speaker .font-weight-bold') %>% html_text(trim=TRUE)
  title <- bubble %>% html_element(css = '.transcript-line-speaker .secondary-title') %>% html_text(trim=TRUE)
  paragraphs <- bubble %>% html_elements(css = '.pb-2') %>% html_text(trim=TRUE)
  msg <- paste(paragraphs, collapse = '\n\n')
  
  bubble_df <- data.frame(speaker = speaker, title = title, msg = msg, stringsAsFactors = FALSE)
  
  return(bubble_df)
}

get_transcript_from_url <- function(url) {
  # Given an URL, return a dataframe containing the speech bubbles.
  page <- read_html(url)
  
  speech_bubbles <- page %>% html_elements(css = ".transcript-line-left, .transcript-line-right")
  
  results_list <- lapply(speech_bubbles, parse_bubble)
  
  # Combine the list of dataframes into a single dataframe
  df <- do.call(rbind, results_list)
  
  # Print the resulting dataframe
  return(df)
}

get_btn_from_exchange <- function(ticker, exchange) {
  url <- paste('https://www.marketbeat.com/stocks', exchange, ticker, 'earnings', sep = '/')
  page <- read_html(url)
  
  button <- page %>% html_element(css = '#cphPrimaryContent_cphTabContent_pnlSummary .green-button.w-100')
  
  if(!length(button)){
    print(paste("Warning: Ticker", ticker,"recent transcript not found searching for older doc."))
    button <- page %>% html_element(xpath = '//*[@id="earnings-history"]/tbody/tr[3]/td[9]/a[1]')
  }
  return(button)
}

ticker_to_transcript <- function(ticker) {
  button <- get_btn_from_exchange(ticker, 'NASDAQ')
  
  if (!length(button)) {
    print("Trying NYSE instead...")
    button <- get_btn_from_exchange(ticker, 'NYSE')
  }
  
  if (!length(button)) {
    print(paste("Warning: Ticker", ticker,"not found or no transcript available."))
    
    return(NULL) 
  }
  
  transcript_url <- button %>% html_attr("href")
  
  
  return( get_transcript_from_url(transcript_url) )
}

# Example usage:
#ticker_to_transcript('AAPL') # NASDAQ listed
#ticker_to_transcript('HAL') # NYSE listed
#ticker_to_transcript('TSM') # NYSE listed, but no transcript available
#ticker_to_transcript('COOL') # doesn't exist