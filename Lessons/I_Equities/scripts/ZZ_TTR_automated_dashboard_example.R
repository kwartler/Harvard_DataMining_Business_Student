#' Author: Ted Kwartler
#' Data: Mar 28, 2022
#' Purpose: Construct a financial dashboard and make a buy/sell recommendation

# Function Parameters:
# tickerSymbol - a stock ticker in quotes; default is Apple as "AAPL"
# lookBack - an integer greater than 34 (for MACD to work), the number of historical days you want to review; default is 365
# kpi - a financial indicator inside quotes.  Options include "macd", "sma", "rsi"; default is 'macd'
# recommendation - a string you enter inside quotes stating based on the indicator whether it says to buy or sell the stock


stockChk <- function(tickerSymbol='AAPL', 
                     lookBack =365, 
                     kpi = 'macd', 
                     recommendation = 'NOT ENTERED'){
  
  # Libs
  require(TTR)
  require(quantmod)
  require(dygraphs)
  require(htmltools)
  require(htmlwidgets)
  
  # Data & Subset
  print(paste('looking up your tickerSymbol:',tickerSymbol))
  x <- getSymbols(tickerSymbol, auto.assign=FALSE)
  x<- tail(x, lookBack)
  
  # Find the closing price column
  closeIdx <- grep('Close', names(x))
  
  # Calc indicators
  if(kpi == 'macd'){
    xKPI <- MACD(x[,closeIdx],
                 nFast = 12, nSlow = 26, nSig = 9, 
                 maType="SMA", percent = T)
  }
  if(kpi == 'rsi'){
    xKPI <-RSI(x[,closeIdx], maType="SMA", n =14)
  }
  if(kpi =='sma'){
    xKPI <-SMA(x[,closeIdx], 50)
    yKPI <-SMA(x[,closeIdx], 200)
  }
  
  # Visual Titles
  closingTitle <- paste('Daily Closing Price of ', tickerSymbol)
  candleTitle <- paste('Candlestick of ', tickerSymbol)
  kpiTitle <- paste(kpi, 'for', tickerSymbol, 'says to ',recommendation)
  
  # Construct kpi visuals
  if(kpi=='sma'){
    smaData <- cbind(x[,closeIdx],xKPI, yKPI) 
    kpiPlot <- dygraph(smaData, 
                       group = "Price", height = 200, 
                       width = "100%", main = kpiTitle) %>%
      dyRangeSelector()
  }
  if(kpi=='rsi'){
    kpiPlot <- dygraph(xKPI, 
                       group = "Price", height = 200, 
                       width = "100%", main = kpiTitle) %>%
      dyLimit(30, label = 'OverSold') %>%
      dyLimit(70, label = 'OverBought') %>%
      dyRangeSelector()
  }
  if(kpi=='macd'){
    kpiPlot <- dygraph(xKPI,
                       group = "Price", height = 200, 
                       width = "100%", main = kpiTitle) %>%
      dySeries('macd',label='MACD') %>%
      dySeries('signal',label='SIGNAL') %>%
      dyRangeSelector()
  }
  
  dashboard <- browsable(
    tagList(
      dygraph(x[,closeIdx], 
              group = "Price",height = 200, 
              width = "100%", main = closingTitle),
      dygraph(x[,1:4],
              group = "Price",height = 200, 
              width = "100%", main = candleTitle) %>%
        dyCandlestick(),kpiPlot))
  
  return(dashboard)
}

# Examples
#stockChk()
#stockChk('CMG', kpi = 'rsi', lookBack = 365)
#stockChk('CMG', kpi= 'sma', lookBack = 365)
#stockChk('CMG', kpi='macd', lookBack = 365, recommendation='buy')
stockChk('CRM', kpi = 'macd', lookBack = 365, recommendation='buy')

# End