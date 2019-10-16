#' Author: Ted Kwartler
#' Date: Oct-2019
#' Purpose: Stock API request & Manipulate a Time Series Object
#' 

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(dygraphs)
library(htmltools)

# Get list of all stocks available
#allTickers <- stockSymbols("NASDAQ") #AMEX, NASDAQ, NYSE
#idx        <- grep('AAPL', allTickers$Symbol)
#allTickers[idx,]

## Get historical stock pricing
getSymbols("AAPL", src = "yahoo") #"AAPL_1_TTR_A.rds"
#RobjectName <- getSymbols('AAPL', auto.assign=F)

# Review
head(AAPL)

# Quick Viz
barChart(AAPL) 

## Subsetting XTS
# Get a complete yr
oneYr <- AAPL["2017"] 
head(oneYr)
tail(oneYr)

# Extract data from Jan 2017 to May 2018 
yrToMonth <- AAPL["2017/2018-05"]
head(yrToMonth)
tail(yrToMonth)

#Get Jan3 to June 21 2018
dateToDate <- AAPL["2018-01-03/2018-06-21"] 
head(dateToDate)
tail(dateToDate)

#Get all data until Dec 2016 
upUntil <- AAPL["/2016-12"] 
head(upUntil)
tail(upUntil)

# Extracting specific columns & simple deltas
# Closing column only
head(Cl(AAPL),5)

# Opening price column
head(Op(AAPL),5)

# High price for the session
head(Hi(AAPL),5)

# Low price for the session
head(Lo(AAPL),5)

# Volume for the session
head(Vo(AAPL),5)

# Simple detlas
head(OpCl(AAPL),5)
?Cl

# D3 Viz
dygraph(AAPL$AAPL.Close)  %>% dyRangeSelector()

candleAAPL <- AAPL[,1:4]
dygraph(candleAAPL) %>%
  dyCandlestick() %>% dyRangeSelector()

# End