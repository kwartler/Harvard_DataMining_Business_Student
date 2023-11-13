#' Author: Ted Kwartler
#' Date: Oct 21, 2022
#' Purpose: MACD Example As Indicator
#'

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(htmltools)

# Get Chipotle
getSymbols("CMG") #"CMG_1_TTR_D.rds"
CMG <- CMG['2018-01-01/2019-01-01']

# Manual MACD
# FAST MA
CMGsma12 <- SMA(CMG$CMG.Close, 12)
tail(CMGsma12, 5) 

# SLOW MA
CMGsma26 <- SMA(CMG$CMG.Close, 26)
tail(CMGsma26, 5) 

# MA Difference
SMAdiff <- CMGsma12 - CMGsma26
tail(SMAdiff, 5) 
tail(CMGsma12, 1) - tail(CMGsma26, 1) #same as above

# 3rd Moving Avg of the difference between the two
manualSig <- SMA(SMAdiff,9)

# Calculate the moving Avgs with a TTR function
CMGmacd <- MACD(CMG$CMG.Close,
                nFast = 12, 
                nSlow = 26, 
                nSig = 9, 
                maType="SMA", #Usually EMA
                percent = F) # Values or Percents

# Examine to ensure the underlying math is understood
data.frame(manualSignal = as.vector(tail(manualSig,5)),
           ttrSignal    = tail(CMGmacd$signal, 5),
           manualMACD   = as.vector(tail(SMAdiff, 5)),
           ttrMACD      = tail(CMGmacd$macd, 5))

# For some it's easier to interpret as a percent of share price
CMGmacdPer <- MACD(CMG$CMG.Close,
                nFast = 12, 
                nSlow = 26, 
                nSig = 9, 
                maType="SMA", 
                percent = T)
tail(CMGmacdPer)

# As a trading indicator
signal <- Lag(ifelse(CMGmacdPer$macd > CMGmacdPer$signal, 1, 0))

# Quick Check
table(signal)

CMG <- merge(CMG$CMG.Close, CMGmacdPer)
CMG$MACDindicator <- CMG$macd - CMG$signal

# Now let's visualize in a stacked dynamic plot
browsable(
  tagList(
    dygraph(CMG$CMG.Close, 
            group = "Price", 
            height = 200, 
            width = "100%"),
    dygraph(CMGmacdPer,
            group = "Price", 
            height = 200, 
            width = "100%") %>%
      dySeries('macd',label='MACD') %>%
      dySeries('signal',label='SIGNAL') %>%
      dyRangeSelector()
  )
)

# Now let's visualize in a stacked dynamic plot; anytime signal value is positive buy
browsable(
  tagList(
    dygraph(CMG$CMG.Close, 
            group = "Price", 
            height = 200, 
            width = "100%"),
    dygraph(CMG$MACDindicator,
            group = "Price", 
            height = 200, 
            width = "100%") %>%
      dyRangeSelector()
  )
)

# End