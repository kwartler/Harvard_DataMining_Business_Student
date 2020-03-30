#' Author: Ted Kwartler
#' Date: 9-2-2019
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
CMG <- CMG['2018-01-01/2018-10-26']

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

# Another MA of the difference
manualSig <- SMA(SMAdiff,9)

# Calculate the moving Avgs with a TTR function
CMGmacd <- MACD(CMG$CMG.Close,
                nFast = 12, 
                nSlow = 26, 
                nSig = 9, 
                maType="SMA", #Usually EMA
                percent = F) # Values or Percents

# Examine
tail(CMGmacd,3)
tail(manualSig,3)

# Easier to interpret as a percent
CMGmacdPer <- MACD(CMG$CMG.Close,
                nFast = 12, 
                nSlow = 26, 
                nSig = 9, 
                maType="SMA", 
                percent = T)


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

# Now let's visualize in a stacked dynamic plot
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