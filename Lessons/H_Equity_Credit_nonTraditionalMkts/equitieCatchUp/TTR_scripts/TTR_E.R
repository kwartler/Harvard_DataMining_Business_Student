#' Author: Ted Kwartler
#' Date: 9-2-2019
#' Purpose: RSI Example
#' 

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(htmltools)

# Get AMZN
getSymbols("AMZN") #"AMZN_1_TTR_E.rds"
AMZN <- AMZN['2018-01-01/2018-06-22']

# Calc RSI
AMZNrsi <- RSI(AMZN$AMZN.Close,
               maType="SMA", 
               n =14)

tail(AMZNrsi,10)

# Visualize 
browsable(
  tagList(
    dygraph(AMZN$AMZN.Close, group = "Price", height = 200, width = "100%"),
    dygraph(AMZNrsi, group = "Price", height = 200, width = "100%") %>%
      dyLimit(30, label = 'OverSold') %>%
      dyLimit(70, label = 'OverBought') %>%
      dyRangeSelector()
  )
)

# One more
getSymbols("HAS") 
HAS <- HAS["/2019-08-30"]

# Calc RSI
HASrsi <- RSI(HAS$HAS.Close,
              maType="SMA", #Usually EMA; not covered
              n =14)

# Visualize 
browsable(
  tagList(
    dygraph(HAS$HAS.Close, group = "Price", height = 200, width = "100%"),
    dygraph(HASrsi, group = "Price", height = 200, width = "100%") %>%
      dyLimit(30, label = 'OverSold') %>%
      dyLimit(70, label = 'OverBought') %>%
      dyRangeSelector()
  )
)

# Make it an indicator and back test
rsiIndicator <- Lag(ifelse(HASrsi > 30 & HASrsi < 70,1,0))
ret          <- ROC(Cl(HAS))*rsiIndicator 
charts.PerformanceSummary(ret)

# Now compound indicators; mix and match indicators, make your own etc
HASmacd    <- MACD(HAS$HAS.Close,
                   nFast = 12, nSlow = 26, nSig = 9, 
                   maType="SMA", 
                   percent = T)
HAScompoundRule <- Lag(ifelse(HASrsi$rsi < 70 &
                                HASmacd$macd > HASmacd$signal,
                              1, 0))

ret      <- ROC(Cl(HAS)) * HAScompoundRule
charts.PerformanceSummary(ret)

# How did it do with buy & hold?
plot(Cl(HAS))

# But how many days was capital at risk?
table(HAScompoundRule)

# End