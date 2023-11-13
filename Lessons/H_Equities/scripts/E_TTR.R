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

# Get a Stock
stk <- getSymbols("AMZN", auto.assign = F) 

# Calc RSI
STKrsi <- RSI(Cl(stk),
               maType="SMA", 
               n =14)

tail(STKrsi,10)

# Visualize 
browsable(
  tagList(
    dygraph(Cl(stk), group = "Price", height = 200, width = "100%"),
    dygraph(STKrsi, group = "Price", height = 200, width = "100%") %>%
      dyLimit(30, label = 'OverSold') %>%
      dyLimit(70, label = 'OverBought') %>%
      dyRangeSelector()
  )
)

# One more
stk <- getSymbols("HAS", auto.assign = F) 

# Calc RSI
STKrsi <- RSI(Cl(stk),
              maType="SMA", #Usually EMA; not covered
              n =14)

# Visualize 
browsable(
  tagList(
    dygraph(Cl(stk), group = "Price", height = 200, width = "100%"),
    dygraph(STKrsi, group = "Price", height = 200, width = "100%") %>%
      dyLimit(30, label = 'OverSold') %>%
      dyLimit(70, label = 'OverBought') %>%
      dyRangeSelector()
  )
)

# Make it an indicator and back test
rsiIndicator <- Lag(ifelse(STKrsi > 30 & STKrsi < 70,1,0))
table(rsiIndicator)
ret          <- ROC(Cl(stk))*rsiIndicator 
charts.PerformanceSummary(ret)

# Now compound indicators; mix and match indicators, make your own etc
STKmacd    <- MACD(Cl(stk),
                   nFast = 12, nSlow = 26, nSig = 9, 
                   maType="SMA", 
                   percent = T)
STKcompoundRule <- Lag(ifelse(STKrsi[,1] < 70 &
                                STKrsi[,1] > 30 & 
                                STKmacd$macd > STKmacd$signal,
                              1, 0))

ret      <- ROC(Cl(stk)) * STKcompoundRule
charts.PerformanceSummary(ret)

# How did it do with buy & hold?
plot(Cl(stk))

# But how many days was capital at risk?
table(rsiIndicator)
table(STKcompoundRule)

# End
