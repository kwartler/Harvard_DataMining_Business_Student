#' Author: Ted Kwartler
#' Date: Nov 1 2020
#' Purpose: Simple Moving Avg Example As Indicator, recent stock
#'

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(PerformanceAnalytics)

# Get Symbol
stk <- getSymbols("IVZ", auto.assign = F) 

# Calculate moving averages
ma50  <- SMA(Cl(stk), 50)
ma200 <- SMA(Cl(stk), 200)

# Organize data
df        <- data.frame(Cl(stk), ma50, ma200)
names(df) <- c('closePrice', 'sma50', 'sma200')

# Graph
dygraph(df)  %>%
  dyRangeSelector()




# End