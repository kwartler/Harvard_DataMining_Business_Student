#' Author: Ted Kwartler
#' Date: 8-30-2019
#' Purpose: Simple Moving Avg Example As Indicator
#'

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(PerformanceAnalytics)

# Get Salesforce
getSymbols("CRM") #"CRM_1_TTR_C.rds"
CRM <- CRM['2018-03-01/2019-06-22'] 

# Calculate moving averages
CRMma50  <- SMA(CRM$CRM.Close, 50)
CRMma200 <- SMA(CRM$CRM.Close, 200)

# Organize data
df        <- data.frame(CRM$CRM.Close, CRMma50, CRMma200)
names(df) <- c('CRMclose', 'sma50', 'sma200')

# Construct a trading rule; 1 = buy or stay in stock, 0 = sell
# Not discussing shorting a stock (-1)
df$Lag.1 <- Lag(ifelse(df$sma50 > df$sma200, 1, 0), k = 1) 
?Lag

# Examine part1
df[199:205,] # row 200 is NA for `Lag.1` due to Lag()

# Manually reviewing this section
#              CRMclose  SMA50    SMA200       Lag.1
# 2018-12-19   131.16    136.4284 136.7504     1
# 2018-12-20   127.73    136.0130 136.7624     0
# Buy or Retain position :     2018-12-19  
# Sell or stay out of position:2018-12-20 

# Examine part2
df[235:240,]
# Manually reviewing this section
#              CRM.Close SMA50    SMA200       Lag.1
# 2019-02-05   158.99    140.4092 140.2293     0
# 2019-02-06   158.13    141.1000 140.4058     1
# Sell or stay out of position :2019-02-05  
# Buy or retain position:       2019-02-06

# Examine more wholistically
tail(df, 25)
summary(df$Lag.1)
table(df$Lag.1)

# Now let's do it for a longer backtest with a different stock
getSymbols("CMG") # Chipotle Restaurants
CMG      <- CMG['2018-01-01/']
CMGma50  <- SMA(CMG$CMG.Close, 50)
CMGma200 <- SMA(CMG$CMG.Close, 200)

# Set up the indicator
tradeSignal <- Lag(ifelse(CMGma50 > CMGma200  , 1, 0))
ret         <- ROC(Cl(CMG))*tradeSignal #Rate of Change TTR::ROC(); not receiever operating curve

# Review your return
charts.PerformanceSummary(ret)

# Compare to buy & hold
plot(Cl(CMG))

# Now let's be knight cap and switch a sign!
getSymbols("CMG") #"CMG_1_TTR_C.rds"
CMGma50     <- SMA(CMG$CMG.Close, 50)
CMGma200    <- SMA(CMG$CMG.Close, 200)
tradeSignal <- Lag(ifelse(CMGma50 < CMGma200  , 1, 0), k = 1)
ret         <- ROC(Cl(CMG))*tradeSignal #Rate of Change TTR::ROC()

# Review your return
charts.PerformanceSummary(ret)

# End
