#' Author: Ted Kwartler
#' Data: Mar 21 2022
#' Purpose: Example get table data of time series

# Options
options(scipen=999)

# wd
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/G_RF_TimeSeries/data")

# Libary
library(rvest)
library(MLmetrics)
library(lubridate)

# Stock
stk <- 'CVS'

# URL
stkURL <- paste0('https://ycharts.com/companies/',
                 stk,
                 '/revenues')

# Left Hand Table
y <- stkURL %>% read_html() %>% html_nodes(xpath = '/html/body/main/div/div[4]/div/div/div/div[3]/div[1]/div/div[2]/div[2]/div[1]/table') %>% html_table()

# Right Hand Table
z <- stkURL %>% read_html() %>% html_nodes(xpath = '/html/body/main/div/div[4]/div/div/div/div[3]/div[1]/div/div[2]/div[2]/div[2]/table') %>% html_table()

# Examine
y[[1]]
z[[1]]

## Next steps are to perform data clean up, change date column with lubridate functions.  Use gsub & as.numeric to clean up Value column. Then combine and sort by date before saving.

# Remove non numeric from Value column
allRev <- rbind(y[[1]], z[[1]])
allRev$Value <-as.numeric(gsub("[^0-9.-]", 
                               "", 
                               allRev$Value))

# Date Column
allRev$DateParsed <- mdy(allRev$Date)

# Order
allRev <- allRev[order(allRev$DateParsed),]

# Now save as CSV
write.csv(allRev, 
          paste0(stk, '_quarterlyRev.csv'), 
          row.names = F)

# Official TS class - Training
stYr  <- year(allRev$DateParsed[1])
stQtr <- quarter(allRev$DateParsed[1])
st    <- c(stYr, stQtr)
trainingTS <- ts(allRev$Value[1:38], 
                 start = st, 
                 frequency = 4)
plot(trainingTS)

# Example HW - OOT Sample
qtrHW <- HoltWinters(trainingTS, seasonal = "additive")
#multiplicative or additive

# Eval
futureTS <- predict(qtrHW, 12)
evalDF <- data.frame(actual  = allRev$Value[39:50],
                     forecast = futureTS)
RMSE(evalDF$actual, evalDF$fit)
MAPE(evalDF$actual, evalDF$fit)

# End
