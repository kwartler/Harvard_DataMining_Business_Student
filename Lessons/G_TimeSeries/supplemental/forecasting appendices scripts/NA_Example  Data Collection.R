#' Author: Ted Kwartler
#' Data: 6-6-2018
#' Purpose: Example to scrape time series financial data (quarterly revenue)


library(jsonlite)


# Find a site with data
# https://www.gurufocus.com/financials/WMT
# https://www.gurufocus.com/chart/AMZN

#### WMT Quarterly
# Quarterly Revenue; be sure to click on the correct option on the left of the page
# https://www.gurufocus.com/chart/WMT?fp=q#&serie=,,id:Revenue_2,s:NYSE:WMT
stockURL <- 'https://www.gurufocus.com/modules/chart/interactive_chart_json_morn.php?symbol=NYSE:WMT&fp=q&ser=Revenue'
wmtQtr <- fromJSON(stockURL)

wmtQtrDF <- data.frame(unixTime = wmtQtr[[2]][,1]/1000, revMill = wmtQtr[[2]][,2])
write.csv(wmtQtrDF, 'WMT_Qtr_Rev.csv', row.names=F)

#### AMZN Quarterly
stockURL2 <- 'https://www.gurufocus.com/modules/chart/interactive_chart_json_morn.php?symbol=NAS:AMZN&fp=q&ser=Revenue'
amznQtr <- fromJSON(stockURL2)

amznQtrDF <- data.frame(unixTime = amznQtr[[2]][,1]/1000, revMill = amznQtr[[2]][,2])
write.csv(amznQtrDF, 'AMZN_Qtr_Rev.csv', row.names=F)

# Clean up workspace
rm(list=ls())

#### AMZN Annual Revenue
amznURL <-'https://www.gurufocus.com/modules/chart/interactive_chart_json_morn.php?symbol=NAS:AMZN&ser=Revenue'
amznAnnual <- fromJSON(amznURL)

amznAnnDF <- data.frame(unixTime = amznAnnual[[2]][,1]/1000, revMill = amznAnnual[[2]][,2])
write.csv(amznAnnDF, 'AMZN_Ann_Rev.csv', row.names=F)

#### WMT Annual Revenue
wmtURL <-'https://www.gurufocus.com/modules/chart/interactive_chart_json_morn.php?symbol=NYSE:WMT&ser=Revenue'
wmtAnnual <- fromJSON(wmtURL)

wmtAnnDF <- data.frame(unixTime = wmtAnnual[[2]][,1]/1000, revMill = wmtAnnual[[2]][,2]*1000)
write.csv(wmtAnnDF, 'WMT_Ann_Rev.csv', row.names=F)

# End