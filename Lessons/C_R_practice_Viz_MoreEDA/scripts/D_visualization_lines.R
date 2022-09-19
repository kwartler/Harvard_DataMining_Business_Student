#' Author: Ted Kwartler
#' Date: Sept 5 2022
#' Purpose: R ggplot line chart examples
#' Good resource: https://r-graphics.org/


# wd
setwd("~/Documents/Harvard_DataMining_Business_Student/personalFiles")

# libs
library(ggplot2)
library(ggthemes)
library(readr)
library(lubridate)
library(qcc)

# Load
possiblePurchase <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/C_R_practice_Viz_MoreEDA/data/MarthasVineyardCondo.csv')
possiblePurchase <- as.data.frame(possiblePurchase)

# Examine
head(possiblePurchase)

# Clean it up - column names
names(possiblePurchase) <- make.names(names(possiblePurchase))

# Clean $ signs
possiblePurchase$Avg.Price.Per.Night <- as.numeric(gsub('[$]', '', possiblePurchase$Avg.Price.Per.Night))

# Clean dates by splitting up info
tmp <- strsplit(possiblePurchase$Month,'-')
tmp # show
tmpMonth <- unlist(lapply(tmp, head, 1))
tmpMonth # show

# match month abbreviation to number & append 20 prefix
possiblePurchase$month <- match(tmpMonth,month.abb)
possiblePurchase$yr    <- paste0('20',unlist(lapply(tmp, tail, 1)))

# Check in to see whats going on
head(possiblePurchase)

# concatenate yr, month in format to be a "date" class using 1st day of month
tmpDates <- as.Date(paste(possiblePurchase$yr,possiblePurchase$month, '1', sep = '-'), "%Y-%m-%d")
tmpDates

# But this date is monthly stats _after_ the month is done so concatenate to the last day of the month ; notice the leap year!
possiblePurchase$closingDate <- days_in_month(tmpDates) # from lubridate

# Now put the Month column into a closing month Date class by overwriting it
possiblePurchase$Month <- as.Date(paste(possiblePurchase$yr,possiblePurchase$month,possiblePurchase$closingDate,sep='-'),
                                  "%Y-%m-%d")

# Finally 
head(possiblePurchase)

# A line chart helps associate linkages between points for easier comparisons; note the 2019 early year lack of occupancy 
ggplot(data = possiblePurchase, aes(x=month, y=NightOccupied, group=yr, color=yr)) +
  geom_line() + 
  theme_few() +
  labs(fill="")

# Here we see all data points, although not continuous we can see the peak to trough 
totalTimeline <- ggplot(data = possiblePurchase, aes(x=month, y=NightOccupied, group=yr, color=yr)) +
  geom_line() + 
  theme_few() +
  labs(fill="")
totalTimeline


# cumsum by group, really compelling with many groups and time component, could be a "line chart" also though
rap <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/C_R_practice_Viz_MoreEDA/data/rapSongsTimeline_wrangledData.csv')
rap <- as.data.frame(rap)
head(rap)
totalWords <- rap %>% group_by(song) %>% summarise(maxWords = max(cumulativeWords, na.rm=TRUE))
totalTime <- rap %>% group_by(song) %>% summarise(endTime = max(endTime, na.rm=TRUE))
rapStats <- left_join(totalWords, totalTime,  by = "song")
rapStats$eminem <- grepl('Eminem', rapStats$song, ignore.case = T)
ggplot(rap,  aes(x = endTime,
                y  = cumulativeWords, 
                          group = song, 
                          color = eminem)) +
  geom_line(alpha = 0.25) +
  geom_point(data =rapStats, aes(x     = endTime,
                                 y   = maxWords, 
                                 group = endTime,
                                 color = eminem), size = 2) +
  theme_minimal() +
  ggtitle('30 Rap Songs, Eminem Rate of Speech Vs Others')
# End