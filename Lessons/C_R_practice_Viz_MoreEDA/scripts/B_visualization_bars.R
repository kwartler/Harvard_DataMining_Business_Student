#' Author: Ted Kwartler
#' Date: Feb 5, 2024
#' Purpose: R bar visual ggplot bars examples
#' Good resource: https://r-graphics.org/

# wd
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")
# libs
library(ggplot2)
library(ggthemes)
library(lubridate)

# Load
possiblePurchase <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/C_R_practice_Viz_MoreEDA/data/MarthasVineyardCondo.csv')

# Examine
head(possiblePurchase)

# Clean it up - column names
#names(possiblePurchase) <- make.names(names(possiblePurchase))

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

# But this date is monthly stats _after_ the month is done so concatenate to the last day of the month ; notice the leap year is accounted for!
possiblePurchase$closingDate <- days_in_month(tmpDates) # from lubridate

# Now put the Month column into a closing month Date class by overwriting it
possiblePurchase$Month <- as.Date(paste(possiblePurchase$yr,possiblePurchase$month,possiblePurchase$closingDate,sep='-'),
                                         "%Y-%m-%d")

# Finally 
head(possiblePurchase)

#  density: numeric and distribution matters; this isn't a great dataset for it, similar to a histogram, from the same variable
ggplot(data = possiblePurchase, aes(x=NightOccupied)) +
  geom_density( color="red", alpha=0.6, position = 'identity') + 
  theme_few() 

# histogram: buckets similar values to see the distribution from the same variable
ggplot(data = possiblePurchase, aes(x=NightOccupied)) +
  geom_histogram(binwidth=2, color = 'red') +
    stat_bin(binwidth=2, geom='text', color='white', aes(label=after_stat(count)),
             position=position_stack(vjust = 0.5)) + theme_few() 

# barchart for comparing quantities.  I prefer geom_col so its not an automatic count of the observations
totalNights <- aggregate(NightOccupied ~ yr, 
                         data = possiblePurchase, 
                         FUN  = sum)
totalNights

ggplot(data = totalNights, aes(x = yr, y = NightOccupied)) + 
  geom_col() + 
  theme_few() + 
  geom_text(aes(label = NightOccupied), color = 'white',vjust = 1.5)

# If the data is "atomic" i.e. can't be broken down further  you can use geom_bar to get a count.
# Our data is not atomic so each bar represents the number of times it appears ie two months in our data set had 10 nights occupied, like the histogram
ggplot(data = possiblePurchase, aes(x = NightOccupied)) + 
  geom_bar() + 
  theme_few() 

# Side by side bar chart to compare values by category; month is really a class or a month not a numeric so factor() is used
df <- subset(possiblePurchase, possiblePurchase$yr != '2020') #drop 2020 incomplete
ggplot(data = df, aes(x = factor(month), y = NightOccupied, fill = yr)) +
  geom_bar(position=position_dodge(), stat="identity") + 
  geom_text(aes(label = NightOccupied), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 2) + 
  theme_few()

# Stacked bar chart: used to compare  values within a category, 
dataStack <- data.frame(month  = as.factor(df$month),
                        yr     = df$yr,
                        vacant = df$closingDate-df$NightOccupied,
                        occupied= df$NightOccupied)
head(dataStack)
tail(dataStack)
ggplot(data = dataStack, aes(fill=yr, y=vacant, x=month)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_few()

# Example facet which is a way to make multiple charts by a factor level
ggplot(data = dataStack, aes(fill=yr, y=occupied, x=month)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_few() +facet_wrap(.~yr)

# filled stacked:  used to compare proportion only within a category
# Make a "goodMonth" indicator as an example
possiblePurchase$goodMonth <- ifelse(possiblePurchase$NightOccupied>15,
                                     "good", "bad")
ggplot(possiblePurchase, aes(x = factor(month), fill = goodMonth)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_few()



# End
