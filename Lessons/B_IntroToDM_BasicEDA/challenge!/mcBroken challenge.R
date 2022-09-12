#' Author: Ted Kwartler
#' Date: 9-05-2022
#' Purpose: EDA on Aug 12 ice cream machines
#' 
#' lon - longitude
#' lat - latitude
#' is_broken - is the machine actively capable of dispensing ice cream
#' city, state, street - geo data
#' last_checked - exact timestamp of the ping
#' date - overall data represented

# libs
library(radiant.data)
library(DataExplorer)
library(readr)

# No sci notation!
options(scipen = 999)


# Read in Data
iceCream <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/B_IntroToDM_BasicEDA/challenge!/mcBroken_august_12_2022.csv')

# Look at the bottom 6 rows

# Are there any NA or missings in the data?

# how many rows are there?  These are machine sensors indications so there are usually a lot

# Subset to just 8-12
iceCream <- subset(________, ________$____=='2022-08-12')

# how many rows are there now?  Was there noise in the data query?

# get a table() of the unique states.  How many machines are found in each location?

# Overall how many broken machines are there?

# what state has the highest _percentage_ of broken machines? This is a tough one, requiring you to know the total machines in a state, the total broken and to get the proportion.
# hint, you can do this stepwise or you can employ the function prop.table()

# create a map of the machines on this date (hint, use leaflet from first week script with "markerClusterOptions()")


# are there any unusual data points that should be removed?  Subset the data to just US machines or a state of interest like MA

# Subset to just the broken machines at this for August 12, then use ggplot to make another map plot with the smaller data, not of all machines but just broken.  If this is too much for your computer...get a new computer ;) or just pick a single state and plot it.

# End
