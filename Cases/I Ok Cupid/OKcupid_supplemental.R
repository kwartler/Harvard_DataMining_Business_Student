#' Author: Ted Kwartler
#' Date: 2-14-2019
#' Purpose: OKCupid Case Supplemental
#' 

# Libs
library(dplyr)

# Set WD
setwd()

# See all files in wd, leave this blank inside the parentheses
dir()

# Get the okcupid data as `profiles`
profiles <- read.csv('profiles.csv')
latlon<- read.csv('LatLon.csv')

##### I would do some basic EDA and plotting of individual vars then move to more complex interactions
table(profiles$orientation)
hist(profiles$age)

##### Example 2 way EDA
table(profiles$age,profiles$orientation)

#### Missing in income & quick mean imputation example; you can still use vtreat instead to clean all this data but we are only exploring not modeling so maybe dont do it for this case.
sum(is.na(profiles$income))
profiles$income[is.na(profiles$income)] <- mean(profiles$income, na.rm=TRUE)

##### Feature Engineer relationship status & education if you thought there was a connection
profiles$statEDU <- paste(profiles$status, profiles$education, sep = '_')
table(profiles$statEDU)

##### Enrich with one of the new data sets, you may want to do this with the other csv
moreData <- left_join(profiles, latlon, by ='location')
head(moreData)

#### You can use complete.cases() to identify records without NA if that is the route you want to explore.  Of course you can use a function covered in class to visualize the variables with the hightest % of NA so you could drop those instead of all rows with an NA.
completeMoreData <- moreData[complete.cases(moreData),]

# End
