#' Author: Ted Kwartler
#' Date: 9-13-2021
#' Purpose: Load geospatial data, explore it and visualize it

## Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/Lessons/B_IntroToDM_EDA/data")

# Libs
library(maps)
library(ggthemes)
library(ggplot2)
library(leaflet)
library(mapproj)

# Import
cellTowers       <- read.csv('newEnglandCellTowers.csv')
cellTowers$state <- trimws(cellTowers$state, which='both')

# Subset to New England
NEtowers <- cellTowers[ cellTowers$state %in% c("MA","ME", "VT", "NH"), ]

# A basic map library
map()
dev.off() #sometimes the graphics driver needs to be reset!
map('usa')	# national boundaries
map("state", interior = FALSE)
dev.off() 
map("state", interior = T)
dev.off() 
map('county', 'new jersey') # reminder clear graphics device
dev.off() #sometimes the graphics driver needs to be reset!
map('state', region = c('mass', 'maine', 'vermont', 'new hampshire'))
points(NEtowers$lon,NEtowers$lat, col='red')

# More familiar ggplot interface
# state.name
stateData <- map_data('state')
head(stateData)
us <- fortify(stateData, region = 'region')
gg <-ggplot() + geom_map(data  =  us, map = us,
                         aes(x = long, y = lat, map_id = region, group = group),
                         fill = 'white', color = 'black', size = 0.25) + 
  coord_map('albers', lat0 = 39, lat1 = 45) +
  theme_map()
gg 

# Subset to multiple states
ne <- stateData[ stateData$region %in% c("massachusetts","maine", "vermont", "new hampshire"), ]
ne <- fortify(ne, region = 'region')
head(ne)
ne <-ggplot() + geom_map(data  =  ne, map = ne,
                         aes(x = long, y = lat, map_id = region, group = group),
                         fill = 'white', color = 'black', size = 0.25) +
  coord_map('albers', lat0 = 39, lat1 = 45) +
  theme_map()
ne 
ne + #add the data points with lon/lat declaring the columns
  geom_point(data=NEtowers, aes(x=lon, y=lat), color='red', alpha=0.15)

# County and single state
counties <- map_data("county")
MAcounty <- subset(counties, region == "massachusetts")
head(MAcounty)
onlyMA   <- subset(NEtowers,NEtowers$state=='MA')
head(onlyMA)

# Now build layer by layer State and county outlines then cell
ma <-ggplot() + geom_map(data  =  MAcounty, map = MAcounty,
                         aes(x = long, y = lat, map_id = region, group = group),
                         fill = 'white', color = 'black', size = 0.25) + 
  
  coord_map('albers', lat0 = 39, lat1 = 45) +
  theme_map() 
ma
# Add the points
ma + geom_point(data=onlyMA, aes(x=lon, y=lat, group=1), 
             color='red', alpha=0.15)

# Notice how the original layer has no dots because we didn't declare an layer when making `ma`
ma

# Leaflet layers using %>% pipe
#https://techcrunch.com/2017/01/19/in-the-future-ai-could-also-mean-auto-insurance/
webMap <- leaflet(data=onlyMA) %>%
  addTiles() %>%
  addMarkers( popup = paste("Owner:", onlyMA$Owner, "<br>",
                            "Registration:", onlyMA$RegistrationNum)) 
webMap
# End
