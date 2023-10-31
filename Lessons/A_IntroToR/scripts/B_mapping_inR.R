#' Author: Ted Kwartler
#' Title: Mapping in R
#' Purpose: Load geospatial data and visualize it
#' Author: Ted Kwartler
#' email: edward.kwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Jan 23, 2022
#'

## Set the working directory
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(maps)
library(ggthemes)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(mapproj)

# Import
#amzn <- read.csv('amznWarehouses.csv')
gitFile <- url('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/A_IntroToR/data/amznWarehouses.csv')
amzn <- read.csv(gitFile)


# This is messy webscraped data, check out the state.
tail(amzn$STATE,25)

# Let's clean this up with string manipulation!
amzn$STATE <- gsub('location_','',amzn$STATE)
amzn$STATE <- trimws(amzn$STATE, which='both')
tail(amzn$STATE,25)


# Subset to New England
NEwarehouses <- amzn[ amzn$STATE  %in% c("MA","ME", "VT", "NH"), ]

# A basic map library
map()
dev.off()
map('usa')	# national boundaries
dev.off()
map("state", interior = FALSE)
dev.off()
map("state", interior = T)
dev.off()
map('county', 'new jersey') # reminder clear graphics device
dev.off()
map('state', region = c('mass', 'maine', 'vermont', 'new hampshire'))
points(NEwarehouses$lon,NEwarehouses$lat, col='red')
dev.off()

# Examine the map data
head(map_data('state'))
us <- fortify(map_data('state'), region = 'region')

# More familiar ggplot interface
gg <- ggplot() + 
  geom_map(data  =  us, 
           map = us,
           aes(x = long, y = lat, map_id = region, group = group), 
           fill = 'white', color = 'black', size = 0.25) + 
  coord_map('albers', lat0 = 39, lat1 = 45) +
  theme_map()
gg

# Subset to multiple states
ne <- us[ us$region %in% c("massachusetts","maine", "vermont", "new hampshire"), ]
ggNE <- ggplot() + 
  geom_map(data  =  ne, 
           map = ne,
           aes(x = long, 
               y = lat, 
               map_id = region, 
               group = group), 
           fill = 'white', color = 'black', size = 0.25) + 
  coord_map('albers', lat0 = 39, lat1 = 45) +
  theme_map()

# Examine
ggNE

# Add points layer
ggNE +
  geom_point(data  = NEwarehouses, 
             aes(x = lon, y=lat), 
             color = 'red', alpha=0.5) 

# County and single state
counties <- map_data("county")
MAcounty <- subset(counties, region == "massachusetts")
onlyMA   <- subset(NEwarehouses,NEwarehouses$stateAbb=='MA')

# State and county outlines
ggMA <- ggplot() + 
  geom_map(data  =  MAcounty, map = MAcounty,
           aes(x = long, y = lat, 
               map_id = region, group = group), 
           fill = 'white', color = 'blue', size = 0.25) + 
  coord_map('albers', lat0 = 39, lat1 = 45) +
  theme_map()

# Examine
ggMA

# Add points layer
ggMA +
  geom_point(data = onlyMA, 
             aes(x = lon, y = lat), color = 'red', alpha=0.5) 

# Leaflet layers using %>% pipe
mplot<- leaflet(data=onlyMA) %>%
  addTiles() %>% 
  addMarkers( popup = paste("Loc:", onlyMA$Location, "<br>",
                            "SqFt:", onlyMA$Sq..Feet,"<br>",
                            "Type:", onlyMA$Type),
              clusterOptions = markerClusterOptions()) %>%
  addResetMapButton()
mplot

# End
