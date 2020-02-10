#' Author: Ted Kwartler
#' Date: 9-11-2019
#' Purpose: Load geospatial data, explore it and visualize it

## Set the working directory
setwd("/cloud/project/Lessons/B_IntroToDM_EDA/wk2_Data")

# Libs
library(maps)
library(ggthemes)
library(ggplot2)
library(leaflet)

# Import
cellTowers       <- read.csv('newEnglandCellTowers.csv')
cellTowers$state <- trimws(cellTowers$state, which='both')

# Subset to New England
NEtowers <- cellTowers[ cellTowers$state %in% c("MA","ME", "VT", "NH"), ]

# A basic map library
map()
map('usa')	# national boundaries
map("state", interior = FALSE)
map("state", interior = T)
map('county', 'new jersey') # reminder clear graphics device
map('state', region = c('mass', 'maine', 'vermont', 'new hampshire'))
points(NEtowers$lon,NEtowers$lat, col='red')

# More familiar ggplot interface
# state.name
stateData <- map_data('state')
head(stateData)
ggplot() + 
  geom_polygon(data = stateData, aes(x=long, y = lat, color = region)) + 
  coord_fixed(1.3) +
  theme_gdocs() + theme(legend.position="none")

# Subset to multiple states
ne <- stateData[ stateData$region %in% c("massachusetts","maine", "vermont", "new hampshire"), ]
ggplot() + 
  geom_polygon(data = ne, 
               aes(x=long, 
                   y = lat, 
                   fill=region, 
                   group = group), 
               fill='lightgrey', 
               color='blue') + 
  coord_fixed(1.3) +
  geom_point(data=NEtowers, aes(x=lon, y=lat), color='red', alpha=0.15) +
  theme_gdocs()

# County and single state
ma       <- subset(stateData,stateData$region=='massachusetts')
counties <- map_data("county")
MAcounty <- subset(counties, region == "massachusetts")
onlyMA   <- subset(NEtowers,NEtowers$state=='MA')

# State and county outlines
ggplot(data = MAcounty, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "white", fill = "lightgrey") + 
  geom_polygon(data=ma, aes(x=long, y=lat, group=group), fill= NA, color='black') + # adds new data
  geom_point(data=onlyMA, aes(x=lon, y=lat, group=1), color='red', alpha=0.15) +
  theme_gdocs()

# Leaflet layers using %>% pipe
mplot<- leaflet(data=onlyMA) %>%
  addTiles() %>%
  addMarkers( popup = paste("Owner:", onlyMA$Owner, "<br>",
                            "Registration:", onlyMA$RegistrationNum)) 
mplot

# End
