library(jsonlite)
tmp <- fromJSON('http://universities.hipolabs.com/search?country=Austria')
geoCode <- fromJSON(paste0('https://nominatim.openstreetmap.org/search.php?q=',URLencode(tmp$name[1]),'&format=jsonv2'))
