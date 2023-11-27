# Practice with APIs Examples
# Cat Facts: https://catfact.ninja/fact
# Bitcoin pricing: https://api.coindesk.com/v1/bpi/currentprice.json
# Predict name based on the person's name: https://api.agify.io?name=meelad
# University Lists: http://universities.hipolabs.com/search?country=United+States
# US Zip code data: https://api.zippopotam.us/us/33162
# More topics here: https://apipheny.io/free-api/

# Libraries
library(jsonlite)
library(httr)

# Perform a GET request to get a cat fact
catAPI <- 'https://catfact.ninja/fact'
cats <- GET(catAPI)

# Call the object
cats

# Now extract the content() and see the list
content(cats)

# A simpler option (we show GET because you have to use in other instances)
# is to use fromJSON()
catFact <- fromJSON(catAPI)

# Now call the object and see the same in 1 step
catFact

# Now call the bitcoin data
bitcoinAPI <- 'https://api.coindesk.com/v1/bpi/currentprice.json'
bitcoinPrice <- fromJSON(bitcoinAPI)

# Now call this object and see a more complex names list. 
bitcoinPrice

# See if you can get the current $rate in USD
bitcoinPrice$bpi$USD$rate

# Now get a list of universities for a nation by first declaring the nation
# Use "United Kingdom" the first time; you can change this later
countryName <- 'United Kingdom'

# Encode any spaces to be read by a web browser
countryName <- URLencode(countryName)

# Examine what you did by calling the object
countryName

# Use paste0() to concatenate the base URL API and the search parameter
baseUni <- 'http://universities.hipolabs.com/search?country='
completeAPI <- paste0(baseUni, countryName)


# Now you can either use fromJSON or GET to obtain the data
allCountryUnis <- fromJSON(completeAPI)

# Examine a portion of what you obtained:
head(allCountryUnis)

# Now examine a zip code data API with a different parameter input
# Try it first with zip code 44224
zipCode <- '44224'

# Declare the base ZIP API URL
baseZip <- 'https://api.zippopotam.us/us/'

# paste0 with a slash not a search parameter as before.  Always be sure to examine and construct correctly!
oneZip <- paste0(baseZip, zipCode)

# Check what you constructed
oneZip

# Now use fromJSON to get the data 
oneZipData <- fromJSON(oneZip)

# Examine the list response from the API
oneZipData

# End
