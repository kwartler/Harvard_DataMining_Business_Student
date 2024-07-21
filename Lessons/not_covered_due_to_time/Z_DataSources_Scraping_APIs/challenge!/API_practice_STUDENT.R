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
cats <- ___(______)

# Call the object
____

# Now extract the content() and see the list
_______(____)

# A simpler option (we show GET because you have to use in other instances)
# is to use fromJSON()
catFact <- ________(catAPI)

# Now call the object and see the same in 1 step
_______

# Now call the bitcoin data using fromJSON
bitcoinAPI <- 'https://api.coindesk.com/v1/bpi/currentprice.json'
bitcoinPrice <- ________(__________)

# Now call this object and see a more complex names list. 
____________

# See if you can get the current $rate in USD
____________$___$___$____

# Now get a list of universities for a nation by first declaring the nation
# Use "United Kingdom" the first time; you can change this later
countryName <- '______ _______'

# Encode any spaces to be read by a web browser using URLencode()
countryName <- _________(countryName)

# Examine what you did by calling the object
countryName

# Use paste0() to concatenate the base URL API and the search parameter
baseUni <- 'http://universities.hipolabs.com/search?country='
completeAPI <- ______(baseUni, countryName)

# Check your completeAPI address by opening a webbrowser and pasting in the address 
completeAPI

# Now you can either use fromJSON or GET to obtain the data
allCountryUnis <- ________(completeAPI)

# Examine a portion of what you obtained:
____(______________)

# Now examine a zip code data API with a different parameter input
# Try it first with zip code 44224
zipCode <- '_____'

# Declare the base ZIP API URL
baseZip <- 'https://api.zippopotam.us/us/'

# paste0 with a slash not a search parameter as before.  Always be sure to examine and construct correctly!
oneZip <- ______(baseZip, zipCode)

# Check what you constructed
______

# Now use fromJSON to get the data 
oneZipData <- ________(oneZip)

# Examine the list response from the API
__________

# End
