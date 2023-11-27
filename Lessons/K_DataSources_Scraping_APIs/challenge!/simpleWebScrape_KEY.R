#'Challenge
#'Webscrape this page to obtain the table of data
#'https://en.wikipedia.org/wiki/World_population

#libraries
library(rvest)

# Declare the page
pg <- 'https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population'

# Read the original page using read_html
tmp <- read_html(pg)

# Obtain the table in a node called '.mw-parser-output' then apply html_table()
x <- tmp %>% html_nodes('.mw-parser-output') %>% html_table()

# The result is a list so select the **second** one and declare it a simple data frame 
popTable <- as.data.frame(x[[2]])

# Examine the top 6 rows
head(popTable)

# Drop the first row 
popTable <- popTable[-1,]

# Use gsub to make Population a numeric column
popTable$Population <- as.numeric(gsub(",","",popTable$Population))

# Re"order()" it by least populated
popTable <- popTable[order(popTable$Population, decreasing = F),]

# Now examine the top 12 rows to see the least populated regions
head(popTable, 12)

# End

