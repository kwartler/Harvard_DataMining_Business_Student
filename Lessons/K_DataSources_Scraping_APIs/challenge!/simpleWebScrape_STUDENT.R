#'Challenge
#'Webscrape this page to obtain the table of data
#'#https://en.wikipedia.org/wiki/World_population

#libraries
library(rvest)

# Declare the page
pg <- '.....'

# Read the original page using read_html
tmp <- ________(__)

# Obtain the table in a node called '.mw-parser-output' then apply html_table()
x <- tmp %>% html_nodes('') %>% ______()

# The result is a list so select the **second** one and declare it a simple data frame 
popTable <- as.____._____(x[[_]])

# Examine the top 6 rows
head(________)

# Drop the first row 
popTable <- ________[-_,]

# Use gsub to make Population a numeric column
popTable$_________ <- __._______(____(",","",________$_________))

# Re"order()" it by least populated
popTable <- ________[_____(________$__________, decreasing = F),]

# Now examine the top 12 rows to see the least populated regions
____(popTable, __)

# End

