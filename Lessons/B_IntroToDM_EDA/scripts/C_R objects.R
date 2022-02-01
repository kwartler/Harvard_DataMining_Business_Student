#' Author: Ted Kwartler
#' Date: 9-13-2021
#' Purpose: R Data Types
#' 

# libs
library(dplyr)

# Numeric Vector
c(1,10,12,3.47)

# Boolean Vector
c(T, T, F, T, F)
c(TRUE,TRUE, FALSE, TRUE,FALSE)
c(T,TRUE, F, TRUE,FALSE)

# Factor Vectors
as.factor(c('MALE','FEMALE','FEMALE'))

# String Vectors
c('MALE','FEMALE','FEMALE')

# Matrix
as.matrix(warpbreaks[1:10, ])

# Data Frame
warpbreaks[1:10, ]

# Attributes of an object contain meta-information and can have unique identifiers
attributes(warpbreaks[1:10,])


# List craziness!
singleVal  <- 123
singleDF   <- data.frame(vec1=c(1,2,3), vec2=c(4,5,6))
singleVec  <- c(T,T,F,F,F)

# Construct list
listA <- list(singleVal=singleVal,
              singleDF=singleDF,
              singleVec=singleVec)

# Get the first element of the list
listA[[1]]

# Get the 2nd element of the list
listA[[2]]

# Get the 3rd element of the list
listA[[3]]

# Get the 3rd element, first object
listA[[3]][1]

# Get the 2nd element, second column
listA[[2]][,2]

# Get the 2nd element, 1st row
listA[[2]][1,]

# Its a best practice to name each element as the list complexity grows
listA$singleVec
listA$singleDF$vec
listA$singleVal


#### JOINS FOR THE CASE REFERENCE ####
# Covered later but here is another example
# There are times you have data sets with information that need to be combined
# For example, a table of customers and a table of their purchases
# SQL and R have "table joins" which aid this merger

leftTable <- data.frame(customerID     = c(0, 1,2,3,4),
                        customerIncome = c(38000,40000,42000, 44000, 56000))
rightTable <- data.frame(customerID           = c(1,4,3,2,5),
                         customerVisitDaysOld = c(22,24,NA,28,30 ),
                         customerPurchaseAmt  = c(19,110,18,36, 105),
                         customerProduct      = c('A','A','B','A', NA))

# Examine
leftTable
rightTable

# Left join remains
leftTBL <- left_join(leftTable, rightTable, by = c('customerID' ='customerID'))
leftTBL

# Right join remains
rightTBL <- right_join(leftTable, rightTable, by = c('customerID' ='customerID'))
rightTBL

# Inner join retains all shared
innerTBL <- inner_join(leftTable, rightTable, by = c('customerID' ='customerID'))
innerTBL

# Anti join retains the only record from left not in right side
antiTBL <- anti_join(leftTable, rightTable, by = c('customerID' ='customerID'))
antiTBL

# Full join keeps them all
fullTBL <- full_join(leftTable, rightTable, by = c('customerID' ='customerID'))
fullTBL

# End

