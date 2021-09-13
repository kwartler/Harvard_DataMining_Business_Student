#' Author: Ted Kwartler
#' Date: 9-13-2021
#' Purpose: R Data Types
#' 


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
singleDF   <- data.frame(vec=c(1,2,3), vec2=c(4,5,6))
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

# End

