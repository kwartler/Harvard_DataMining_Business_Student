#' Author: Ted Kwartler
#' Data: 2-6-2019
#' Purpose: Intro to basic R operations

## Simple math operators
2 + 2

4/2

4*2

5/3

5 %/% 3 #"modulo operator" means give me the whole integer no remainder

(5 / 3) - (5 %/% 3) #1.666 - 1

## Define variable objects
# Numeric Variables
x <- 2

y <- 3

z = 4

# T/F variables; spacing doesn't matter
harvard                  <- TRUE
yale      <- FALSE
princeton <- F

# Factor variables
ted    <- as.factor('MALE')
meghan <- as.factor('FEMALE')

#Examine
ted

# String (plain text) variables
nora   <- 'She is my daughter.'
brenna <- 'Another daughter.'
other  <- 'twins'

## Declare new objects using other variables
a <- x + y + z + 10

b <- a - ((x+y)*2) #inside out  

# Mixing object types will cause cryptic errors 
x + y + z + 10 + other

# (exception T/F)!
harvard + 1

## Make a vector; think of as a single column in a spreadsheet
vectorA <- c(1,2,y)

vectorB <- c(TRUE,TRUE,FALSE)

vectorC <- c(nora, brenna, other) # 3 seprarate "rows" of the vector

## Now collapse, this is like the concatenate function in Excel
collapsedStrings <- paste(nora, brenna, other, collapse = ' ') # not a vector!  Now 1 larger string

## Vector Operations
vectorA
vectorB
vectorA - vectorB # Vector operation AND auto-change TRUE =1, FALSE=0

# Now let's make a data frame, think of it conceptually like a spreadsheet
## I am stumped, let's get help for making a data frame
?data.frame # exact name
??data.frame # broader search 


## Construct a data frame; think of as an excel worksheet
dataDF <- data.frame(numberVec = vectorA,
                     trueFalseVec = vectorB,
                     stringsVec = vectorC)

# Examine an entire data frame (different than a matrix class, more to come on that)
dataDF

# Declare a new column
dataDF$NewCol <- c(10,9,8)

# Examine with new column
dataDF

# Examine a single column
dataDF$numberVec # by name
dataDF[,1] # by index...remember ROWS then COLUMNS

# Examine a single row
dataDF[2,] # by index position

# Examine a single value
dataDF$numberVec[2] # column name, then position (2)
dataDF[1,2] #by index row 1, column 2 

## Extract from R to a file; object to save then path
# Windows slashes are backwards!
write.csv(dataDF,"/cloud/project/tmpDataStorage/example.csv", row.names=F) 

## Read in a file as an object; just path
newDF<-read.csv("/cloud/project/tmpDataStorage/example.csv") 

# Examine & Compare to original
newDF
dataDF

## Basic plotting; MUCH more to come
x <- c(1,2,3,4)
y <- c(4,3,2,1)
plot(x,y)
plot(x,x)

vec1 <- c('Harvard', 'Harvard','Yale','Harvard')
table(vec1)
barplot(table(vec1))

# Save a plot to disk programatically
jpeg('/cloud/project/tmpDataStorage/example.jpg') # open up a background graphics device, declare path and file name
barplot(table(vec1)) # plot it
dev.off() # turn off the background graphics device

## Logical statements and for loops
# If statements
someValue <- 1

if (someValue==1){
  print('the value is 1')
}

otherValue <- TRUE
if (otherValue==F){
  print('value is FALSE')
  x <- 2+2
  } else {
  print('value is TRUE')
  x <- 3+3
  }
x

# For loops
for (i in 1:10){
  print('R is easy')
  }

for (j in 1:x){
  print(j+x)
}


#### BONUS: Lists are more complex with elements; just remember elements are just pieces of data
# Even
list(numberVec = vectorA,
     trueFalseVec = vectorB,
     stringsVec = vectorC)

# Ragged; 
list(numberVec = vectorA,
     trueFalseVec = vectorB[-1],
     stringsVec = vectorC)

data.frame(numberVec = vectorA,
           trueFalseVec = vectorB[-1],
           stringsVec = vectorC)

# Nested
list(numberVec = list(oneSection = vectorA,
                      twoSection = vectorB),
     trueFalseVec = vectorB[-1],
     stringsVec = vectorC)

# End
