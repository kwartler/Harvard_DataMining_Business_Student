#' Author: Ted Kwartler
#' Date: Feb 6 2022
#' Purpose: Joins
#'

# Libs
library(dplyr)

# Get small data example
instruments <- as.data.frame(band_instruments)
instruments <- rbind(instruments, c("Sarah", "drums"))
instruments

# Get more data with shared column
grades <-  data.frame(name = c(instruments$name[1:3],"Jim"),
                      ptAvg = c(3.2,3.3,3.4, 3.9))
grades

# Left join; bring information from grades to instruments; no "Jim" & no grade for "Sarah"
leftData <- left_join(instruments, grades, by = c('name'))
leftData

# Right Join;bring infromation from instruments to grades;  no "Sarah" & no "Jim" unstrument 
rightData <- right_join(instruments, grades, by = c('name'))
rightData

# Inner Join; drop Sarah and Jim as incomplete records
innerData <- inner_join(instruments, grades, by = c('name'))
innerData

# Full join; everything is included
fullData <- full_join(instruments, grades, by = c('name'))
fullData

# All joins can be done with column names that are different names
names(instruments)[1] <- 'student'
instruments

leftDataNewColName <- left_join(instruments, grades, by = c('student'='name'))
leftDataNewColName


# End

