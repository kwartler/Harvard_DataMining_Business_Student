#' Author: Ted Kwartler
#' Date: 2-13-2019
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

# Left join
left_join(instruments, grades, by = c('name'))

# Right Join
right_join(instruments, grades, by = c('name'))

# Inner Join
inner_join(instruments, grades, by = c('name'))

# Full join
full_join(instruments, grades, by = c('name'))


# End