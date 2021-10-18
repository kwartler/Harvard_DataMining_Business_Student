#' Author: Ted Kwartler
#' Data: 5-17-2018
#' Purpose: Show normalization in action
#' 

# Options
options(scipen=999)


# Make 2 fake vectors; one has larger values.
df <- data.frame(vec1 = c(1,2,3,4,5,6),
                 vec2 = c(10,20,30,40,50,60))
df

# Center and scale - subtract the mean of each vector and divide by the standard deviation
scale(df, center=T, scale=T)


## Manual Calc
# mean
vec1Avg <- mean(df$vec1)
vec2Avg <- mean(df$vec2)

# sd
vec1SD <- sd(df$vec1)
vec2SD <- sd(df$vec2)

(df$vec1 - vec1Avg) / vec1SD
(df$vec2 - vec2Avg) / vec2SD

# now the mean is 0
mean((df$vec2 - vec2Avg) / vec2SD)

# End
