#' Author: Ted Kwartler
#' Data: 2-13-2019
#' Purpose: Show normalization in action
#'

# Normalize Example
options(scipen=999)


# Make 2 fake vectors
vec1 <- c(1,2,3,4,5,6)
vec2 <- c(10,20,30,40,50,60)

# Calculate the un-normalized difference
vec1 - vec2

# Scale them to subtract the mean (center) so the average is now 0 and divide by the standard deviation (center)
scaledVec1 <- scale(vec1,center=T)
scaledVec2 <- scale(vec2,center=T)

# Now examine the difference; essentially 0.000
scaledVec1 -scaledVec2

## Manually compute on Vec1
vec1Mean <- mean(vec1)
vec1SD   <- sd(vec1)

inProgressVec1 <- vec1 - vec1Mean

# Compare
inProgressVec1 / vec1SD
scaledVec1

## Manually calc vec2 scale
vec2Mean <- mean(vec2)
vec2SD   <- sd(vec2)

inProgressVec2 <- vec2 - vec2Mean

# Compare
inProgressVec2 / vec2SD
scaledVec2


# Summary
summary(vec1)
summary(vec2)
summary(scaledVec1)
summary(scaledVec2)

# End