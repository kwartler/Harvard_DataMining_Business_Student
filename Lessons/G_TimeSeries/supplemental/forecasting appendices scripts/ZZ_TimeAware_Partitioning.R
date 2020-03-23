#' Author: Ted Kwartler
#' Data: 7-22-2018
#' Purpose: Time aware partitioning for a ts object

# Time aware partition; select the number of periods in the validation set
nValid <- 36
nTrain <- length(ridership.ts) - nValid

# partition the data
trainTS <- window(ridership.ts, start = c(1991, 1), 
                   end  = c(1991, nTrain))
validTS <- window(ridership.ts, start = c(1991, nTrain + 1),
                   end = c(1991, nTrain + nValid))