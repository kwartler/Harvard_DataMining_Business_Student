# All possible cutoff thresholds
allCutoffs <- seq(from = 0, to = 1, by = .001)

# Create an empty list to save the accuracy values
allAccuracies <- list()

# Create a loop to run through each cutoff
for(i in 1:length(allCutoffs)){
  print(allCutoffs[i]) # print progress
  cutoff <- allCutoffs[i] # select one cutoff value
  teamClasses <- ifelse(teamPreds >= cutoff, 1,0) # apply the cutoff to each prediction probability
  resp <- Accuracy(y_pred = teamClasses, y_true = bball$R1.Class.1.win) # get corresponding KPI
  allAccuracies[[i]] <- resp # add it to the list
}

# Organize to a vector
allAccuracies <- unlist(allAccuracies)

# Which cutoff value has the highest accuracy?
allCutoffs[which.max(allAccuracies)]

# What is the corresponding accuracy?
allAccuracies[which.max(allAccuracies)]
