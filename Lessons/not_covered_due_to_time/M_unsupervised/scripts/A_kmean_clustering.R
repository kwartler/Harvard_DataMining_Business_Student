#' Author: Ted Kwartler
#' Date: Dec 3-2023
#' Purpose: K Means Clustering for customer segmentation
#' 

# Libraries
library(dplyr)
library(ggplot2)
library(ggthemes)
library(cluster)
library(factoextra)

# Load
customerData <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/M_unsupervised/data/Cluster_Customers.csv')

### SAMPLE
# toy data set so let's just take 5 rows
set.seed(2023)
idx        <- sample(1:nrow(customerData),10)
training   <- customerData[-idx,]
validation <- customerData[idx,]


### EXPLORE
head(training)
dim(training)
summary(training)

### MODIFY
# Drop the ID
training$CustomerID   <- NULL
validation$CustomerID <- NULL

# Now scale the columns of the training and also apply it to the validation
training <- scale(training) # Subtract each column's mean & divide by std deviation

# Examine the transformation; distance from the column average i.e. z-score
summary(training)

# Now select the mean values and standard deviations 
trainMean <- attr(training, "scaled:center")
trainSD   <- attr(training, "scaled:scale")

# Now scale the validation set
validation <- (validation - trainMean) / trainSD

# The result won't exactly have a mean of 0 since values were from the training set
summary(validation)

### MODEL
# Select optimal clusters using the elbow method
pElbow <- fviz_nbclust(training, kmeans, method = "wss")
pElbow

# Could be 3-4
pElbow + geom_vline(xintercept = 4, linetype = 2) 

# Apply k-means clustering algorithm
nClusters <- 3
kmeansFit <- kmeans(training, centers = nClusters) # Adjust the centers based on the elbow plot

### ASSESS
# Cluster assessment
clusterInfo <- data.frame(cluster = 1:nClusters,
                          size = kmeansFit$size,
                          withinss = kmeansFit$withinss)

print(clusterInfo)

# Visualize the clusters
fviz_cluster(kmeansFit, data = training) +
  ggtitle("Clustering Result") + theme_few()

# Now let's examine any personas; training assignments were returned so let's append
customers <- customerData[-idx,]
customers$clusterN  <- kmeansFit$cluster

# More could be done but we're examining 
aggregate(Age ~ clusterN, customers, mean)
aggregate(Annual.Income..k.. ~ clusterN, customers, mean)
aggregate(Spending.Score..1.100. ~ clusterN, customers, mean)
# Cluster numbers change each time but generally there are young, low income, mid spending,
# older, mid income, low spending & mid age, high income, high spending

# kmeans() doesn't use predict() so we can loop through each validation observation and calculate the distance to each cluster
kmeansFit$centers

# Initializing an empty vector to store cluster assignments
validationClusters <- vector()

# Now iterate through each row of the validation set
for (i in 1:nrow(validation)) {
  
  # Compute Euclidean distance between each data point and the cluster centers
  clusterDistances <- sqrt(rowSums((t(validation[i, ] - kmeansFit$centers))^2))
  
  # assign to the cluster which has the minimum distance (i.e., the nearest cluster)
  validationClusters[i] <- which.min(clusterDistances)
}

# printing the cluster assignments
print(validationClusters)

# Validation Append
vCustomer <- customerData[idx,]
vCustomer$nCluster <- validationClusters
vCustomer

# End
