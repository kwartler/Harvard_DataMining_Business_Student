#' Author: Ted Kwartler
#' Date: Apr 26-2020
#' Purpose: Collaborative Filtering Example

# Libraries
library(fst)
library(dplyr)
library(recommenderlab)
library(reshape2)

# wd
setwd("~/Documents/Harvard_DataMining_Business_Admin/lessons/L_Ethics_RecoEngine/data")

# Raw Data In & EDA
songListens <- read_fst('10000Songs.fst')
songData    <- read_fst(unzip(file.choose())) #interactive file selection with an unzip!

# Examine Song Information
head(songData)
#nlevels(as.factor(songData$song_id)) #999056; takes while to run

# Examine Listening Behavior
head(songListens, 10)
nlevels(songListens$userID)
nlevels(songListens$song_id)
summary(songListens$listens)

# Oh my!
subset(songListens, songListens$listens==2213)
subset(songData, songData$song_id=='SOFCGSE12AF72A674F')
#https://www.youtube.com/watch?v=OgkoiFwI5eM 3:32
#((212*2213)/60)/24 = 325 days?

# Just get first 1000 unique users for example
sampUsers   <- unique(songListens$userID)[1:1000]
songListens <- songListens[songListens$userID %in% sampUsers,]

# Cast i.e. pivot the data
affinityMatrix <- acast(songListens, userID ~ song_id)

# Examine
dim(affinityMatrix)
affinityMatrix[50:53,108:110]
subset(songData, songData$song_id=='SOAJWRM12A8C13CF2B')

# Convert to reco lab class
affinityMatrix <- as(affinityMatrix,"realRatingMatrix")

# Construct a User-Based-Collaborative-Filter
recModel <- Recommender(affinityMatrix, method = "UBCF")

# Get #103 to 106 users
user <- affinityMatrix[sampUsers[103:106],]

# recommended top 5 items for each user
recommendedItems <- predict(recModel, user, n=5)

# Chg to List 
recoList <- as(recommendedItems, "list")

# Examine
recoList

# Now what songs are those?
userThreeRecos <- unlist(recoList[[3]])
userThreeRecos
songData[songData$song_id %in% userThreeRecos,]

# What did this listener originally listen too?  Does the reco make sense?
userThreeID <- names(recoList)[3] #"8ee90038724c4957eb4df16f3e9c6ed2b570a3ec"
userThreeID

 
# Subset to a single user then join the data
singleListens <- subset(songListens, songListens$userID == userThreeID)
singleListens

left_join(singleListens, songData)

# End