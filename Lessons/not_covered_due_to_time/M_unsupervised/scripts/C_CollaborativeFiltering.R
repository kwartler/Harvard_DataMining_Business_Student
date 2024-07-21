#' Author: Ted Kwartler
#' Date: Dec 3-2023
#' Purpose: Collaborative Filtering Example

### RecoLab 0.2-6 has some unusual code, in the UBCF so instead install the older version
#pth <- '/cloud/project/Lessons/M_Ethics_RecoEngine/recommenderlab_0.2-5.tar.gz'
#install.packages(pth, repos = NULL, type="source")
# OR
# urlGZ <- "https://github.com/kwartler/Harvard_DataMining_Business_Student/raw/master/Lessons/M_unsupervised/recommenderlab_0.2-5.tar.gz"
# destFile <- "recommenderlab_0.2-5.tar.gz"
# download.file(urlGZ, destFile, mode = "wb")
# install_local("recommenderlab_0.2-5.tar.gz", force = TRUE)

# Libraries
library(fst)
library(dplyr)
library(recommenderlab)
library(reshape2)

# wd
setwd("/Users/edwardkwartler/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Raw Data In & EDA
songListens <- read_fst('~/Desktop/Harvard_DataMining_Business_Student/Lessons/M_unsupervised/data/10000Songs.fst')
songData    <- read_fst(unzip(file.choose())) #interactive file selection with an unzip!

# Examine Song Information
head(songData)

# How many unique songs are there
length(unique(songData$song_id))

# Examine Listening Behavior
head(songListens, 10)
nlevels(songListens$userID) #76klisteners
nlevels(songListens$song_id) #10k unique songs (toy data reduction)
summary(songListens$listens)
plot(density(songListens$listens))

# Oh my!
subset(songListens, songListens$listens==2213)
subset(songData, songData$song_id=='SOFCGSE12AF72A674F')
#https://www.youtube.com/watch?v=OgkoiFwI5eM 3:32
#((212*2213)/60)/24 = 325 days?

# Just get first 1000 unique users & their listens for example; otherwise will have to run for longer!
sampUsers   <- unique(songListens$userID)[1:1000]
songListens <- songListens[songListens$userID %in% sampUsers,]

# Cast i.e. pivot the data
affinityMatrix <- acast(songListens, userID ~ song_id)

# Examine
dim(affinityMatrix)
affinityMatrix[50:53,108:110]

# Let's look up the song in the song dictionary
subset(songData, songData$song_id=='SOAJWRM12A8C13CF2B')

# So user 0ec9cc33028dff6209aa49bf645ef64bdcbe00fc listened to song SOAJWRM12A8C13CF2B six times

# Convert to reco lab class
affinityMatrix[50:53,108:110]
affinityMatrix <- as(affinityMatrix,"realRatingMatrix")

# Construct a User-Based-Collaborative-Filter
recModel <- Recommender(affinityMatrix, method = "UBCF")

# Get #90 to 100 users for example
user <- affinityMatrix[sampUsers[90:100],] #103:106
head(as(user, 'data.frame'))

# recommended top 5 items for each user 
recommendedItems <- predict(recModel, user, n=5)

# Chg to List 
recoList <- as(recommendedItems, "list")

# Examine
recoList

# Append the original user IDs
as.character(sampUsers[90:100])
names(recoList) <- sampUsers[90:100]

# Now let's compare the user listens to the recommendations
userIdx <- 9 #1-11 since we subset to some users earlier
oneUserRecos <- unlist(recoList[[userIdx]])
oneUserListens <- subset(songListens, songListens$userID==names(recoList)[userIdx])
oneUserRecos
oneUserListens

# Now append song names to both
left_join(oneUserListens, songData) # what did they listen too
songData[songData$song_id %in% oneUserRecos,] # what was recommended


# End