#' Author: Ted Kwartler
#' Date: 10-23-2019
#' Purpose: Non-traditional market investing
#'

# Libs
library(triangle)
library(dplyr)

# Load custom functions
source('~/Documents/Harvard_DataMining_Business_Student/Lessons/I_ConsumerCredit_NonTraditionalInvesting/scripts/Z_cardValues.R')
source('~/Documents/Harvard_DataMining_Business_Student/Lessons/I_ConsumerCredit_NonTraditionalInvesting/scripts/Z_crackPack.R')
source('~/Documents/Harvard_DataMining_Business_Student/Lessons/I_ConsumerCredit_NonTraditionalInvesting/scripts/Z_openBox.R')

# Setwd
setwd("~/Documents/Harvard_DataMining_Business_Student/Lessons/I_ConsumerCredit_NonTraditionalInvesting/data")

# We will use a CSV but...
# Use this URL example to get a recent price list for a set
#https://shop.tcgplayer.com/price-guide/magic/war-of-the-spark
nam<-gsub(' ','-','war of the spark')
paste0('https://shop.tcgplayer.com/price-guide/magic/', nam)

# Data Integrity! Review manually for outliers and correct.  The site headers change so be sure to review and adjust or change the functions to expect different column names!
cards <- read.csv("Guilds_10_16_18.csv" , fileEncoding="latin1")
#cards <- read.csv('IconicMasters_7_25_18.csv')

# Examine
head(cards)

# Single pack simulation; packsPerFoil is usually 6 depends on set
crackPack(cards, packsPerFoil = 6)

# Get another pack
onePack <- crackPack(cards,packsPerFoil = 6)

# Simulate market valuation at the pack level
cardValues(onePack, worthlessCommons = T, verbose = F)

# Simulate market valuation at the card level
cardVals <- cardValues(onePack, worthlessCommons = T, verbose = T)
cardVals
sum(cardVals$TCGdistPrice)

# Let's open a box of booster packs; usually 36 but not always
simBox <- openBox(cards, 
                  numPacks       = 36, 
                  packsPerMythic = 8, 
                  packsPerFoil   = 6,
                  foilsInSet     = T)

# What is the expected return for a complete booster box?
(boxReturn <- cardValues(simBox))

# What about opening 40 boxes (1440 packs)
nBox <- 40
boxSim <-list()
for (i in 1:nBox){
  boxes <- openBox(cards, 
                   numPacks       = 36, 
                   packsPerMythic = 8, 
                   packsPerFoil   = 6, 
                   foilsInSet     = T)
  boxes <- cardValues(boxes, verbose = T)
  nam <- i
  boxes$boxNum <-i
  print(paste('opened box',i))
  boxSim[[nam]] <- boxes
}

# Organize the verbose outcome
boxSim <- do.call(rbind, boxSim)

# See what verbose=T in the loop does
boxSim[1:10,]

# Sum individual box return
indBoxes <- aggregate(boxSim$TCGdistPrice, 
                      by  = list(boxSim$boxNum), 
                      FUN = sum)
head(indBoxes)

# Get the average return
boxAVG <- mean(indBoxes[,2])

# Plot and Review 
boxPrice <- 83.5 #$190 for Iconic Masters & $83.50 for Guilds, Throne
hist(unlist(indBoxes[,2]), main='Iconic Masters')
abline(v=boxPrice,col="red")
text(boxPrice,10,'cost', col='red', pos=1,srt=90, cex=1)
abline(v=boxAVG,col="blue")
text(boxAVG,10,'AvgReturn',col='blue', pos=1,srt=90, cex=1)

# Percent of boxes below cost
length(subset(indBoxes[,2],indBoxes[,2]<=boxPrice)) / nBox

# End
