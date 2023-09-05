#' Author: Ted Kwartler
#' Date: 11-4-2022
#' Purpose: Non-traditional market investing
#'

# libraries
library(triangle)
library(readr)
library(devtools)

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# "Open" a pack with the correct card rarities function
#source("~/Desktop/Harvard_DataMining_Business_Student/Lessons/I_nonTraditionalMkt_LPsolve_RealEstate/scripts/Z_crackPack.R")
crackPack <- "https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/I_nonTraditionalMkt_LPsolve_RealEstate/scripts/Z_crackPack.R"
source_url(crackPack)


# Get card values accounting for distribution by rarity type
#source("~/Desktop/Harvard_DataMining_Business_Student/Lessons/I_nonTraditionalMkt_LPsolve_RealEstate/scripts/Z_cardValues.R")
cardValues <- "https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/I_nonTraditionalMkt_LPsolve_RealEstate/scripts/Z_cardValues.R"
source_url(cardValues)

# Card list obtained online
cards <- read_csv("https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/I_nonTraditionalMkt_LPsolve_RealEstate/data/Guilds_10_16_18.csv")
#cards <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/I_nonTraditionalMkt_LPsolve_RealEstate/data/IconicMasters_7_25_18.csv')

# Examine
head(cards)

# "Open" a pack
onePack <- crackPack(cards)

# Value the pack
packVals <- cardValues(onePack, verbose = T)
packVals
sum(packVals$TCGdistPrice)

# Now let's just loop over this operation to simulate opening a box
# Let's open a box of booster packs; usually 36 but not always
eachPackValue <- vector()
for(i in 1:36){
  onePack <- crackPack(cards)
  packVals <- cardValues(onePack, verbose = F)
  eachPackValue[i] <- packVals
}

# What was the revenue from the box?
sum(eachPackValue)

# How much profit?
boxPrice <- 83.5
shippingHandling <- 20
boxPrice / (sum(eachPackValue) - (boxPrice+shippingHandling)) 

# There is variability so let's open 100 boxes to understand the likelihood of making money
boxes <- list()
for(j in 1:100){
  cat('box')
  eachPackValue <- vector()
  for(i in 1:36){
    onePack <- crackPack(cards)
    packVals <- cardValues(onePack, verbose = F)
    eachPackValue[i] <- packVals
    
    cat('.')
  }
  boxes[[j]] <- data.frame(boxID = rep(j,36),
                           packID = 1:36,
                           packVal = eachPackValue)
  print(j)
}

# Look at one box
boxes[[1]]

# Change to a data frame
boxes <- do.call(rbind, boxes)

# Aggregate
totalRevenue <- aggregate(packVal~boxID, boxes, sum)

plot(density(totalRevenue$packVal), main='Guilds of Ravnica')
abline(v = boxPrice + shippingHandling, col = "red")
abline(v = mean(totalRevenue$packVal), col = "blue")

# Percent of boxes below cost; 
length(subset(totalRevenue$packVal,
              totalRevenue$packVal <= (boxPrice + shippingHandling))) / nrow(totalRevenue)


# Probability of left of Red assuming normal distribution, which it isnt quite.
#pnorm gives you the percentage of values greater than or less than a value on the x-axis
redLeft <- ((boxPrice + shippingHandling) - mean(totalRevenue$packVal)) / sd(totalRevenue$packVal)
pnorm(redLeft)
pnorm(q    = (boxPrice + shippingHandling), 
      mean = mean(totalRevenue$packVal),
      sd   = sd(totalRevenue$packVal))
# End
