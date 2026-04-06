# MTG BloomBurrow
# Nov 5, 2025
# TK

# libraries
library(triangle)
library(dplyr)
library(devtools)
library(dplyr)
library(readr)

# "Open" a pack with the correct card rarities function
crackPack <- "https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/4f6c4f20b2980066132e80adf64e13a6b506d5fc/Lessons/J_CreditModeling/scripts/Z_crackPack.R"
source_url(crackPack)
crackPack

# Get card values accounting for distribution by rarity type
cardValues <- "https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/4f6c4f20b2980066132e80adf64e13a6b506d5fc/Lessons/J_CreditModeling/scripts/Z_cardValues.R"
source_url(cardValues)
cardValues

# Bring in the bloomborrow
# https://www.mtggoldfish.com/sets/Bloomburrow#online ; table top pricing!
# https://mtg.dawnglare.com/?p=sets
# https://mtg.dawnglare.com/?p=beta
cards <- read.csv('https://raw.githubusercontent.com/kwartler/Vienna_24/refs/heads/main/Fall_2024/day1/data/bloomburrow_11_5_2024.csv')

# Since things have changed since originally written we have some changes to make so the functions work:
# Column name rarity to R
names(cards)[2] <- 'R'

# Clean up the data and create the high/low
cards$Med <- parse_number(cards$price)

# Range - just guessing at variability
cards$High <- cards$Med  + abs(parse_number(cards$weekly_chg))
cards$Low <- cards$Med  -  abs(parse_number(cards$weekly_chg))


# Select a pack
onePack <- crackPack(cards,
                     packsPerMythic = 8, 
                     packsPerFoil = 1, 
                     foilsInSet = T)

# Now get the value
packVals <- cardValues(onePack, verbose = T)
packVals

# Let's open a box!
eachPackValue <- vector()
for(i in 1:36){
  onePack <- crackPack(cards)
  packVals <- cardValues(onePack, verbose = F)
  eachPackValue[i] <- packVals
}
sum(eachPackValue)

# Retail Price $133 at dacardworld.com Nov 2024
# End