e#' Author: Ted Kwartler
#' Date: Apr 6 2026
#' Purpose: Ravnica: Murders at Karlov Manor investing simulation
#'

library(httr)
library(readr)
library(triangle)
library(dplyr)
library(devtools)

headers = c(
  `sec-ch-ua-platform` = '"macOS"',
  Referer = "https://www.tcgplayer.com/",
  `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36",
  Accept = "application/json, text/plain, */*",
  `sec-ch-ua` = '"Chromium";v="142", "Google Chrome";v="142", "Not_A Brand";v="99"',
  `sec-ch-ua-mobile` = "?0"
)

params = list(
  rows = "5000",
  productTypeID = "1"
)

res <- httr::GET(url = "https://infinite-api.tcgplayer.com/priceguide/set/23361/cards/", httr::add_headers(.headers=headers), query = params)

x <- content(res)

df <- list()
for(i in 1:length(x[[3]])){
  Name   <- x[[3]][[i]]$productName
  R <- x[[3]][[i]]$rarity
  Med  <- x[[3]][[i]]$marketPrice
  # Range - just guessing at variability
  High <- Med + abs((x[[3]][[i]]$lowPrice))
  Low  <- x[[3]][[i]]$lowPrice
  tmp <- data.frame(Name, `Set.Name` = 'MKM', R, High, Med, Low)
  df[[i]] <- tmp
}
df <- do.call(rbind, df)

# Need to recode R
df$R <- recode_factor(
  df$R,
  "Common" = "C",
  "Land" = "L",
  "Mythic" = "M",
  "Rare" = "R",
  "Token" = "T",
  "Uncommon" = "U")

# There are so many variants, let's simplify for class!
dfUnique <- df[!duplicated(df$Name),]

# More nonsense
drops <- !grepl('Serial Numbered', dfUnique$Name)
dfUnique <- dfUnique[drops, ]

# Since we pulled this data, there are some issues with H,M,L:
fixPriceOrder <- function(df) {

  rowsSwap <- df$Low > df$Med
  
  # 2. Extract the values that need swapping
  lowSwap <- df$Low[rowsSwap]
  medSwap <- df$Med[rowsSwap]
  
  # 3. Perform the swap
  # Assign the old 'Low' values to the 'Med' column in the problematic rows
  df$Med[rowsSwap] <- lowSwap
  # Assign the old 'Med' values to the 'Low' column in the problematic rows
  df$Low[rowsSwap] <- medSwap
  
  # 4. Return the corrected data frame
  return(df)
}

# Apply the function to your data frame
dfUnique <- fixPriceOrder(dfUnique)

### Read in from the teaching data repo; above is shown for reference
dfUnique <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/Murders%20at%20Karlov%20Manor.csv')

# "Open" a pack with the correct card rarities function
crackPack <- "https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/4f6c4f20b2980066132e80adf64e13a6b506d5fc/Lessons/J_CreditModeling/scripts/Z_crackPack.R"
source_url(crackPack)

# Get card values accounting for distribution by rarity type
cardValues <- "https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/4f6c4f20b2980066132e80adf64e13a6b506d5fc/Lessons/J_CreditModeling/scripts/Z_cardValues.R"
source_url(cardValues)

eachPackValue <- vector()
for(i in 1:36){
  onePack <- crackPack(dfUnique)
  packVals <- cardValues(onePack, verbose = F)
  eachPackValue[i] <- packVals
}
#Underground Mortuary
# What was the revenue from the box?
sum(eachPackValue)

# How much profit or loss?
boxPrice <- 100 #inflation
shippingHandling <- 20
sum(eachPackValue) - (boxPrice+shippingHandling) 

# There is variability so let's open 100 boxes to understand the likelihood of making money
boxes <- list()
for(j in 1:100){
  cat('box')
  eachPackValue <- vector()
  for(i in 1:36){
    onePack <- crackPack(dfUnique)
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
boxes[[42]]

# Change to a data frame
boxes <- do.call(rbind, boxes)

# Aggregate
totalRevenue <- aggregate(packVal~boxID, boxes, sum)

plot(density(totalRevenue$packVal), main='Ravnica: Murders at Karlov Manor')
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



# End