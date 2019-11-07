#' Author: Ted Kwartler
#' Date: 11-5-2018
#' Purpose: Rewrite the C14 Example

# Libs
library(arules)

# WD
setwd("/cloud/project/Lessons/J_CollaborativeFiltering_RecoEngines/data")
      
# Data
faceDF <- read.csv("Faceplate.csv")

# Examine
faceDF

# Drop Transaction ID & convert to matrix
faceMat <- as.matrix(faceDF[, -1])

# Examine
faceMat

# Change Obj class; but apriori can accept a DF if discretized
faceTrans <- as(faceMat, "transactions")

# See the item sets for actual transactions
inspect(faceTrans)

# Apply aprior algo
faceRules <- apriori(faceTrans, 
                  parameter = list(supp = 0.2, 
                                   conf = 0.5, 
                                   target = "rules"))

# Inspect the first six rules, sorted by their lift
inspect(head(sort(faceRules, by = "lift"), n = 6))

# Extract the rules to a useable format and subset example
#rulesDF <- inspect(faceRules) #author method
rulesDF  <- as(faceRules, 'data.frame')
rulesDF[rulesDF$support >= 0.04 & rulesDF$confidence >= 0.7,]

# Clear workspace
rm(list=ls())

# Now work on a larger, more realistic dataset
books <- read.csv('CharlesBookClub.csv')

# Subset to Transaction level
books <- books[,8:18]

# Examine
head(books)

# Discretize into binary outcome; discretize function is cumbersome & error prone
books <- ifelse(books > 0,1,0)

# Incidence Matrix class obj
books <- as(books, 'transactions')

# Vizualize, note sum >1 bc concurrent purchases
itemFrequencyPlot(books)

# Apriori control parameters
ctrl <- list(supp= 200/4000, 
             # how many times *minimum* should the rule appear 
             conf = 0.5, 
             # for a specific antecedent, at least 50% of the time the specific consequent must be found
             target = "rules") # return rules

# Apply Apriori
booksApr <- apriori(books, parameter = ctrl) 

# inspect the rules in a useful format
booksApr <- as(booksApr, 'data.frame')
booksApr <- booksApr[order(booksApr$lift, decreasing = T),] # Reorder by lift

# Examine
head(booksApr, 10)

# Top row
# 65  {ChildBks,CookBks,GeogBks} => {YouthBks} 0.06325  0.5776256 2.424452   253
# 65th rule: 
# Antecedent: If someone buys children's, cooking and geography books, 
# Consequent: then suggest a youth book.
# support (% of time the transaction appeared): 253 / 4000 = .06325
# confidence (when the transaction appeared how many times did the consequent appear): .577 
# lift (confidence/(benchmark confidence): 
# *benchmark confidence is the number of time youth books appeared as the consequent naturally) 2.42 better than random youth book purchases.

# End