#' Author: Ted Kwartler
#' Date: Nov 2, 2022
#' Purpose: lp solve example - min
#' This is an example of linear optimization from the book “Modeling and Solving Linear Programming with R” by Jose M. Sallan, Oriol Lordan and Vincenc Fernandez.
#' 
# Premise: We work at a furniture company.  We make chair and stools.  UnitA is a chair with 1 seat, 4 legs, and 1 back.  UnitB is a stool with 1 seat, 3 legs, and no back.  The factory can create new seats, legs and backs from wooden blocks.  Each wooden block yields 10 seats, 20 legs and 2 backs. The factory has to produce 1000 or more units per month to keep workers busy AND at least 1 unit of each item has to be produced in a month.

# Cost= $30∗unitA + $40∗UnitB + $80∗WoodenBlocks

#Define a linear programming model, which minimizes the total cost (the production costs of the two chairs, plus the buying of new wood blocks) while still allocating 1000 units so the factory isn't idle.

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(lpSolve)

# Cost Function 
# UnitA and UnitB production reduce inventory, woodenBlock usage increases inventory because it is made into seat backs and legs; but they still cost money so are in the cost function
costFunction <- data.frame(item = c('unitA','unitB','woodenBlock'),
                           cost = c(30, 40, 80))
costFunction

# Inventory & characteristics
inventory <- data.frame(item   = c('seats','legs','back','minAllocation'), 
                        onHand = c(500, 200, 100, 1000))
inventory

# Constraints
#UnitA = 1 seat, 4 legs, 1 back, must make at least 1 chair
#UnitB = 1 seat, 3 legs, 1 back, must make at least 1 chair
#woodenBlock = CREATES 10 seats, 20 legs and 2 backs, don't have to buy any blocks
# **since this is a "min" problem; "creating" inventory means negating its use [switches sign]** 
constraintDF <- data.frame(unitA       = c(1,4,1,1),
                           unitB       = c(1,3,0,1),
                           woodenBlock = c(-10,-20,-2,0),
                           direction   = c("<=", "<=", "<=", ">="))
rownames(constraintDF) <- c('seats','legs','back', 'minAmt')
constraintDF

# Linear solution
lpSolution <- lp("min", costFunction$cost,
                 constraintDF[,1:3],
                 constraintDF$direction, 
                 inventory$onHand, 
                 all.int = T)

# What should be built/how many wooden blocks will it take?
lpSolution$solution 

# Total Cost
# unitA = 30 *420
# unitB = 40 *580
# blocks = 161 *80
sum(lpSolution$solution * costFunction$cost)

# Left over inventory
addedInv <- -1*(lpSolution$solution[3] * constraintDF$woodenBlock) #wooden block inputs
totalInv <- (addedInv + inventory$onHand)[1:3]

unitAneeds <- lpSolution$solution[1]*constraintDF[1:3,1]
unitBneeds <- lpSolution$solution[2]*constraintDF[1:3,2]
totalInv - (unitAneeds + unitBneeds) #looks like legs are the limiting input

# End
