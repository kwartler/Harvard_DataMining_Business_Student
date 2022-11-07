#' Author: Ted Kwartler
#' Date: Nov 2, 2022
#' Purpose: lp solve example - max
#' 

# WD
setwd("~/Desktop/Harvard_DataMining_Business_Student/personalFiles")

# Libs
library(lpSolve)


# Lets set up our business **profit function**
profitFunction <- data.frame(product = c('p1','p2','p3'),profit = c(1, 9, 1))
profitFunction #which product would we sell just based on this?  Let's see if LP Solve agrees

# Now lets define our product **inputs**
productInputs <- matrix(c(1, 2, 3, 3, 2, 2), nrow=2, byrow=TRUE)
rownames(productInputs) <- c('wires','plastic')
colnames(productInputs) <- c('productA','productB','productC')
productInputs

# Now our input **constraints**, this is our current inventory
currentInventory <- data.frame(inputType = c('wires','plastic'),
                               onHand    = c(9,15),
                               maxUsage  = c('<=','<='))

# Linear solution
lpSolution <- lp("max", profitFunction$profit, 
                 productInputs, 
                 currentInventory$maxUsage, 
                 currentInventory$onHand) # all.int = T parameter for just whole numbers

# What should be built?
lpSolution$solution 

# Which products should be created w/corresponding profit?
profitFunction[which(lpSolution$solution>0),]

# total Profit = solution * number built
#4.5units * 9profit
lpSolution$solution * profitFunction$profit[which(lpSolution$solution>0)]

# Let's check inventory usage to see if we stayed under constraints
# What does it take to make a single productB?
productInputs
productInputs[,which(lpSolution$solution>0)] # just the solution's bill of material

# How many total inputs were used
# 4.5 * 2 wires and 2 plastics
inventoryUse <- lpSolution$solution[which(lpSolution$solution>0)] * 
  as.vector(productInputs[,which(lpSolution$solution>0)])
names(inventoryUse) <- c('wires','plastic')

# How many did we have in stock?  Did LPSolve stay within our constraint?
currentInventory
inventoryUse

# Now a little more complex; 25 products made of 25 inputs
rm(list = ls())
set.seed(1234)
profitFunction <- data.frame(product = paste0('product',1:25),profit = sample(65:105, 25))
profitFunction

# More inputs [15]
set.seed(1234)
productInputs <- matrix(sample(1:15, 625, replace = T), nrow=25, byrow=TRUE)
rownames(productInputs) <- c('laborHours','qualityInspectionHrs','wires','plastic', 'keys', 
                             'switches','LEDs','batteries','bluetoothDevice',
                             'touchpad','powerSupply','fan','hardDrive','cpu',
                             'motherboard','solder','gpu','microphone','camera',
                             'usbPort','thumbprintPad','lcdScreen','audioJack','case','logo')
colnames(productInputs) <- paste0('product',1:25)
productInputs

# Now the current inventory constraints
set.seed(1234)
currentInventory <- data.frame(inputType = c('laborHours','qualityInspectionHrs',
                                             'wires','plastic', 'keys', 
                                             'switches','LEDs','batteries',
                                             'bluetoothDevice','touchpad','powerSupply',
                                             'fan','hardDrive','cpu', 'motherboard',
                                             'solder','gpu','microphone','camera',
                                             'usbPort','thumbprintPad','lcdScreen',
                                             'audioJack','case','logo'),
                               onHand    = sample(205:1005, 25, replace = T),
                               maxUsage  = rep('<=', 25))

# Linear solution
lpSolution <- lp("max", profitFunction$profit, 
                 productInputs, 
                 currentInventory$maxUsage, 
                 currentInventory$onHand,
                 all.int = T)

# What should be built?
lpSolution$solution 

# Which products should be created w/corresponding profit?
profitFunction[which(lpSolution$solution>0),]

# End
