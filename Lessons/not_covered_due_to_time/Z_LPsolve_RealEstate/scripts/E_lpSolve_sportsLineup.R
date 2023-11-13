#' Author: Ted Kwartler
#' Date: Nov 2, 2022
#' Purpose: lp solve example - Sports Example
#' 

# Wd

#lib
library(readr)
library(lpSolve)

# Suppose you built a model to predict points for football players for a given week
playerInfo <- read_csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/I_nonTraditionalMkt_LPsolve_RealEstate/data/playerInfo.csv')
playerInfo <- as.data.frame(playerInfo)
head(playerInfo)
table(playerInfo$Position)

# Just pick the minimum defense by reducing its amount from the salary budget
minDSTsal <- 2400

# Now apply the lineup constraints; here we use model.matrix to make the matrix according to the levels in "Position" as a bunch of 1/0s like dummy variables.  Then another row for playerCount [all players are a single roster spot], and finally their salary cost
inputMat <- rbind(t(model.matrix(~ Position + 0,playerInfo)), 
                rep(1, nrow(playerInfo)), 
                playerInfo$Salary)
rownames(inputMat)[(nrow(inputMat)-1):nrow(inputMat)] <- c('playerCount', 'Salary')
colnames(inputMat) <- make.names(playerInfo$Name)

# Examine what we made 
inputMat[1:6,1:15]

# Now the constraint matrix
lineupDF <- data.frame(position = c('QB',
                                    'RB',  'TE',
                                    'WR',  'totalPlayersLineup', 
                                    'maxSalary'),
                       lineupNeed = c(1,3,1,4,8, (50000-minDSTsal)),
                       directionConstraints = c('=','<=',
                                                '=','<=','=','<='))
lineupDF # described by the league, qty at each position, total roster size, and money

lpSolution <- lp('max', 
                 playerInfo$points, 
                 inputMat, 
                 lineupDF$directionConstraints, 
                 lineupDF$lineupNeed, 
                 all.bin = TRUE) 
lpSolution

# Optimal lineup given the point projection accuracy and constraints
playerInfo[which(lpSolution$solution == 1),]

# End