setwd("/Users/MiSelf2/Desktop/Stats 101 Prob & Infer/Simulations/SimulationFun")
library(tidyverse)

# nSixSided <- 12
# nEightSided <- 1
# nTenSided <- 1
# nTwelveSided <- 1
# nDice <- nSixSided + nEightSided + nTenSided + nTwelveSided

nSidedVector <- c(6, 8, 10, 12)
nNumDiffSided <- length(nSidedVector)
nDicePerSidedVector <- c(12, 1, 1, 1)
nDice <- sum(nDicePerSidedVector)


# 
# prod(((nSidesVector-1)/nSidesVector) ^ nDicePerSideVector)
# (5/6)^nSixSided * (7/8)^nEightSided * (9/10)^nTenSided * (11/12)^nTwelveSided

## We label the dice from 1:nDice in increasing order according to number of sides. 
### Dice with the same number of sides are indistinguishable
### e.g. 1 = 1st 6-sided, 2 = 2nd 6-sided, ... , 12 = 12th 6-sided, 13 = 1st 8-sided, 
### ### 14 = 1st 10-sided, 15 = 1st 12 sided.

## the indicator set contains the dice still "in play" (i.e. that have not been removed)
### it's initialized as 1:nDice since all dice are in play at the beginning of the game
## the faces vector is a vector of the max face for each die in order


indicatorSet <- 1:nDice
facesVector <- integer(0)
for(iSided in 1:nNumDiffSided){
  facesVector <- c(facesVector, rep(nSidedVector[iSided], nDicePerSidedVector[iSided]))
}
maxScore <- sum(facesVector)

## a roll for a particulart is a vector of numbers,  the same length as the indicator set, 
### with a single sampled value from the possible faces of the remaining dice

sample.dice <- function(faceVec, indVec){
  faceVec[indVec] %>% 
    map(sample, size = 1) %>% 
    unlist()
}

## a roll of all the dice
sample.dice(faceVec = facesVector, indVec = 1:nDice)

## a roll of all the dice 3rd 4th 7th, and 15th dice
sample.dice(faceVec = facesVector, indVec = c(3, 4, 7, 15))



## test code ##
# toyFaceVec <- c(6, 6, 8, 10, 22)
# toyIndVec <- c(1, 4, 5)
# unlist(map(toyFaceVec[toyIndVec], sample, size = 1)) 
## more test code ##
# toyInd <- sort(indicatorSet[sample(nDice, size = 10, replace = FALSE)])
# roll = sample.dice(faceVec = facesVector, indVec = toyInd)
# toyInd
# rbind(roll, facesVector[toyInd])
# which(roll == facesVector[toyInd])

#### Let's play ####
## a game consists of a list of turns
### each turn consists a list of two vectors; the inPlayDice and outOfPlayDice vectors
### the inPlayDice are the max faces of the dice still in play
### the outOfPlayDice are the dice removed with the value the were remove in the order they were remove


## the strategy implemented will be to remove all maximum valued dice
### if a roll occurs where there are no maximum dice, then we remove the next best dice
### where best is measured as the minimum difference from the max and ties go to the larger faced dice
### e.g. 11 from a 12-sided die is better than 5 from a 6-sided die
### e.g. 5 from a 6-sided die is better than 10 from a 12-sided die

### we call this strategy "max-min-diff"

## a function that implements this strategy
biscuit.max.min.diff.game <- function(){
  best.die.index <- function(roll, faces){
    diffVec = faces - roll
    minDiff = min(diffVec)
    max(which(diffVec == minDiff))
  }
  
  
  indicatorSet <- 1:nDice
  inPlayDice <- facesVector[indicatorSet]
  outOfPlayDice <- numeric(0)
  # gameList <- list()
  
  while(length(indicatorSet) > 0){
    roll = sample.dice(faceVec = facesVector, indVec = indicatorSet)
    maxDiceInds = which(roll == facesVector[indicatorSet])
    if(length(maxDiceInds) > 0 ){
      outOfPlayDice <- c(outOfPlayDice, roll[maxDiceInds])
      inPlayDice <- inPlayDice[-maxDiceInds]
      indicatorSet <- indicatorSet[-maxDiceInds]
    } else{
      bestDieInd <- best.die.index(roll = roll, faces = facesVector[indicatorSet])
      outOfPlayDice <- c(outOfPlayDice, roll[bestDieInd])
      inPlayDice <- inPlayDice[-bestDieInd]
      indicatorSet <- indicatorSet[-bestDieInd]
    }
  }
  
  gameScore = sum(outOfPlayDice)
  minusGameScore = gameScore - maxScore
  
  return(c(gameScore, minusGameScore))
}

biscuit.max.min.diff.game()

nSimGames <- 10^4
minusScoreVec <- numeric()
for(iSimGame in 1:nSimGames){
  minusScoreVec[iSimGame] <- biscuit.max.min.diff.game()[2]
}

mean(minusScoreVec)



#### TO DO ####
### investigate the game with 2 six-sided dice and 1 twelve-sided die
