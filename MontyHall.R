#### simulated solution ####
carDoor <- sample(3, 1)
goatDoors <- setdiff(1:3, carDoor)
initialDoorPick <- sample(3, 1)
eligibleRevealDoors <- setdiff(goatDoors, initialDoorPick)
doorRevealed <- sample(eligibleRevealDoors, 1)
switchDoor <- setdiff(1:3, c(initialDoorPick, doorRevealed))

carDoor == initialDoorPick
carDoor == switchDoor

nSims <- 10^4
initialPickWinVec <- logical(nSims)
switchDoorWinVec <- logical(nSims)
for(iSim in 1:nSims){
  carDoor <- sample(3, 1)
  goatDoors <- setdiff(1:3, carDoor)
  initialDoorPick <- sample(3, 1)
  eligibleRevealDoors <- setdiff(goatDoors, initialDoorPick)
  doorRevealed <- sample(eligibleRevealDoors, 1)
  switchDoor <- setdiff(1:3, c(initialDoorPick, doorRevealed))
  
  initialPickWinVec[iSim] <- carDoor == initialDoorPick
  switchDoorWinVec[iSim] <- carDoor == switchDoor
}


mean(initialPickWinVec)
mean(switchDoorWinVec)

### this is wrong. can you find the error?

## we know this is wrong because the events initial pick and swtich winning partion the sample space
### so they should sum to 1.

## to determine where it goes wrong lets generate a solution where they both produce a non-win
set.seed(1)

carDoor <- sample(3, 1)
goatDoors <- setdiff(1:3, carDoor)
initialDoorPick <- sample(3, 1)
eligibleRevealDoors <- setdiff(goatDoors, initialDoorPick)
doorRevealed <- sample(eligibleRevealDoors, 1)
switchDoor <- setdiff(1:3, c(initialDoorPick, doorRevealed))

carDoor == initialDoorPick
carDoor == switchDoor

carDoor 
goatDoors 
initialDoorPick
eligibleRevealDoors 
doorRevealed 
switchDoor

storyDf <- rbind(c("Car", "Goat", "Goat"),
      c("", "", "Initial Pick"),
      c("", "Elligible", ""),
      c("Revealed", "", ""),
      c("", "Switch Pick", ""))
rownames(storyDf) <- c("Reality", "InitialPick", "Revealable", "DoorShown", "SwitchDoor")
storyDf

### from this we see that there is somehting wrong with the doorRevealed variable
sample(eligibleRevealDoors, 1)

### mother fucking sample function I heard about this happening. this is sneaky 

#### TO DO #### 
## find where I read this about the sample function. 


#### quick fix ####

nSims <- 10^4
initialPickWinVec <- logical(nSims)
switchDoorWinVec <- logical(nSims)
doorNames <- c("one", "two", "three")
for(iSim in 1:nSims){
  carDoor <- sample(doorNames, 1)
  goatDoors <- setdiff(doorNames, carDoor)
  initialDoorPick <- sample(doorNames, 1)
  eligibleRevealDoors <- setdiff(goatDoors, initialDoorPick)
  doorRevealed <- sample(eligibleRevealDoors, 1)
  switchDoor <- setdiff(doorNames, c(initialDoorPick, doorRevealed))
  
  initialPickWinVec[iSim] <- carDoor == initialDoorPick
  switchDoorWinVec[iSim] <- carDoor == switchDoor
}

mean(initialPickWinVec)
mean(switchDoorWinVec)