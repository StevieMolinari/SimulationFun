library(tidyverse)
setwd("/Users/MiSelf2/Desktop/Stats 101 Prob & Infer/Simulations/SimulationFun")

### the birthday problem
## how many people does one need so that the probability of more than two people sharing a birthday 
### is greater than 1/2; forget about leap years and assume uniformity of birthday distribution and
### independence of birthday among people.

#### theoretical answer #### 
## probability that two people do not share the same birthday is 
364/365

## probability that three people do not share the same birthday is 
(364/365)*(363/365)
## 364/365 is the probabily of the second not having the same birthday as the first and
### 363/365 is the probabily of the third not having the same birthday as the first and second
### do you see where we use independence/uniformity assumptions??

## probability that four people do not share the same birthday is 
(364/365)*(363/365)*(362/365)

## probability that ten people do not share the same birthday is 
prod(364:(365-9))/(365^9)
### notice that we have 9 comparisons going on in a group of ten people 

## a more general solution might look like
nPeople <- 10
propNotShare <- prod( 364:(365 - (nPeople-1)) )/(365^(nPeople-1))
probShare <- 1 - propNotShare

## now we look at this probability for different number of people up to 50
### we vectorize the above code and plot it
nPeopleVec <- 2:50
propNotShareVec <- prod( 364:(365 - (nPeopleVec-1)) )/(365^(nPeopleVec-1))
probShareVec <- 1 - propNotShareVec

plot(nPeopleVec, probShareVec, type = "l")

#### TO DO ####
## fix this code with the map function

## follow up questions
### should we be concerned with computational limits (365^50 is quite large)?
### how could we use a for loop to do this itertively ?
### what are other ways of coding this up?
### pbirthday function in R?

#### simulated answer #### 
## some preliminary work

#### TO DO ####
## decypher and comment this code

iNPeople <- 10

bdays <- sample(365, size = iNPeople, replace = TRUE)
unique(bdays)

length(bdays) == length(unique(bdays))
iNPeople == length(unique(bdays))

## simulation time
nPeopleMAX <- 50
probShareSimVec <- numeric(nPeopleMAX)
nSims <- 10^4


for(iNPeople in 1:nPeopleMAX){
  isSharingVec <- logical(nSims)
  for(jSim in 1:nSims){
    bdays <- sample(365, size = iNPeople, replace = TRUE)
    isSharingVec[jSim] <- iNPeople != length(unique(bdays))
  }
  probShareSimVec[iNPeople] <- mean(isSharingVec)
}

plot(probShareSimVec, type = "l")
abline(1/2, 0, col = 2, lty = 2)

min(which(probShareSimVec > 1/2))
