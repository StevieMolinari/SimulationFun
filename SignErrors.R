#### Sign error problem ####
library(tidyverse)
### posted to twitter by Bill Chen @billchenpoker on 2017-11-30

"A student can make a sign error independently in any step in a math problem. [If] each sign flip is less than 50% is less than 50% to occur, and an even number of sign flips gets you the right sign. Does the student necessarily have over a 50% chance of getting the right sign"

probError <- 1/3
nSteps <- 11
probNErrors <- dbinom(0:nSteps, nSteps, probError)
evenNumMistakesProbs<- probNErrors[seq(0, (floor(nSteps/2)*2), by = 2) + 1]

sum(evenNumMistakesProbs)
