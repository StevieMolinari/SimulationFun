#### Sign error problem ####
library(tidyverse)
### posted to twitter by Bill Chen @billchenpoker on 2017-11-30

"A student can make a sign error independently in any step in a math problem. [If] each sign flip is less than 50% is less than 50% to occur, and an even number of sign flips gets you the right sign. Does the student necessarily have over a 50% chance of getting the right sign"

# prob.correct <- function(probStepError, nSteps){
#   evenNumErr <- seq(0, nSteps - (nSteps %% 2), by = 2)
#   sum(dbinom(evenNumErr, nSteps, probStepError))
# }

prob.correct <- function(pNVec){
  evenNumErr <- seq(0, pNVec[2] - (pNVec[2] %% 2), by = 2)
  sum(dbinom(evenNumErr, pNVec[2], pNVec[1]))
}

distribution.normalizer <- function(x){x/sum(x)}

pPriorSupport <- 1:999/2000
pPriorProb <- 1/length(pPriorSupport)
nPriorSupport <- 1:25
nPriorProb <- dpois(nPriorSupport, 5) %>% distribution.normalizer()

supportGrid <- expand.grid(pPriorSupport, nPriorSupport)
probGrid <- expand.grid(pPriorProb, nPriorProb)

(apply(supportGrid, 1, prob.correct)*apply(probGrid, 1, prod)) %>% sum()
(apply(supportGrid, 1, prob.correct)*probGrid[,1]*probGrid[,2]) %>% sum()


nSims <- 10^6
apply(cbind(runif(nSims, 0, 1/2), rpois(nSims, 5)), 1, prob.correct) %>% mean()




apply(cbind(runif(nSims, 0, 1/2), sample(size = nSims, x = nPriorSupport, prob = nPriorProb, replace = TRUE)), 1, prob.correct) %>%
  mean()

sample(12, size = 1)

