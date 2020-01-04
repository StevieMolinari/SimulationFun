#### What is the probability that a random quadratic factors #### 
library(tidyverse)

nCoeffs <- 10
coeffRange <- 0:(nCoeffs - 1)
# coeffRange <- 1:nCoeffs

## a quadratic ax^2 + bx + c factors if its discriminant b^2 - 4ac is a perfect square
### to estimate this we iterate over all possible options for a, b, and c in our coefficient range
counter <- 0
perfectSquares <- (0:100)^2
for(iA in coeffRange){
  for(jB in coeffRange){
    for(kC in coeffRange){
      if(jB^2 - 4*iA*kC %in% perfectSquares){counter = counter + 1}
    }
  }
}

## 92.4% no way!







coeffDf <- 
  tibble(
    ACoeff = rep(rep(0:9, each = nCoeffs), each = nCoeffs),
    BCoeff = rep(rep(0:9, each = nCoeffs), times = nCoeffs),
    CCoeff = rep(rep(0:9, times = nCoeffs), times = nCoeffs)
  ) %>% 
  mutate(
    Discriminant = BCoeff^2 - 4*ACoeff*CCoeff,
    Factorable = Discriminant %in% perfectSquares
  )

coeffDf %>% summarise(mean(Factorable))

## this says 25%. curious

coeffDf %>% 
  filter(
    ACoeff == 0
  ) %>% 
  summarise(
    mean(Factorable)
  )
  

  