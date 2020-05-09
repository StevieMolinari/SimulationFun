#### CumSumSamping ####
# randomly draw from a unifrom (0,1), add the entries until you pass 1. what is the expected value?

# a draw of 10 uniforms
runif(n = 10)

# cumsum() is cumulative sum
cumsum(runif(10))

# a logical comparison
cumsum(runif(10)) >= 1

# which returns indicdes which have logical value true
which(cumsum(runif(10)) >= 1)

# R is a "1-index" language (rather than a zero index)
which(c(TRUE, TRUE, FALSE, TRUE))

# min is minimum
min(which(cumsum(runif(10)) >= 1))

# this is one instnce; let's do it 15,000 times; not preallocatins space in nDrawsVec
nSims <- 150000
nDrawsVec <- numeric(0)

for(iSim in 1:nSims){
  nDrawsVec <- c(nDrawsVec, min(which(cumsum(runif(10)) >= 1)))
}

mean(nDrawsVec)


# now with preallocating 
nSims <- 150000
nDrawsVec <- numeric(nSims)

for(iSim in 1:nSims){
  nDrawsVec[iSim] <- min(which(cumsum(runif(10)) >= 1))
}

mean(nDrawsVec)


#### Other matters ####
## a period in a numeric is a decimal point
2.3
2.3>2
2.3<3

## this doesn't work
2.3.3

## a period in a characters is a period
"this is a character string"
c("this", "is", "a", "char", "vec")
length(c("this", "is", "a", "char", "vec"))

"This is a sentence. This is another."

## a period in a name is a period

randomVector1 <- rnorm(10)
randomVector1
randomVector1.1 <- rnorm(10)
randomVector1.1


## naming conventions
lowerCaseCamelNamingConvention <- 42
CapitalCamelConv <- 43
this.is.snake.convention.with.a.period <- 44
this_is_underscore_snake <- 45

random.draw <- function(n = 10){
  min(which(cumsum(runif(n)) >= 1))
}

random.draw(n = 20)

