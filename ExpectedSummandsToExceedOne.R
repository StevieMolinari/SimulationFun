#### PROBLEM STATEMENT ####
### 1958 Putnam Exam Problem 3A ###
## Real numbers are chosen at random from the interval [0,1]. 
### Continue choosing real numbers until the sum of the chosen numbers exceeds 1. 
### What is the expected value for the number of real numbers chosen?

#### Preliminaries ####
## magrittr has the pipes '%>%' 
### i.e. f(x, y) <=> x %>% f(y) or x %>% f(y) %>% g(z) <=> g(f(x,y),z) 
### shortcut cmmd + shift + M
library(magrittr)

#### pretty pics ####
## plots of $X_n = \sum_{i=1}^n U_i$ for i = 1:10
nDraws <- 10
nSims <- 10^4
randomSamples = numeric(nSims)
for(iDraws in 1:nDraws){
  randomSamples = randomSamples + runif(nSims)
  hist(randomSamples, main = paste0("nDraws = ", iDraws))
}

### better plots??
# iDraws = 3
randomSamples = numeric(nSims)
for(iDraws in 1:nDraws){
  randomSamples = randomSamples + runif(nSims)
  nLessThanOne = sum(randomSamples<1)
  propLessThanOne = nLessThanOne/nSims
  hist(randomSamples, freq = FALSE, 
       main = paste0("nDraws = ", iDraws, ": P(X_n < 1) ~ ", round(propLessThanOne,2)),
       xlab = "X_n", ylab = "P(X_n = x)")
}


#### more sims/more numbers ####
nDraws = 10
randomDraw = runif(nDraws)
randomDraw
# sum(randomDraw)
cumsum(randomDraw)
cumsum(randomDraw) < 1
which(cumsum(randomDraw) < 1)
# min(which(cumsum(randomDraw) < 1)
max(which(cumsum(randomDraw) < 1))

## function that takes in numeric vector and returns piecewise evaluated logical vector of same size
geq.to.one <- function(x){x>=1}

## function that takes in a numeric vector (in our case uniform zero ones)
first.sum.geq.to.one <- function(x){
  x %>% cumsum() %>% geq.to.one() %>% which() %>% min()
}  
### CUMulative SUM cumsum returns a numeric vector c and returns %c_k = \sum_1^k u_i$,
### geq.to.one does its ^^^ thing, 
### which() takes in a logical and provides a numeric vector of indices,
### min takes in a (numeric) vector and provides the min (singleton numeric) value  


### let's go
nDraws <- 10
nSims <- 10^5
randomSampleMatrix <- matrix(runif(nSims*nDraws), ncol = nDraws)
summandsToExceedOne <- apply(randomSampleMatrix, 1, first.sum.geq.to.one)
hist(summandsToExceedOne) 
## for the life of me, i don't know why that histogram is squishing together the first two like that

#### a homemade histogram  of the pdf ####
heightsVec <- table(summandsToExceedOne)/nSims
nRects <- length(heightsVec)
class(heightsVec)
class(heightsVec[1])

plot(c(0, 0), pch = "", 
     xlim = c(1.5, nRects+ 1/2), ylim = c(0, max(heightsVec)),
     xlab = "P(X = x)", ylab = "x", main = "PDF of X")
rect(xleft = 2:nRects - 1/2, ybottom = 0, xright = 2:nRects + 1/2, ytop = heightsVec,
     col = "dodgerblue")

## i like mine better


### guesses at their expected value (*SHHHH*)
# mean(summandsToExceedOne)
# exp(1)

# investigating the proportions
table(summandsToExceedOne)/nSims

### *SSSHHHHH* ###
# 1/(factorial(0:7)*(2:9))


#### numerically checking the cdf ####
### what is the probability that n iid u(0,1) sum to less than 1
sums.geq.to.one <- function(x){
  x %>% cumsum() %>% geq.to.one()
}  
rowCumSummedRandSampMat <- apply(randomSampleMatrix, 1, cumsum) %>% t()
# dim(randomSampleMatrix)
# dim(rowCumSummedRandSampMat)
rowCumSummedRandSampMat[1,] 

probNSummandsGeqToOne <- rowCumSummedRandSampMat %>% geq.to.one() %>% colMeans()
probNSummandsLeqToOne <- 1 - probNSummandsGeqToOne
probNSummandsLeqToOne
1/factorial(1:9)

### Theorem: P(X_n) = 1/n! ####
## proof by induction depends on the following: for any a<1, p(X_n < a) = a^n/n!

## can we check this lemma?
### duhh. the probability that n iid u(0,1) sums to less than a <= 1 is a^n/n!
geq.to.a <- function(x, a){x>=a}
anA <- 1/4
probNSummandsGeqToA <- rowCumSummedRandSampMat %>% geq.to.a(a = anA) %>% colMeans()
probNSummandsLeqToA <- 1 - probNSummandsGeqToA
probNSummandsLeqToA
(1/factorial(1:9)) * anA^(1:9)


### does this show, that this only holds for a<1?


