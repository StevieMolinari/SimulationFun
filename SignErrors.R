#### Sign error problem ####
### posted to twitter by Bill Chen @billchenpoker on 2017-11-30

"A student can make a sign error independently in any step in a math problem. [If] each sign flip is less than 50% to occur, and an even number of sign flips gets you the right sign, does the student necessarily have over a 50% chance of getting the right sign?"

# P(p_flip) ~ U[0, 0.5]
# The below will NOT include 0.5, as stated in the problem,
# as the docs say "'runif' will not generate either of the
# extreme values unless 'max = min' or 'max-min' is small
# compared to 'min'". So that's good
p.sign.flip <- runif(n = 1, min = 0, max = 0.5)

# sign flip ~ Bin(n, p_flip)
# simulate many "math problem"s with n=2, 5, 10 steps:
# for each one the statistic is T = whether #{1 in Bin(n, p)} is even
sign.flip.2 <- rbinom(n = 1e6, size = 2, prob = p.sign.flip)
sign.flip.5 <- rbinom(n = 1e6, size = 5, prob = p.sign.flip)
sign.flip.10 <- rbinom(n = 1e6, size = 10, prob = p.sign.flip)

p.sign.flip.2_hat <- sum(sign.flip.2 %% 2 == 0) / length(sign.flip.2)
p.sign.flip.5_hat <- sum(sign.flip.5 %% 2 == 0) / length(sign.flip.5)
p.sign.flip.10_hat <- sum(sign.flip.10 %% 2 == 0) / length(sign.flip.10)

# Results
# For Binomial(n = 2, p = 0.4007): 0.5199
# For Binomial(n = 5, p = 0.4007): 0.50009
# For Binomial(n = 10, p = 0.4007): 0.500025
