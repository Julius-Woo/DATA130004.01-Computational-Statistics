# estimate θ by the antithetic variate approach
m <- 10000
u <- runif(m/2)
v <- 1-u
g1 <- (exp(-u)/(1 + u^2)+exp(-v)/(1 + v^2))/2
anti <- mean(g1)
v1 <- mean((g1-anti)^2)/(m/2)

# estimate θ by the simple Monte Carlo method
u2 <- runif(m)
g2 <- exp(-u2)/(1 + u2^2)
smc <- mean(g2)
v2 <- mean((g2-smc)^2)/m

c(anti, smc)
c(v1,v2)
# reduction
(v2 - v1)/v2