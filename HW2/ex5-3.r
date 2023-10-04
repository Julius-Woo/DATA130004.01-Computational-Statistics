# Monte Carlo estimate from Uniform(0, 0.5).
m <- 10000
u <- runif(m,0,0.5)
g <- exp(-u)
theta <- 0.5*mean(g)
theta
1 - exp(-0.5)
# estimate variance1
var1 <- 0.5*0.5* mean((g-mean(g))^2)/m
var1

# sampling from the exponential distribution
m <- 10000
v <- rexp(m,1)
theta <- mean(v<=0.5)
theta
p <- 1 - exp(-0.5)
p
# estimate variance2
var2 <- theta*(1-theta)/m
var2
accvar <- p*(1-p)/m
accvar

# compare
var1/var2
(8*exp(-0.5)-5*exp(-1)-3)/4/p/(1-p)
