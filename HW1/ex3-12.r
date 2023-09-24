# Exponential-Gamma mixture
n <- 1000
r <- 4
beta <- 2
lambda <- rgamma(n,r,beta)
x <- rexp(n,lambda)