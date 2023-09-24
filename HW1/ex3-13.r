# Exponential-Gamma mixture and Pareto dist
n <- 1000
r <- 4
beta <- 2
lambda <- rgamma(n,r,beta)
x <- rexp(n,lambda)
hist(x,prob=T)
y <- sort(x)
f_pareto=r * beta^r / (beta + y)^(r + 1)
lines(y,f_pareto)