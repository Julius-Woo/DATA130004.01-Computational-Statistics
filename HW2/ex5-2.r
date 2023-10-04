# Monte Carlo estimate of the standard normal cdf
x <- seq(0.1,2.5,length=10)
m <- 10000
cdf <- numeric(length(x))
for(i in 1:length(x)){
  j <- runif(m,0,x[i])
  A <- exp(-j^2/2)
  cdf[i] <- 0.5 + x[i]*mean(A)/sqrt(2*pi)
}
Phi <- pnorm(x)
print(round(rbind(x,cdf,Phi),3))

# an estimate of the variance of Monte Carlo estimate of Φ(2)
x <- 2
m <- 10000
j <- runif(m,0,x)
g <- exp(-j^2/2)
theta <- x*mean(g)/sqrt(2*pi) + 0.5
varg <- mean((g - mean(g))^2)
varhat <- x^2*varg/(m*2*pi)
c(theta, varhat)
pnorm(2)
# a 95% confidence interval for Φ(2)
theta + qnorm(c(0.025, 0.975)) * sqrt(varhat)