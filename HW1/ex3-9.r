# Rescaled Epanechnikov kernel
rREpan <- function(n){
  u1 <- runif(n, -1, 1)
  u2 <- runif(n, -1, 1)
  u3 <- runif(n, -1, 1)
  x <- rep(n, 0)
  for(i in 1:n){
    if(abs(u3[i]) >= abs(u2[i]) && abs(u3[i]) >= abs(u1[i]))
      x[i] <- u2[i]
    else
      x[i] <- u3[i]
  }
  return(x)
}

x <- rREpan(10000)
hist(x,prob=T)
y <- seq(-1, 1, 0.001)
fe <- 0.75 * (1 - y^2)
lines(y,fe)
