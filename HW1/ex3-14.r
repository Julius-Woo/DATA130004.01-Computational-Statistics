rmvn_Chol <- function(n,mu,sigma){
  d <- length(mu)
  CT <- chol(sigma)
  Z <- matrix(rnorm(n*d),n,d)
  X <- Z%*%CT + matrix(mu,n,d,byrow=T)
  return(X)
}
sigma <- matrix(c(1, -0.5, 0.5, -0.5, 1, -0.5, 0.5, -0.5, 1), 3, 3)
mu <- c(0, 1, 2)
x <- rmvn_Chol(200, mu, sigma)
colMeans(x)
cor(x)
pairs(x)