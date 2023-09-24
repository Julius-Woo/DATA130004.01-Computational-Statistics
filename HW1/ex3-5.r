# Inverse transform method
n <- 1000
p <- c(0.1, 0.2, 0.2, 0.2, 0.3)
p_cdf <- cumsum(p)
x <- rep(n, 0)
for (i in 1:n) {
    u <- runif(1)
    x[i] <- sum(u > p_cdf)
}
rbind(table(x) / n, p)

# Sample function
n <- 1000
p <- c(0.1, 0.2, 0.2, 0.2, 0.3)
y <- sample(0:4, size=n, prob=p, replace=T)
rbind(table(y) / n, p)