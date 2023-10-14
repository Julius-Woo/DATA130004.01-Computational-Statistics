# Rare event P(Z>10) where Z~N(0,1)
# (a) Simple Monte Carlo
SMC <- function(N){
  X <- rnorm(N)
  mean(X>10)
}

# (b) Change of measure
CoM <- function(N){
  Y <- rnorm(N, mean=10, sd=1)
  mean((Y>10)*exp(50-10*Y))
}


Ns <- c(1e3,1e4,1e5,1e6,1e7)
results <- matrix(NA, nrow=length(Ns), ncol=3)
colnames(results) <- c("Sample Size", "Simple Monte Carlo", "Change of Measure")

for (i in 1:length(Ns)){
  n <- Ns[i]
  results[i,] <- c(n, SMC(n), CoM(n))
}
results_df <- as.data.frame(results)
results_df

