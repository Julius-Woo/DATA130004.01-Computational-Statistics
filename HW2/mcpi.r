set.seed(123)

# use Monte Carlo to approximate pi for dimension d
# N: total sample number
# prevcount: previous count
mcpi <- function(d,N,prevcount=0){
  count <- prevcount
  
  if (count==0){
    for(i in 1:N){
      u <- runif(d,-0.5,0.5)
      if(sum(u^2)<=0.5*0.5)
        count <- count+1
    }
  }
  # skip the previous samples
  else{
    u <- runif(d,-0.5,0.5)
    if(sum(u^2)<=0.5*0.5)
      count <- count+1
  }
  f <- count/N
  pi_est <- (f * 2^d * gamma(d/2 + 1))^(2/d)
  return(list(pi_est = pi_est, count = count))
}

# determine the minimum n to approximate π to its 5th digit
sizeto5 <- function(d){
  N <- 2000
  prevresult <- mcpi(d,N)
  while (TRUE) {
    if (trunc(prevresult$pi_est*1e5) == 314159)
      return(c(Dimension = d, Sample_Size = N, Pi_Approximation = round(prevresult$pi_est, 6)))
    N <- N+1
    prevresult <- mcpi(d, N, prevresult$count)
  }
}

dims <- 2:10
results <- lapply(dims, sizeto5)

results_df <- as.data.frame(do.call(rbind, results))
print(results_df)