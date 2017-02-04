library(purrr)
library(microbenchmark)

# Factorial_loop: a version that computes the factorial
# of an integer using looping (such as a for loop)
Factorial_loop <- function(n=10){
  ans <- 1
  if(n==0){
    return(ans)
  }else if(n<0){
    message("n < 0, no Factorials")
    return(NA)
  }
  
  for(i in 1:n){
    ans <- ans * i
  }
  return(ans)
}

# Factorial_reduce: a version that computes the
# factorial using the reduce() function in the purrr package.
mul <- function(x, y){
  return(as.numeric(x)*as.numeric(y))
}
Factorial_reduce <- function(n=10){
  ans <- 1
  if(n==0){
    return(ans)
  }else if(n<0){
    message("n < 0, no Factorials")
    return(NA)
  }
  ans <- purrr::reduce_right(1:n, mul)
  return(ans)
}

# Factorial_func: a version that uses recursion to compute the Factorial.
Factorial_func <- function(n=10){
  if(n==0 || n==1){
    return(1)
  }else if(n<0){
    message("n < 0, no Factorials")
    return(NA)
  }
  return(Factorial_func(n-1)*n)
}

# Factorial_mem: a version that uses memoization to compute the Factorial.
Factorial_vals <- c(1, rep(NA, 70))

Factorial_mem <- function(n=10){
  if(n<0){
    message("n < 0, no Factorials")
    return(NA)
  }else if(n>70){
    message("0 <= n <= 70")
    return(NA)
  }else if(n==0){
    return(1)
  }else if(!is.na(Factorial_vals[n])){
    Factorial_vals[n]
  }else{
    Factorial_vals[n-1] <<- Factorial_mem(n-1)
    n*Factorial_vals[n-1]
  }
}

## Comparison between the different functions

# For n = 10
loop_sum1 <- microbenchmark(Factorial_loop(10))
recur_sum1 <- microbenchmark(Factorial_func(10))
reduce_sum1 <- microbenchmark(Factorial_reduce(10))
mem_sum1 <- microbenchmark(Factorial_mem(10))

# For n = 5, 10, 15 ... ,70
range_n <- seq(5, 70, 5)
memo_data <- map(range_n,
                 function(x){microbenchmark(Factorial_mem(x))$time})
memo_data <- sapply(memo_data, mean)
memo_sum2 <- summary(memo_data)

recur_data <- map(range_n,
                  function(x){microbenchmark(Factorial_func(x))$time})
recur_data <- sapply(recur_data, mean)
recur_sum2 <- summary(recur_data)

reduce_data <- map(range_n,
                   function(x){microbenchmark(Factorial_reduce(x))$time})
reduce_data <- sapply(reduce_data, mean)
reduce_sum2 <- summary(reduce_data)

loop_data <- map(range_n,
                 function(x){microbenchmark(Factorial_loop(x))$time})
loop_data <- sapply(loop_data, mean)
loop_sum2 <- summary(loop_data)

# sink(file = "./factorial_output.txt")
# paste0("The results for 100 runs with n=10")
# paste0("Factorial_loop")
# loop_sum1
# paste0("Factorial_func")
# recur_sum1
# paste0("Factorial_reduce")
# reduce_sum1
# paste0("Factorial_mem")
# mem_sum1
# 
# paste0("The results for 100 runs with n=5,10,15... ,70")
# paste0("Factorial_loop")
# loop_sum2
# paste0("Factorial_func")
# recur_sum2
# paste0("Factorial_reduce")
# reduce_sum2
# paste0("Factorial_mem")
# memo_sum2
# 
# paste("Clearly, Memoisation and Looping methods are much better for",
#        "computing factorials than Recursion and Reduce, which do worse",
#       "as n keeps increasing.")
# sink()