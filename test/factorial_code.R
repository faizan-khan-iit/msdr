library(purrr)
library(microbenchmark)
library(ggplot2)
# For this Part you will need to write four different versions
# of the Factorial function:
# 
# Factorial_loop: a version that computes the factorial
# of an integer using looping (such as a for loop)
factorial_loop <- function(n=10){
  ans <- 1
  if(n==0){
    return(ans)
  }else if(n<0){
    message("n < 0, no factorials")
    return(NA)
  }
  
  for(i in 1:n){
    ans <- ans * i
  }
  return(ans)
}

# Factorial_reduce: a version that computes the
# factorial using the reduce() function in the purrr package.
# Alternatively, you can use the Reduce() function in the base package.
mul <- function(x, y){
  return(as.numeric(x)*as.numeric(y))
}
factorial_reduce <- function(n=10){
  ans <- 1
  if(n==0){
    return(ans)
  }else if(n<0){
    message("n < 0, no factorials")
    return(NA)
  }
  ans <- purrr::reduce(1:n, mul)
  return(ans)
}
# Factorial_func: a version that uses recursion to compute the factorial.
factorial_recur <- function(n=10){
  if(n==0 || n==1){
    return(1)
  }else if(n<0){
    message("n < 0, no factorials")
    return(NA)
  }
  return(factorial_recur(n-1)*n)
}

# Factorial_mem: a version that uses memoization to compute the factorial.
factorial_vals <- c(1, rep(NA, 70))

factorial_mem <- function(n=10){
  if(n<0){
    message("n < 0, no factorials")
    return(NA)
  }else if(n>70){
    message("0 <= n <= 70")
    return(NA)
  }else if(n==0){
    return(1)
  }else if(!is.na(factorial_vals[n])){
    factorial_vals[n]
  }else{
    factorial_vals[n-1] <<- factorial_mem(n-1)
    n*factorial_vals[n-1]
  }
}

# After writing your four versions of the Factorial function, use the
# microbenchmark package to time the operation of these functions and
# provide a summary of their performance.
# summaries <- lapply(c(factorial_loop, factorial_mem,
#                       factorial_recur, factorial_reduce), microbenchmark)
loop_sum <- microbenchmark(factorial_loop(10))
recur_sum <- microbenchmark(factorial_recur(10))
reduce_sum <- microbenchmark(factorial_reduce(10))
mem_sum <- microbenchmark(factorial_mem(10))

# Edit range of n for different results
range_n <- seq(5, 70, 5)
memo_data <- map(range_n,
                 function(x){microbenchmark(factorial_mem(x))$time})
memo_data <- sapply(memo_data, mean)

recur_data <- map(range_n,
                 function(x){microbenchmark(factorial_recur(x))$time})
recur_data <- sapply(recur_data, mean)

reduce_data <- map(range_n,
                 function(x){microbenchmark(factorial_reduce(x))$time})
reduce_data <- sapply(reduce_data, mean)

loop_data <- map(range_n,
                 function(x){microbenchmark(factorial_loop(x))$time})
loop_data <- sapply(loop_data, mean)

data_combined <- data.frame(time=c(memo_data, recur_data, reduce_data, loop_data),
                            n=rep(range_n, 4),
                            method=c(rep(c("Memoisation", "Recursion", "Reduce", "Loop"),
                                         each=length(range_n))))
comparison_plot_new <- ggplot(data=data_combined) +
  geom_point(aes(n, time, colour=method)) +
  geom_line(aes(n, time, colour=method)) +
  xlab("n") + ylab("Time")
