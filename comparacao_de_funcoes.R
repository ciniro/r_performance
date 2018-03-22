rm(list=ls())
library (compiler) 
library (microbenchmark) 
library (ggplot2)

n = 300

f1 <- function (n) 
{
  l <- list()
  for(idx in 1:n) 
  {
    l <- append(l, idx)
  }
  return(l)
}

f2 <- function (n) 
{
  l <- list()
  for(idx in 1:n) 
  {
    l[[length(l) + 1]] <- idx
  }
  return(l)
}

f3 <- function (n) 
{
  l <- vector("list", n)
  for(idx in 1:n) 
  {
    l[[idx]] <- idx
  }
  return(l)
}

f4 <- function (n) 
{
  return(as.list(sapply(1:n, function (idx) idx)))
}

fc1 <-   cmpfun (f1)
fc2 <-   cmpfun (f2)
fc3 <-   cmpfun (f3)
fc4 <-   cmpfun (f4)

compare <- microbenchmark(f1(n), fc1(n), f2(n),  fc2(n), f3(n),  fc3(n), f4(n),  fc4(n), times=1000)

autoplot (compare) 
boxplot(compare)
qplot(y=time, data=compare, colour=expr) + scale_y_log10()