library(purrr)
library(ggfortify)
library(microbenchmark)
factorial_loop <- function(n){
  y <- 1
  if (n != 0){
    for(i in 1:n){
      y <- y*i
    }
  }
  y
}
#factorial_loop(7)

factorial_reduce <- function(n){
  if(n ==0){
    1
  } else {
    reduce(c(1:n), function(x, y){x * y})
  }
}
#factorial_reduce(7)


factorial_func <- function(n){
  if (n == 0){
    1
  } else {
    n * factorial_func(n-1)
  }
}
#factorial_func(7)

fac_tbl <- c(rep(NA, 10000))
factorial_mem <- function(n){
  if (n == 0) return(1)
  if(is.na(fac_tbl[n])) {
      fac_tbl[n] <<- n * factorial_mem(n-1)
  }
  fac_tbl[n]
}
factorial_mem(7)

###Evaluation
num<-10
data_exec <- microbenchmark(a<-num,
                        b <- factorial_loop(num),
                        c <- factorial_mem(num),
                        d <- factorial_reduce(num),
                        e <- factorial_func(num))
autoplot(data_exec)


###Evaluation Summary
benchmark  <- function(n){
  record_temp_perf <- microbenchmark(factorial_loop(n), 
                                     factorial_reduce(n),
                                     factorial_func(n),
                                     factorial_mem(n))
  record_temp_perf.df <- data.frame(summary(record_temp_perf))
  autoplot(record_temp_perf)
  record_temp_perf.df
}

benchmark(10)
benchmark(100)
benchmark(1000)


#write.table(benchmark(10),"factorial_out.txt")
