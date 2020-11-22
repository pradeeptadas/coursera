file_path <- "C:\\Users\\Gannon_chef\\Documents\\Pradeepta\\Coursera\\R_programming"

read_file <- function(directory, id){
  my_data <- read.csv(paste(file_path,"\\",directory,"\\",sprintf("%03d", id),".csv", sep=""))
  my_data
}

pollutantmean <- function(directory, pollutant, id=1:332) {
  s <- 0
  n <- 0
  for (i in id){
    d<-read_file(directory, i)
    s <- s + sum(na.omit(d[[pollutant]]))
    n <- n + length(na.omit(d[[pollutant]]))
  }
  s/n
}



complete <- function(directory, id = 1:332){
  df <- data.frame(id=integer(),
                   nobs=integer())
  
  for (i in id){
    d<-read_file(directory, i)
    d<-na.omit(d)
    df<-rbind(df, data.frame(id=i, nobs=nrow(d)))
  }
  df
}

corr <- function(directory, threshold = 0){
  res <- numeric(0)
  complete_data <- complete(directory)
  complete_data <- complete_data[complete_data$nobs>=threshold,]
  for (i in complete_data$id) {
    d<-read_file(directory, i)
    d<-na.omit(d)
    res <- c(res, cor(d$sulfate, d$nitrate))
  }
  res
}
