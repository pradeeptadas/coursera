file_path <- "C:\\Users\\Gannon_chef\\Documents\\Pradeepta\\Coursera\\R_programming"

read_file <- function(specdata, id){
  my_data <- read.table(paste(file_path,"\\",id,".csv", sep=""), header = TRUE, sep = "", dec = ".")  
}

pollutantmean <- function(directory, pollutant, id=1:332) {
  
}