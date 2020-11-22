#Using Reference Class
## not completed! :-(

# Define methods for LongitudionalData objects ---------------------------------
make_LD <- function(df) {
  ld_df <- df %>% ?
  structure(ld_df, class = c("LongitudinalData"))


  Student <- setRefClass("LongitudinalData",
                       fields = list(name = "character",
                                     grad_year = "numeric",
                                     credits = "numeric",
                                     id = "character",
                                     courses = "list"),
                       methods = list(
                         hello = function(){
                           paste("Hi! My name is", name)
                         },
                         add_credits = function(n){
                           credits <<- credits + n
                         },
                         get_email = function(){
                           paste0(id, "@jhu.edu")
                         }
                       ))

}


##################################################################
## Code Testing
##################################################################
data <- read_csv("C://Users//Gannon_chef//Documents//Pradeepta//Coursera//R_programming//AdvancedR//MIE.csv")
x <- make_LD(data)
print(class(x))
print(x)

## Subject 10 doesn't exist
out <- subject(x, 10)
print(out)

out <- subject(x, 14)
print(out)

out <- subject(x, 54) %>% summary
print(out)

out <- subject(x, 14) %>% summary
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

## Show a summary of the pollutant values
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)

