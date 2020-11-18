file = "C:\\Users\\Gannon_chef\\Documents\\Pradeepta\\Coursera\\Econometrics\\TrainExer21.txt"
data_sales<-read.table(file, header = TRUE, sep = "", dec = ".")
model <- lm(LogWage~Female+Age+Educ+Parttime, data=data_sales)
summary(model)


model2 <- lm(LogWage~Female, data=data_sales)
summary(model2)


model3 <- lm(Wage~Female+Age+Educ+Parttime, data=data_sales)
summary(model3)

file25 = "C:\\Users\\Gannon_chef\\Documents\\Pradeepta\\Coursera\\Econometrics\\TrainExer25.txt"
new_logwage<-read.table(file25, header = TRUE, sep = "", dec = ".")

model4 <- lm(LogWage~Female+Age+Educ+Parttime, data=new_logwage)
summary(model4)
res <- resid(model4)
new_logwage<-cbind(new_logwage, residual=res)
model5 <- lm(residual~DE2+DE3+DE4, data=new_logwage)
summary(model5)
mean(resid(model5))
