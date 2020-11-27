file = "C:\\Users\\Gannon_chef\\Documents\\Pradeepta\\GitCoursera\\coursera\\Econometrics\\Week5\\DataLecture5-1.txt"
data<-read.table(file, header = TRUE, sep = "", dec = ".")
head(data)
model <- lm(Response~Price, data)
summary(model)


file = "C:\\Users\\Gannon_chef\\Documents\\Pradeepta\\GitCoursera\\coursera\\Econometrics\\Week5\\DataLecture5-5.txt"
data<-read.table(file, header = TRUE, sep = "", dec = ".")
summary(data)
str(data)
#we see that the response, mae and activity to factor
data$male<-ifelse(test=data$male == 1, yes="M", no="F")
data$male <- as.factor(data$male)

data$response <- as.factor(data$response)
data$activity <- as.factor(data$activity)


summary(data)
str(data)

#lets see the distribution
xtabs(~response+male,data=data)
#>  284/(171+284)
#[1] 0.6241758
#> 387/(387+82)
#[1] 0.8251599

xtabs(~activity+response,data=data)
#>  52/(52+403)
#[1] 0.1142857
#>  122/(122+348)
#[1] 0.2595745

##lets start with logistic regression
#glm -> generalized linear model
#family = binomial for logistic regression
mean(data[data$response==0,]$age)
mean(data[data$response==1,]$age)



m1 <- glm(response~male, data=data, family="binomial")
summary(m1)



#deviance residual look good because they are centered around 0 and look symmatrical


# log odds that female has heart disease = -0.7228
# log odds for male is = -0.7228 + 1.0323

# p values are statistically significant
# small p values are not alone interesting. we also want large effect sizes. thats what log(odds) and log (odds ratio) tell us. 

## when we do normal linear regression we estimate both mean and variance from the data. 
## in contrast, for logistic regression, mean is calculated from data and variance is derived from mean. 

## so variance might be underestimated because is it calculated as 1. 
#so in that case, we can adjust the dispersion parameter in summary command

##AIC can be used to compare onemodel to another. 
##fisher scoring iteration tells us how quickly glm converged on the maximum likelihood estimates for the coefficients. 

m2 <- glm(response~male+activity+age, data=data, family="binomial")
summary(m2)

data$age_sq <- (data$age/10)^2
m3 <- glm(response~male+activity+age+age_sq, data=data, family="binomial")
summary(m3)

#mcfaddens pseudo r-square
logistic <- m3
ll.null <- logistic$null.deviance/-2 #log likelihood of null hyp model with just intercept 
ll.proposed <- logistic$deviance/-2 #log likelihood of fancy model  
r_2 <- (ll.null - ll.proposed) / ll.null
r_2

#One measure of model fit is the significance of the overall model. This test asks whether the model with 
#predictors fits significantly better than a model with just an intercept (i.e., a null model). 
#The test statistic is the difference between the residual deviance for the model with predictors and the null model. 
#The test statistic is distributed chi-squared with degrees of freedom equal to the differences in degrees of freedom 
#between the current and the null model (i.e., the number of predictor variables in the model). 
#To find the difference in deviance for the two models (i.e., the test statistic) we can use the command:
with(logistic, null.deviance - deviance)
#The degrees of freedom for the difference between the two models is equal to the number of predictor variables 
#in the mode, and can be obtained using:
with(logistic, df.null - df.residual)
#Finally, the p-value can be obtained using:
with(logistic, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
##The chi-square of 41.46 with 4 degrees of freedom and an associated p-value of less than 0.001 
##tells us that our model as a whole fits significantly better than an empty model. 
#This is sometimes called a likelihood ratio test (the deviance residual is -2*log likelihood). 
#To see the model’s log likelihood, we type:
qchisq(0.975, df=4)  
logLik(logistic)

#########

#Many different measures of psuedo-R-squared exist. They all attempt to provide information similar to that provided by R-squared in OLS regression; however, none of them can be interpreted exactly as R-squared in OLS regression is interpreted
library(pROC)
roc(response~logistic$fitted.values, data = data, plot = TRUE, main = "ROC CURVE", col= "blue")


#> exp(0.95)
#[1] 2.58571  --> so male are more likely to respond than females
#2.5 times higher for active customer
library(stargazer)
stargazer(logistic, type="html", title="Results", header = FALSE)

#################################
#doesnt work - plot!
#################################
#p-valu
1-pchisq(2*(ll.null - ll.proposed), df=(length(logistic$coefficients)-1))
#here the p value is 1!!! so its not a good fit as well. 
library(ggplot2)
library(cowplot)
predicted.data <- data.frame(probability.of.hd=logistic$fitted.values, hd=data$response)
predicted.data <- predicted.data[order(predicted.data$probability.of.hd, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of response!")




####
model <- glm(response~male+activity+age+I((age/10)^2)+I(male*age)+I(male*(age/10)^2),
             data=data, family = "binomial")
stargazer(model, type="html", title="Results", header = FALSE)

