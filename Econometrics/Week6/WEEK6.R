file = "C:\\Users\\Gannon_chef\\Documents\\Pradeepta\\Coursera\\Econometrics\\WEEK6\\Dataset61-RPK.txt"
data_rpk<-read.table(file, header = TRUE, sep = "", dec = ".",fill = TRUE)
head(data_rpk)

library(IRdisplay)
library(magrittr)
library(tidyverse)
library(scales)
library(gridExtra)
library(forecast)
library(tseries)
library(ggthemes)
theme_set(theme_economist())

file = "C:\\Users\\Gannon_chef\\Documents\\Pradeepta\\Coursera\\Econometrics\\WEEK6\\Dataset62-IP.txt"
data_orig<-read.table(file, header = TRUE, sep = "", dec = ".",fill = TRUE)
head(data_orig)
str(data_orig)

dim(data_orig)
data_ip = data_orig[1:240,]
dim(data_ip)


data_ip %>% ggplot(aes(x= seq_along(CLI), y= CLI))+ 
  geom_line(color= "red")+
  geom_line(y= data_ip$IP, color= "blue") +
  #ylim(c(105,145))+
  ggtitle("CLI(red) and IP(blue) Evolution")


data_ip %>% ggplot(aes(x= seq_along(LOGCLI), y= LOGCLI))+ 
  geom_line(color= "red")+
  geom_line(y= data_ip$LOGIP, color= "blue") +
  #ylim(c(105,145))+
  ggtitle("LOGCLI(red) and LOGIP(blue) Evolution")

data_ip %>% ggplot(aes(x= seq_along(GRIP), y= GRIP))+ geom_line(color= "red")

data_ip %>% ggplot(aes(x= seq_along(GRCLI), y= GRCLI))+ geom_line(color= "red")

g1<-ggAcf(data_ip$CLI)
g2<-ggAcf(data_ip$IP)
g3<-ggAcf(data_ip$GRIP)
g4<-ggAcf(data_ip$GRCLI)
grid.arrange(g1,g2,g3,g4)

adf.test(data_ip$GRCLI)
adf.test(data_ip$GRIP)
adf.test(data_ip$LOGCLI)
adf.test(data_ip$LOGIP)

# FROM ABOVE THERE ARE NO STATINARITY IN LOGCLI and LOGIP

#fir ARMA model - GRCLI
arma.model <- auto.arima(data_ip$GRCLI, max.d=0, allowdrift=TRUE)
arma.model
arma.residual<-resid(arma.model)
ggAcf(arma.residual)
#THIS LOOKS GOOD!

#-----------------------------------------------------------------------------#
#engle granger test for cointegrated??
#https://www.econometrics-with-r.org/16-3-cointegration.html

ols <- lm(LOGIP~LOGCLI, data_ip)
summary(ols)
r <- resid(ols)
adf.test(r)


#from now we will only consider monthly growth rate
####1.univariate autoregression for GRIP

g1<-ggAcf(data_ip$GRIP, type = "correlation")
g2<-ggAcf(data_ip$GRIP, type = "partial")
grid.arrange(g1,g2)
n<-240 ## number of samples
2/sqrt(n)   #--> looks like AR3 is useful as partial autocorrelation is significant upto 3
##start with lag 3-12 and then reduce
arma.model <- auto.arima(data_ip$GRIP, max.d=0, max.q=0,allowdrift=TRUE)
arma.model
arma.residual<-resid(arma.model)
ggplot() + geom_point(aes(x = seq_along(arma.residual), y = arma.residual))
ggAcf(arma.residual)


#normality of residual
jarque.bera.test(arma.residual) ##-> normality is rejected

####2.autoregressive distributed lag model which includes GRCLI
#now can we better this by using a leading index?
#if CLI is leading, then by how many months?

data_ip$GRIP_l3 <- lag(data_ip$GRIP, 3)
data_ip$GRCLI_l6 <- lag(data_ip$GRCLI, 6)
ols2<-lm(GRIP~GRIP_l3+GRCLI_l6, data_ip)
summary(ols2)
#so CLI leads IP by 6 months
jarque.bera.test(resid(ols2)) ##-> normality is rejected
#breush godfrey - 6 lags = 0.36 no serial correlation 
ggAcf(ols2$residual)

ff <- forecast(arma.residual, h=24, level=80)
autoplot(ff)


library(dynlm)
library(forecast)
library(stargazer)
library(scales)
library(quantmod)
library(urca)
library("forecast")

# estimate the ADL(2,1) model of GDP growth
ts_GRIP <- ts(data_ip$GRIP, start =2)
ts_GRCLI <- ts(data_ip$GRCLI, start =2)
dyn.model <- dynlm(ts_GRIP ~ L(ts_GRIP, 3) + L(ts_GRCLI, 6))
summary(dyn.model)

coeftest(dyn.model, vcov. = sandwich)
ff <- forecast(dyn.model, h=24, level=80)
autoplot(ff)

#use formala syntax with data.frame and correct variable names
fit1 <- tslm(ts.out ~ ts.in, df.oldData)

#now predict using ts.in values from df.newData
projection <- forecast.lm(dyn.model, newdata = data_orig)


new_data <- data_orig[241:nrow(data_orig),]
pred <- predict(dyn.model, newdata = new_data[c("GRCLI", "GRIP")])
pred
