file = "C:\\Users\\Gannon_chef\\Documents\\Pradeepta\\Coursera\\Econometrics\\TrainExer3-3.txt"
data<-read.table(file, header = TRUE, sep = "", dec = ".")
head(data)
data['logIndex'] = lapply(data['Index'], log)
data['delta.logIndex'] = c(NA, diff(data$logIndex, lag = 1))
head(data)
data <- na.omit(data)
model <- lm(delta.logIndex~BookMarket, data=data)
summary(model)
plot(resid(model))


model2 <- lm(Index~BookMarket, data=data)
summary(model2)
plot(resid(model2))


p <- ggplot(model2, aes(x = .fitted, y = .resid)) + geom_point() 
p



#b2m and b2m_sq
data['b2m_sq'] <- data['BookMarket'] * data['BookMarket']
model3 <- lm(delta.logIndex~BookMarket, data=data)
summary(model3)
