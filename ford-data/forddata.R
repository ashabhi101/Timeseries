# Ford Data 
library(lubridate)
library(zoo)
library(fBasics)
getwd()
setwd("C:/Users/abhil/Desktop/Rfiles")
ford = read.csv("ford.csv")
head(ford)

mdate= "2009-12-31"
ford<- getSymbols("F", from=mdate, auto.assign = F)
head(ford)
chartSeries(ford, theme = "white", name= "Financial chart")


# calculate simple return
n= length(ford$Adj.Close)
srf = ((ford$Adj.Close[2:n]-ford$Adj.Close[1:n-1])/ford$Adj.Close[1:n-1])
plot.ts(ford$Date[-1], srf,type = "l")
plot.ts(ford$Adj.Close)
srf1 = diff(ford$Adj.Close)/ ford$Adj.Close[-length(ford$Adj.Close)]
head(srf1)
head(srf)

tsrf = timeSeries(ford$Adj.Close,)
plot.ts(tsrf)
class(ford$Date)
as.timeDate(ford$Date)
plot.ts(srf, type = "l")

# 1 log return
logreturn = log(ford$Adj.Close[2:n])-log(ford$Adj.Close[1:n-1])
x = mean(logreturn)
midreturn = logreturn - x
head(logreturn)
head(midreturn)
par(mfrow = c(2,2))
acf(logreturn^2)
pacf(logreturn^2)
acf(midreturn^2)
pacf(midreturn^2)

#2
library(FitAR)
Box.test(logreturn,type = 'Ljung')

x = FitAR(logreturn,2, lag.max=5)
summary(x)
acf(x$res)

plot(logreturn,type = "l")
y = ar.mle(logreturn,aic=TRUE,order.max = NULL, se.fit =TRUE)
y
summary(y)

mod3<- garchFit(~arma(1,0)+garch(1,1),data=logreturn,trace=F, cond.dist =c("norm")); mod3
summary(mod3)
resmod3<- residuals(mod3, standardize = T)
acf(resmod3)
qqnorm(resmod3)
qqline(resmod3)
Box.test(resmod3, type='Ljung')
Box.test(resmod3^2, type='Ljung')

#or 
model1 =arima(frets, order = c(2,0,0))
summary(model1)
Box.test(model1$residuals, type='Ljung')
Box.test(model1$residuals^2, type='Ljung')
summary(x)
Box.test(x$res, type='Ljung')
Box.test(x$res^2, type='Ljung')


library(fGarch)
model2<- garchFit(~arma(1,0)+garch(1,1),data=logreturn,trace=F, cond.dist =c("std") ); mod4
summary(model2)
resmod<- residuals(model2, standardize = T)
acf(resmod)
qqnorm(resmod)
qqline(resmod)
Box.test(resmod4, type='Ljung')
Box.test(resmod4^2, type='Ljung') # left over residuals after fitting are independent

model3<- garchFit(~arma(1,0)+garch(1,1),data=frets,trace=F, cond.dist =c("norm")); model3
summary(model3)
resmodel3<- residuals(model3, standardize = T)
par(mfrow = c(2,2))
acf(resmodel3)
qqnorm(resmodel3)
qqline(resmodel3)
Box.test(resmodel3, type='Ljung')
Box.test(resmodel3^2, type='Ljung')
