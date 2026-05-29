# Simulações
#set.seed(1304)
set.seed(304)
x1<-arima.sim(list(order=c(1,0,0),ar=.8),n=200)
x2<-arima.sim(list(order=c(2,0,0),ar=c(0.5,0.1)),n=200)
x3<-arima.sim(list(order=c(2,0,0),ar=c(0,-0.8)),n=200)
x4<-arima.sim(list(order=c(0,0,2),ma=c(2,-2)),n=200)
x5<- arima.sim(list(order = c(1,0,1), ar =- 0.7,ma=-.9), n = 200)
x6<-arima.sim(n = 63, list(ar = c(0.8897, -0.4858), 
                    ma = c(-0.2279, 0.2488)),sd = sqrt(0.1796))

x7<-arima.sim(list(order=c(1,1,0),ar=.5),n=150)
x8<-arima.sim(list(order=c(0,1,1),ma=-.5),n=150)

# Identificação
ts.plot(x1)
acf(x1)
pacf(x1)

pacf(x1,plot=F)

arima(x1,order=c(1,0,0))

m1<-arima(x1,order=c(1,0,0))
m1
m1$sigma2

m1$aic

hist(m1$residuals)
ts.plot(m1$residuals)
acf(m1$residuals)
pacf(m1$residuals)
Box.test(m1$residuals, lag = 2, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 3, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 4, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 5, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 6, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 7, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 8, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 9, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 10, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 11, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 12, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 13, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 14, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 15, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 16, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 17, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 18, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 19, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 20, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 21, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 22, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 23, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 24, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 25, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 26, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 27, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 28, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 29, type="Ljung", fitdf = 1)
Box.test(m1$residuals, lag = 30, type="Ljung", fitdf = 1)

tsdiag(m1)


###################################################
#
#
#    Previsão
#
#
#############################################



predict(m1,10)



##################################################
library(astsa)
acf2(x1)

sarima(x1,1,0,0)

##################################
#
#
sarima.for(x1,10,1,0,0)
#
#
#########################################
