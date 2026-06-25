ts.plot(AirPassengers)
length(AirPassengers)
serie<-as.numeric(AirPassengers)
treino<-AirPassengers[1:120]
treino<-ts(treino,freq=12)
teste<-AirPassengers[121:144]

M1<-arima(log(treino),order=c(0,1,1),
          seasonal=list(order=c(0,1,1),period=12))
prev.ar<-predict(M1,24)
hat.ar<-exp(prev.ar$pred)
min(AirPassengers)
max(hat.ar)
max(AirPassengers)


m.hw<-HoltWinters(treino)
p.hw<-predict(m.hw,24)
accuracy(teste,hat.ar)
accuracy(teste,p.hw)
holt<-c(treino,p.hw)
BJ<-c(treino,hat.ar)

ts.plot(holt, xlim=c(1,144),ylim=c(100,640), col=2, ylab="")
par(new=T)
ts.plot(BJ, xlim=c(1,144),ylim=c(100,640), col=3,ylab="")
par(new=T)
ts.plot(serie, xlim=c(1,144),ylim=c(100,640), col=1)
