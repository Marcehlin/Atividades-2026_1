n<-1000
#
a<-0.01
b<-0.01
a/b
a/b^2
#
media<-function(a,b,x) (length(x)+a)/(sum(x)+b)
mediana<-function(a,b,x) qgamma(0.5,length(x)+a,sum(x)+b)
moda<-function(a,b,x) (length(x)+a-1)/(sum(x)+b)
#
lambda<-0.01
#
media1<-NULL
mediana1<-NULL
moda1<-NULL

cont<-1
for (n in c(10,30,50,100,500,1000,2500,5000,7500,10000)){
set.seed(123)
x<-rexp(n,lambda)
#hist(x)
#
#
media1[cont]<-media(a,b,x)
mediana1[cont]<-mediana(a,b,x)
moda1[cont]<-moda(a,b,x)
cont<-cont+1}

n<-c(10,30,50,100,500,1000,2500,5000,7500,10000)
plot(n,media1,type='l')
plot(n,mediana1,type='l')
plot(n,moda1,type='l')
