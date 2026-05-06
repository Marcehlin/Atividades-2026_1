#
####### Exemplo de distribuição a posteriori da senhora grávida do sétimo filho
#
n<-6
x<-5
p<-seq(0,1,by=0.001)
likel<-dbinom(x,n,p)
#
Esp<-function(p1,p2) p1/(p1+p2)
Var<-function(p1,p2) (p1*p2)/((p1+p2)^2*(p1+p2+1))
#
##### Considerando uma uniforme(0,1) como distribuição a priori para p
#
a<-1
b<-1
prior<-dbeta(p,a,b)
posterior<-dbeta(p,x+a,n-x+b)
#
plot(p,likel,type='l',ylim=c(0,1.05))
lines(p,prior, col='blue')
lines(p,posterior/max(posterior), col='red')
Esp(x+a,n-x+b)
Var(x+a,n-x+b)
#
##### Considerando uma beta(129.5,129.5) como distribuição a priori para p
#
a<-129.5
b<-129.5
prior<-dbeta(p,a,b)
posterior<-dbeta(p,x+a,n-x+b)
#
plot(p,likel,type='l',ylim=c(0,1.05))
lines(p,prior/max(prior), col='blue')
lines(p,posterior/max(posterior), col='red')
Esp(x+a,n-x+b)
Var(x+a,n-x+b)
#
##### Considerando uma beta(1.2,1.15) como distribuição a priori para p
#
a<-10.5
b<-10
prior<-dbeta(p,a,b)
posterior<-dbeta(p,x+a,n-x+b)
#
plot(p,likel,type='l',ylim=c(0,1.05))
lines(p,prior/max(prior), col='blue')
lines(p,posterior/max(posterior), col='red')
Esp(x+a,n-x+b)
Var(x+a,n-x+b)