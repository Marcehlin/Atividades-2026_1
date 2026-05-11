##########
## Intervalos de credibilidade com exemplos de aulas anteriores
##########
#
####
library(HDInterval)
#
#### Exemplo 1: slide 5 da unidade 3. A senhora grávida do sétimo filho
#
x<-5
n<-6
a<-b<-1 # hiperparâmetros da priori beta para p
apost<-x+a
bpost<-n-x+b
#
pchap1<-apost/(apost+bpost) # considerando perda quadrática
pchap1
pchap2<-qbeta(0.50,apost,bpost) # considerando perda absoluta
pchap2
#
IC<-qbeta(c(0.025,0.975),apost,bpost) # intervalo de credibilidade 95% para p com caudas iguais
IC
#
set.seed(100)
aa<-rbeta(20000,apost,bpost) # simulando uma amostra da posteriori
HPD<-hdi(aa, credMass = 0.95) # intervalo de credibilidade HPD 95% para p
HPD
#
p<-seq(0,1,by=0.01)
post<-dbeta(p,apost,bpost)
plot(p,post,type='l')
abline(v=IC[1])
abline(v=IC[2])
abline(v=HPD[1],col='red')
abline(v=HPD[2],col='red')
#
#### Exemplo 2: slide 6 da unidade 3. Uma amostra da distribuição exponencial com priori gama
#
lambda<-0.5 # para simular uma amostra de uma exponencial
n<-20 # depois aumentar o tamanho da amostra
set.seed(100)
x<-rexp(n,lambda)
a<-b<-1 # hiperparâmetros da priori gama para lambda
apost<-n+a
bpost<-sum(x)+b
#
lchap1<-apost/bpost # considerando perda quadrática
lchap1
lchap2<-qgamma(0.50,apost,bpost) # considerando perda absoluta
lchap2
#
IC<-qgamma(c(0.025,0.975),apost,bpost) # intervalo de credibilidade 95% para lambda com caudas iguais
IC
#
set.seed(100)
aa<-rgamma(20000,apost,bpost) # simulando uma amostra da posteriori
HPD<-hdi(aa, credMass = 0.95) # intervalo de credibilidade HPD 95% para lambda
HPD
HPD2<-hdi(qgamma, 0.95, shape=apost, rate=bpost) # intervalo HPD 95% direto da distribuição, sem simular amostra
HPD2
#
lam<-seq(0,2,by=0.01)
post<-dgamma(lam,apost,bpost)
plot(lam,post,type='l')
abline(v=IC[1])
abline(v=IC[2])
abline(v=HPD2[1],col='red')
abline(v=HPD2[2],col='red')
#
#### Exemplo 3: slide 7 da unidade 4. Uma amostra da distribuição Poisson com priori Jeffreys
#
lambda<-10 # para simular uma amostra de uma Poisson
n<-20 # depois aumentar o tamanho da amostra para 1000
set.seed(100)
x<-rpois(n,lambda)
apost<-sum(x)+0.5
bpost<-n
#
lchap1<-apost/bpost # considerando perda quadrática
lchap1
lchap2<-qgamma(0.50,apost,bpost) # considerando perda absoluta
lchap2
#
IC<-qgamma(c(0.025,0.975),apost,bpost) # intervalo de credibilidade 95% para lambda com caudas iguais
IC
#
set.seed(100)
aa<-rgamma(20000,apost,bpost) # simulando uma amostra da posteriori
HPD<-hdi(aa, credMass = 0.95) # intervalo de credibilidade HPD 95% para lambda
HPD
HPD2<-hdi(qgamma, 0.95, shape=apost, rate=bpost) # intervalo HPD 95% direto da distribuição, sem simular amostra
HPD2
#
lam<-seq(8,12,by=0.01)
post<-dgamma(lam,apost,bpost)
plot(lam,post,type='l')
abline(v=IC[1])
abline(v=IC[2])
abline(v=HPD2[1],col='red')
abline(v=HPD2[2],col='red')
#
#
#### Exemplo 4: slide 14 da unidade 4. Uma amostra da distribuição Normal com priori Normal-Gama-inversa
mu<-50 # para simular uma amostra de uma normal
sigma2<-9 # para simular uma amostra de uma normal
n<-100 # depois aumentar o tamanho da amostra para 500
set.seed(101)
x<-rnorm(n,mu,sqrt(sigma2))
#
# hiperparâmetros da priori normal-gama-inversa
lambda<-1 
m<-10
a<-3
b<-7
#
lambdapost<-lambda+n
mpost<-(lambda*m+n*mean(x))/(lambda+n)
apost<-n/2 +a
bpost<-b+((n-1)*var(x)+(lambda*n*(mean(x)-m)^2)/(lambda+n))/2
#
##### simulando amostras da normal-gama-inversa para depois aproximar os parâmetros via Monte Carlo
#
aMC<-20000
sigma2a<-NULL
mua<-NULL
set.seed(100)
for (i in 1:aMC){
	sigma2a[i]<-1/rgamma(1,apost,bpost)
	mua[i]<-rnorm(1,mpost,sqrt(sigma2a[i]/lambdapost))}
#
muchap1<-mean(mua) # considerando perda quadrática
muchap1
muchap2<-median(mua) # considerando perda absoluta
muchap2
#
IC<-quantile(mua,c(0.025,0.975)) # intervalo de credibilidade 95% para lambda com caudas iguais
IC
#
HPD<-hdi(mua, credMass = 0.95) # intervalo de credibilidade HPD 95% para lambda
HPD
#
sigchap1<-mean(sigma2a) # considerando perda quadrática
sigchap1
sigchap2<-median(sigma2a) # considerando perda absoluta
sigchap2
#
IC<-quantile(sigma2a,c(0.025,0.975)) # intervalo de credibilidade 95% para lambda com caudas iguais
IC
#
HPD<-hdi(sigma2a, credMass = 0.95) # intervalo de credibilidade HPD 95% para lambda
HPD