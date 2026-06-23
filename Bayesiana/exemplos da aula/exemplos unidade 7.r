#
##### Algoritmo Metropolis-Hastings para simular valores da qui-quadrado inversa usando a uniforme
#
fdpfunction<-function(nu,x) x^{-nu/2-1}*exp(-1/(2*x))
#
iter<-100
y<-NULL
prob_ace<-NULL
y[1]<-1
aceit<-0
set.seed(10)
for (i in 2:iter){
	x_prop<-runif(1,0,100)
	u<-runif(1,0,1)
	prob_ace[i]<-min(1,fdpfunction(5,x_prop)/fdpfunction(5,y[i-1]))
	if (u<=prob_ace[i]){
		y[i]<-x_prop
		aceit<-aceit+1} else y[i]<-y[i-1]}
plot(prob_ace)
taxa_aceit<-aceit/iter
taxa_aceit
#
plot(y,type='l') # depois para tamanho de amostra n=10000
#
##### algoritmo MH para simular valores da qui-quadrado inversa usando a gama
#
iter<-100
y<-NULL
prob_ace<-NULL
y[1]<-1
aceit<-0
set.seed(10)
for (i in 2:iter){
	x_prop<-rgamma(1,2,2)
	u<-runif(1,0,1)
	prob_ace[i]<-min(1,(fdpfunction(5,x_prop)*dgamma(y[i-1],2,2))/(fdpfunction(5,y[i-1])*dgamma(x_prop,2,2)))
	if (u<=prob_ace[i]){
		y[i]<-x_prop
		aceit<-aceit+1} else y[i]<-y[i-1]}
plot(prob_ace)
taxa_aceit<-aceit/iter
taxa_aceit
#
plot(y,type='l') # depois para tamanho de amostra n=10000
#
####### Por Monte Carlo
#
mean(y) # média da distribuição
var(y) # var da distribuição
#
####################################
#
###############################
# Exemplo modelo de mistura GS - amostra de tamanho 100
###############################
##
set.seed(100)
altura<-c(rnorm(40,1.80,0.10),rnorm(40,1.60,0.05),rnorm(20,1.20,0.13))
grupo_real<-c(rep("M",40),rep("F",40),rep("C",20))
#hist(altura)
#
############## funções úteis para o GS
#
# simula valores de uma distribuição discreta
#
rDiscreta<-function(p){
 u<-runif(1)
 P<-cumsum(p)
 val<-sum(P<u)+1
 val}
#
# calcula média, lambda, a e beta da distribuição a posteriori Normal Gama Inversa,
#
postparamvarunyis<-function(yi,mu0,lambda0,a0,b0,nki){
	ykmean<-mean(yi)
	yksumsquar<-sum(yi**2)
	munew<-((ykmean*nki)+(lambda0*mu0))/(lambda0+nki)
	lambdanew<-lambda0+nki
	anew<-a0+(nki/2)
	s2<-yksumsquar-(nki*ykmean**2)
	bnew<-b0+(s2+((nki*lambda0*(ykmean-mu0)**2)/(lambda0+nki)))/2
	list(munew,lambdanew,anew,bnew)}
#
# simula vetores da distribuição Dirichlet
#
rDiric<-function(gama){
  X<-rgamma(length(gama),gama,1)
  Y<-X/sum(X)
  return(Y)}
#
#### valores dos hiperparametros e chutes iniciais
#
y<-altura
K<-3
lambda0<-0.5
mu0<-0
a0<-3
b0<-6
gama<-rep(1,K)
#
set.seed(100)
S<-sample(1:3,length(y),replace=T)
S_tot<-S
table(S)
table(S,grupo_real)
n_k<-NULL
for (k in 1:K) n_k[k]<-sum(S==k)
#
ws<-rDiric(n_k+gama)
w_tot<-ws
#
medias<-NULL
variancias<-NULL
set.seed(100)
for (k in 1:K){
	if (n_k[k]>0){
		par_ngi<-postparamvarunyis(y[S==k],mu0,lambda0,a0,b0,n_k[k])
		variancias[k]<-1/rgamma(1,par_ngi[[3]],1/par_ngi[[4]])
		medias[k]<-rnorm(1,par_ngi[[1]],sqrt(variancias[k]/par_ngi[[2]]))} else {
		variancias[k]<-1/rgamma(1,a0,1/b0)
		medias[k]<-rnorm(1,mu0,sqrt(variancias[k]/lambda0))}}
var_tot<-variancias
medias_tot<-medias
#
library(compiler)
enableJIT(3)
#
iter<-1000
set.seed(100)
for (it in 2:iter){
	#
	## atualiza os Ss
	#
	for (i in 1:length(y)){
		probs<-ws*dnorm(y[i],medias,sqrt(variancias))
		probs<-probs/sum(probs)
	    S[i]<-rDiscreta(probs)}
	S_tot<-rbind(S_tot,S)
	#
	## atualiza os ws
	#
	for (k in 1:K) n_k[k]<-sum(S==k)
	ws<-rDiric(n_k+gama)
	w_tot<-rbind(w_tot,ws)
	#
	## atualiza as médias e variâncias
	#
	for (k in 1:K){
	if (n_k[k]>0){
		par_ngi<-postparamvarunyis(y[S==k],mu0,lambda0,a0,b0,n_k[k])
		variancias[k]<-1/rgamma(1,par_ngi[[3]],1/par_ngi[[4]])
		medias[k]<-rnorm(1,par_ngi[[1]],sqrt(variancias[k]/par_ngi[[2]]))} else {
		variancias[k]<-1/rgamma(1,a0,1/b0)
		medias[k]<-rnorm(1,mu0,sqrt(variancias[k]/lambda0))}}
	var_tot<-rbind(var_tot,variancias)
	medias_tot<-rbind(medias_tot,medias)
}
table(S,grupo_real)
#
par(mfrow=c(3,1))
plot(w_tot[,1],type='l')
plot(w_tot[,2],type='l')
plot(w_tot[,3],type='l')
apply(w_tot,2,mean)
#
par(mfrow=c(3,1))
plot(medias_tot[,1],type='l')
plot(medias_tot[,2],type='l')
plot(medias_tot[,3],type='l')
apply(medias_tot,2,mean)
#
par(mfrow=c(3,1))
plot(var_tot[,1],type='l')
plot(var_tot[,2],type='l')
plot(var_tot[,3],type='l')
apply(var_tot,2,mean)
#
################ Exemplo da convergência
#
##### MH para simular valores da qui-quadrado inversa usando a uniforme
#
fdpfunction<-function(nu,x) x^{-nu/2-1}*exp(-1/(2*x))
#
n<-10000
y<-NULL
p<-NULL
y[1]<-1
aceit<-0
set.seed(10)
for (i in 2:n){
	x_prop<-runif(1,0,100)
	u<-runif(1,0,1)
	p[i]<-min(1,fdpfunction(5,x_prop)/fdpfunction(5,y[i-1]))
	if (u<=p[i]){ y[i]<-x_prop
		aceit<-aceit+1}  else y[i]<-y[i-1]}
plot(p)
aceit
#
### Gráfico do traço
#
plot(y,type='l') # depois para tamanho de amostra n=10000
#
### sobreposição de histogramas ou densidades estimadas
#
plot(density(y[100:2500]))
lines(density(y[5000:10000]),col='red')
#
library(coda)
yf<-mcmc(y[100:10000])
geweke.diag(yf)
#
# Análise de autocorrelação
#
autocorr(yf)
autocorr.plot(yf)
#
##### MH para simular valores da qui-quadrado inversa usando a gama
#
n<-10000
y<-NULL
p<-NULL
y[1]<-1
aceit<-0
set.seed(10)
for (i in 2:n){
	x_prop<-rgamma(1,2,2)
	u<-runif(1,0,1)
	p[i]<-min(1,(fdpfunction(5,x_prop)*dgamma(y[i-1],2,2))/(fdpfunction(5,y[i-1])*dgamma(x_prop,2,2)))
	if (u<=p[i]){ y[i]<-x_prop
		aceit<-aceit+1}  else y[i]<-y[i-1]}
plot(p)
aceit
#
### Gráfico do traço
#
plot(y,type='l') # depois para tamanho de amostra n=10000
#
### sobreposição de histogramas ou densidades estimadas
#
plot(density(y[100:2500]))
lines(density(y[5000:10000]),col='red')
#
library(coda)
yf<-mcmc(y[100:10000])
geweke.diag(yf)
#
# Análise de autocorrelação
#
autocorr(yf)
autocorr.plot(yf)
#