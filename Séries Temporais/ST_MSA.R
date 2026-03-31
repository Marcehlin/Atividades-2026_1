###########################################################################
#
#  Grafico de amplitude media
#
#   chutar o valor de k, 
#
#   se a serie for, sabidamente sazonal, usar o periodo
#
###########################################################################
media.amplitude<-function(x,k)
{N<-length(x)
x.m<-rep(0,(N-k))
x.r<-rep(0,(N-k))
for (i in 1:(N-k)) x.m[i]<-mean(x[i:(i+k)])
for (i in 1:(N-k)) x.r[i]<-max(x[i:(i+k)])-min(x[i:(i+k)])
plot(x.m,x.r,xlab="medias",ylab="amplitude")
aa1<-lm(x.r~x.m)
print(summary(aa1))
abline(aa1$coef[1],aa1$coef[2],col=2)
}
##########################################################################
#
# FIM
#
##########################################################################



############################################
#
#  Medias Moveis
#
#   chutar o valor de k, 
#
#    se a serie for, sabidamente sazonal, usar o periodo
#
###########################################################################
media.moveis<-function(x,k)
{x<-as.vector(x)
N<-length(x)
xstar<-rep(0,N)
for (i in 1:k) xstar[i]<-NA
for (i in (1+k):(N-k)) xstar[i]<-mean(x[(i-k):(i+k)],na.rm=TRUE)
for (i in (N-k+1):N) xstar[i]<-NA
ts.plot(x)
lines(xstar, col=2, lwd=2)
xstar}
##########################################################################
#
# FIM
#
##########################################################################




###########################################################################
#
#  Medianas Moveis
#
#   chutar o valor de k, 
#
#    se a serie for, sabidamente sazonal, usar o periodo
#
###########################################################################
mediana.bloco<-function(x,k)
{x<-as.vector(x)
N<-length(x)
xstar<-rep(0,N)
for (i in 1:k) xstar[i]<-NA
for (i in (1+k):(N-k)) xstar[i]<-median(x[(i-k):(i+k)],na.rm=TRUE)
for (i in (N-k+1):N) xstar[i]<-NA
ts.plot(x)
lines(xstar, col=2, lwd=2)
xstar}
##########################################################################
#
# FIM
#
##########################################################################




####################################################################################################################################################
#
#  Medias em bloco
#
#  chutar o valor de k, 
#
#  para ver a media de cada bloco
#
#
###########################################################################
media.bloco<-function(x,k)
{x<-as.vector(x)
N<-length(x)
xstar<-rep(NA,N)
valor<-N%/%k
for (i in 1:valor)
  xstar[((i-1)*k+1):(i*k)]<-rep(mean(x[((i-1)*k+1):(i*k)],na.rm=TRUE),k)
for (i in (valor*k+1):N) xstar[i]<-mean(x[(valor*k+1):N])
ts.plot(x)
lines(xstar, col=2, lwd=2)
xstar}
##########################################################################
#
# FIM
#
##########################################################################







####################################################################################################################################################
#
#  Amplitude Estavel
#
#  chutar o valor de k, 
#
#  para ver a amplitude de cada bloco
#
#
###########################################################################
amplitude.bloco<-function(x,k)
{x<-as.vector(x)
N<-length(x)
xstarM<-rep(NA,N)
xstarm<-rep(NA,N)
valor<-N%/%k
for (i in 1:valor){
  xstarM[((i-1)*k+1):(i*k)]<-rep(max(x[((i-1)*k+1):(i*k)],na.rm=TRUE),k)
  xstarm[((i-1)*k+1):(i*k)]<-rep(min(x[((i-1)*k+1):(i*k)],na.rm=TRUE),k)
}
ts.plot(x)
lines(xstarM, col=2,lty=3, lwd=2)
lines(xstarm, col=2,lty=3, lwd=2)
}
##########################################################################
#
# FIM
#
##########################################################################






##########################################################################


##########################################################################
#
# Verificacao de Sazonalidade
#
# Definir o periodo da sazonalidade
#
##########################################################################


grafico.sazonalidade<-function(x,s){
  x<-as.vector(x)
  N<-length(x)
  Mx<-max(x)
  mx<-min(x)
  valor<-N%/%s
  eixox<-seq(1:s)
  plot(eixox,x[1:s],ylim=c(mx,Mx),xlim=c(0,(s+1)),type="l")
  for (i in 1:valor){
    y<-rep(NA,s)
    y[1:s]<-x[((i-1)*s+1):(i*s)]
     lines(y,col=i)
  }
  y<-rep(NA,s)
  y<-x[(valor*s+1):N]
  lines(y,col=(valor+1))
}

##########################################################################
#
# FIM
#
##########################################################################


##########################################################################
#
# Verificacao de Sazonalidade Box-Plot
#
# Definir o periodo da sazonalidade
#
##########################################################################


grafico.boxplot<-function(x,s){
  x<-as.vector(x)
  N<-length(x)
  valor<-N%/%s
  apoiox<-rep(NA,N)
  apoioy<-rep(1:s,valor)
  for(i in 1:(s*valor)) apoiox[i]<-apoioy[i]
  boxplot(x~apoiox,xlab='Sazonalidade')
}

##########################################################################
#
# FIM
#
##########################################################################









##########################################################################
#
# Ajuste de um polinomio
#
# Definir o grau do polinomio
#
##########################################################################
polinomio.ajustado<-function(x,p){
  x<-as.vector(x)
  N<-length(x)
  XX<-matrix(0,N,p)
  for (i in 1:N) {
    for (j in 1:p) XX[i,j]<-i**j}
  aa<-lm(x~XX)
  ts.plot(x)
  lines(aa$fitted,col=2)
  aa$res}
##########################################################################
#
# FIM
#
##########################################################################

