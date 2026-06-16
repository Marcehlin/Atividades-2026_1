X1 <- c(2,8,6,8)
X2 <- c(12,9,9,10)

dados <- cbind(X1,X2)

dados <- data.frame(X1, X2)

library(ICSNP)
HotellingsT2(dados, mu = c(7,11))

cov(dados)

qf(0.95,2,2)

9*50/(44*3) 

16*9
