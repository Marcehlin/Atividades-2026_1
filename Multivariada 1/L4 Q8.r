#A
X1 <- c(1,2,3,3,4,5,6,8,9,11)
X2 <- c(18.95,19.00,17.95,15.54,14.00,12.95,8.94,7.49,6.00,3.99)

matriz <- cbind(X1,X2)

colMeans(matriz)
cov(matriz)

d_j <- mahalanobis(matriz,colMeans(matriz),cov(matriz))
d_j

#ou a "mão"
X <- matrix(matriz, ncol = 2)
d <- mahalanobis(X,colMeans(X),cov(X))
inversa <- solve(cov(X))
ponto <- X[1, ]
diferenca <- ponto - colMeans(X)
#(x - μ)' * Σ^-1
parte1 <- diferenca %*% inversa
#A * (x - μ)
distancia_quadrada <- parte1 %*% diferenca

#B 

distancias_ordenada <- sort(d_j)
n <- nrow(matriz)
#quantis teoricos:
i <- (1:n) / n #vetor de 1 a n=500

#i <- (i-0.5)/n #ajustar

quantis_teoricos <- qchisq(i,df=2)

plot(distancias_ordenada,quantis_teoricos)
