# Matriz de dados original
X <- matrix(c(12, 17, 29,
              18, 20, 38,
              14, 16, 30,
              20, 18, 38,
              16, 19, 35), nrow = 5, byrow = TRUE)
colnames(X) <- c("X1", "X2", "X3")

# Calcula a média de cada coluna
medias <- colMeans(X)

# Matriz de desvios: subtrai a média de cada coluna
Xc <- sweep(X, 2, medias, FUN = "-")

# Exibe a matriz de desvios
print(Xc)

X <- matrix(c(12,17,29, 18,20,38, 14,16,30, 20,18,38, 16,19,35), nrow=5, byrow=TRUE)
Xc <- scale(X, center=TRUE, scale=FALSE)
Sn <- cov(X) * (4/5)  # para usar divisor n=5, pois cov() usa n-1
# ou diretamente: Sn <- (t(Xc) %*% Xc) / 5
det(Sn)  # 0
