library(ICSNP)

xbar <- c(200,300)

mu0 <- c(198,302)

S_inv <- matrix(
c(0.02,-0.01,
 -0.01,0.01),
nrow=2,
byrow=TRUE
)

n <- 45
p <- 2


d <- xbar - mu0


T2 <- n * t(d) %*% S_inv %*% d

T2

library(car)
library(ellipse)
S <- matrix(
c(100,100,
  100,200),
2,2)



n <- 45
p <- 2
S_x_barra <- S/n

raio <- sqrt(
  qf(.95,p,n-p) *
  p*(n-1)/(n-p)
)

el <- ellipse(
  S,
  centre=c(200,300),
  radius=raio
)

plot(
  el,
  type="l",
  asp=1,
  xlab="Cauda",
  ylab="Asa"
)

points(200,300,pch=19)
points(198,302,pch=19,col="red")

points(mu_0[1], mu_0[2], col = "red", pch = 13, cex = 1.5)
text(mu_0[1], mu_0[2], labels = "  (198, 302)", pos = 4, col = "red")

# 1. Definição dos dados do problema
n <- 45
p <- 2
x_barra <- c(200, 300)
S_inv <- matrix(c(0.02, -0.01, -0.01, 0.01), nrow = 2, byrow = TRUE)
S <- solve(S_inv) # Matriz de covariância original
mu_0 <- c(198, 302)

# 2. Teste T2 de Hotelling "na mão" via código
diff_mean <- x_barra - mu_0
T2 <- n * t(diff_mean) %*% S_inv %*% diff_mean
T2 <- as.numeric(T2)

F_calc <- ((n - p) / (p * (n - 1))) * T2
p_valor <- pf(F_calc, df1 = p, df2 = n - p, lower.tail = FALSE)

cat("--- Resultados do Teste --- \n")
cat("Estatística T2:", T2, "\n")
cat("F calculado:   ", F_calc, "\n")
cat("p-valor:       ", p_valor, "\n\n")

# 3. Desenhar a Elipse de Confiança de 95%
library(car)

# O gráfico da região de confiança para a MÉDIA populacional (mu) 
# usa a matriz de covariância do estimador da média, que é S / n.
S_media <- S / n
val_critico_F <- qf(0.95, df1 = p, df2 = n - p)
c_quadrado <- (p * (n - 1) / (n - p)) * val_critico_F

# Gerando o gráfico
plot(x_barra[1], x_barra[2], col = "blue", pch = 19, 
     xlim = c(190, 210), ylim = c(290, 310),
     xlab = "Cauda", ylab = "Asa", 
     main = "Elipse de Confiança de 95% para a Média")

# Desenha a elipse centrada em x_barra
ellipse(center = x_barra, shape = S_media, radius = sqrt(c_quadrado), 
        col = "lightblue", fill = TRUE, lty = 2)

# Adiciona o ponto de Murilo para comparação
points(mu_0[1], mu_0[2], col = "red", pch = 13, cex = 1.5)
text(mu_0[1], mu_0[2], labels = "  Murilo (198, 302)", pos = 4, col = "red")
points(x_barra[1], x_barra[2], col = "blue", pch = 19)
text(x_barra[1], x_barra[2], labels = "  Média Amostral", pos = 3, col = "blue")