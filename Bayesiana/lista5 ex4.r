# ------------------------------------------------------------
# FUNÇÃO PARA GERAR DA GAMA INVERSA
# Se Y ~ Gama(shape, rate), então 1/Y ~ Inv-Gama(shape, rate)
# ------------------------------------------------------------
rinvgamma <- function(n, shape, rate) {
  return(1 / rgamma(n, shape = shape, rate = rate))
}

# ------------------------------------------------------------
# ALGORITMO GIBBS SAMPLING PARA A NORMAL COM PRIORI JEFFREYS
# ------------------------------------------------------------
gibbs_normal_jeffreys <- function(dados, n_iter = 10000, burnin = 1000) {
  
  n <- length(dados)
  xbar <- mean(dados)
  
  # Inicialização dos parâmetros (qualquer valor funciona)
  mu <- xbar
  sigma2 <- 1
  
  # Matriz para guardar as amostras
  amostras_mu <- numeric(n_iter)
  amostras_sigma2 <- numeric(n_iter)
  
  for (t in 1:n_iter) {
    # Passo 1: Amostrar mu | sigma2, dados
    # Var(mu) = sigma2 / n
    var_mu <- sigma2 / n
    mu <- rnorm(1, mean = xbar, sd = sqrt(var_mu))
    
    # Passo 2: Amostrar sigma2 | mu, dados
    # shape = n/2, rate = (1/2) * sum((dados - mu)^2)
    shape_sigma <- n / 2
    rate_sigma <- 0.5 * sum((dados - mu)^2)
    sigma2 <- rinvgamma(1, shape = shape_sigma, rate = rate_sigma)
    
    # Armazenar os valores
    amostras_mu[t] <- mu
    amostras_sigma2[t] <- sigma2
  }
  
  # Descartar o burn-in
  return(list(
    mu = amostras_mu[(burnin+1):n_iter],
    sigma2 = amostras_sigma2[(burnin+1):n_iter]
  ))
}

# ------------------------------------------------------------
# EXEMPLO DE USO
# ------------------------------------------------------------
set.seed(123)

# Gerar dados simulados (para testar)
n <- 30
dados <- rnorm(n, mean = 5, sd = 2)  # verdadeiro mu = 5, sigma^2 = 4

# Rodar o Gibbs
resultado <- gibbs_normal_jeffreys(dados, n_iter = 11000, burnin = 1000)

# Verificar os resultados
par(mfrow = c(2, 2))

# Trace plot para mu
plot(resultado$mu, type = "l", col = "blue", 
     main = "Cadeia de mu", xlab = "Iteração", ylab = "mu")

# Trace plot para sigma2
plot(resultado$sigma2, type = "l", col = "red", 
     main = "Cadeia de sigma^2", xlab = "Iteração", ylab = "sigma^2")

# Histograma de mu com a média amostral
hist(resultado$mu, breaks = 30, prob = TRUE, col = "lightblue",
     main = "Posteriori de mu", xlab = "mu")
abline(v = mean(dados), col = "darkgreen", lwd = 2, lty = 2)

# Histograma de sigma2
hist(resultado$sigma2, breaks = 30, prob = TRUE, col = "lightpink",
     main = "Posteriori de sigma^2", xlab = "sigma^2")

par(mfrow = c(1, 1))

# Estatísticas resumo
cat("Média a posteriori de mu:", mean(resultado$mu), "\n")
cat("Média a posteriori de sigma^2:", mean(resultado$sigma2), "\n")
cat("Intervalo de 95% para mu:", quantile(resultado$mu, c(0.025, 0.975)), "\n")
