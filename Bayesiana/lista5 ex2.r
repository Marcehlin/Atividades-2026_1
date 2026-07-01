# ----------------------------
# 1. FUNÇÃO ALVO (BETA(20,20))
# ----------------------------
target <- function(x) {
  if (x < 0 || x > 1) return(0)  # suporte limitado
  return(dbeta(x, 20, 20))        # ou x^(19)*(1-x)^(19) / beta(20,20)
}

# ----------------------------
# 2. ALGORITMO MH
# ----------------------------
mh_beta <- function(sigma, iter = 10000, init = 0.5) {
  chain <- numeric(iter)
  chain[1] <- init
  accept <- 0
  
  for (i in 2:iter) {
    # Propor um novo valor com passeio aleatório Normal
    prop <- rnorm(1, mean = chain[i-1], sd = sigma)
    
    # Se o valor proposto estiver fora do suporte [0,1], rejeitamos imediatamente
    if (prop < 0 || prop > 1) {
      alpha <- 0
    } else {
      # Cálculo da razão de aceitação (proposta simétrica -> razão = 1)
      alpha <- min(1, target(prop) / target(chain[i-1]))
    }
    
    # Decisão de aceitar ou rejeitar
    if (runif(1) < alpha) {
      chain[i] <- prop
      accept <- accept + 1
    } else {
      chain[i] <- chain[i-1]
    }
  }
  
  return(list(chain = chain, acceptance_rate = accept / iter))
}

# ----------------------------
# 3. TESTAR DIFERENTES VARIÂNCIAS (sigma)
# ----------------------------
set.seed(123)
sigmas <- c(0.02, 0.05, 0.1, 0.2, 0.3)
resultados <- list()

for (s in sigmas) {
  cat("\nTestando sigma =", s, "...\n")
  res <- mh_beta(s, iter = 20000)
  resultados[[as.character(s)]] <- res
  cat("Taxa de Aceitação:", round(res$acceptance_rate * 100, 2), "%\n")
}

# ----------------------------
# 4. VERIFICAR EFICIÊNCIA (GRÁFICOS)
# ----------------------------
par(mfrow = c(2, 3), mar = c(3, 3, 3, 2))

for (s in names(resultados)) {
  chain <- resultados[[s]]$chain
  
  # Trace plot (primeiras 500 iterações para enxergar bem)
  plot(chain[1:500], type = "l", main = paste("Sigma =", s, 
        "- Aceitação:", round(resultados[[s]]$acceptance_rate*100, 1), "%"),
       xlab = "Iteração", ylab = "x", col = "blue")
  
  # Autocorrelação (até lag 40)
  acf(chain, lag.max = 40, main = paste("ACF", s))
}
par(mfrow = c(1, 1))
