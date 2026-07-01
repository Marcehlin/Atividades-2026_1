fdpfunction<-function(nu,x) x^{-nu/2-1}*exp(-1/(2*x))
#
iter<-100
y<-NULL
prob_ace<-NULL
y[1]<-1
aceit<-0
set.seed(100)
lambda <- 3
for (i in 2:iter){
	x_prop<-rexp(1,lambda)
	u<-runif(1,0,1)
	prob_ace[i]<-min(1,fdpfunction(5,x_prop)/fdpfunction(5,y[i-1]) * exp(lambda*(x_prop - y[i-1])))
	if (u<=prob_ace[i]){
		y[i]<-x_prop
		aceit<-aceit+1} else y[i]<-y[i-1]}
plot(prob_ace)
taxa_aceit<-aceit/iter
taxa_aceit
plot(y,type='l')


# Função alvo (núcleo da distribuição Inverse Gamma)
fdpfunction <- function(nu, x) {
  x^(-nu/2 - 1) * exp(-1 / (2 * x))
}

# Função que roda o MCMC com a proposta EXPONENCIAL corrigida
run_mcmc_exp <- function(lambda, iter = 5000, nu = 5, inicio = 1) {
  y <- numeric(iter)
  y[1] <- inicio
  aceit <- 0
  
  for (i in 2:iter) {
    # 1. Propor um valor da Exponencial(lambda)
    x_prop <- rexp(1, rate = lambda)
    
    # 2. Razão de verossimilhança (alvo)
    log_target_ratio <- log(fdpfunction(nu, x_prop)) - log(fdpfunction(nu, y[i-1]))
    
    # 3. Razão das propostas (CORREÇÃO para assimetria)
    # q(atual | prop) / q(prop | atual) = exp(lambda * (x_prop - atual))
    log_q_ratio <- lambda * (x_prop - y[i-1])
    
    # 4. Log da probabilidade de aceitação
    log_alpha <- min(0, log_target_ratio + log_q_ratio)
    
    # 5. Decidir se aceita
    if (log(runif(1)) <= log_alpha) {
      y[i] <- x_prop
      aceit <- aceit + 1
    } else {
      y[i] <- y[i-1]
    }
  }
  
  return(list(chain = y, acceptance_rate = aceit / iter))
}

# Função que roda o MCMC com a proposta UNIFORME original (simétrica, sem correção)
run_mcmc_unif <- function(iter = 5000, nu = 5, inicio = 1) {
  y <- numeric(iter)
  y[1] <- inicio
  aceit <- 0
  
  for (i in 2:iter) {
    x_prop <- runif(1, 0, 100)
    log_target_ratio <- log(fdpfunction(nu, x_prop)) - log(fdpfunction(nu, y[i-1]))
    log_alpha <- min(0, log_target_ratio) # Razão das propostas = 1 (Uniforme simétrica)
    
    if (log(runif(1)) <= log_alpha) {
      y[i] <- x_prop
      aceit <- aceit + 1
    } else {
      y[i] <- y[i-1]
    }
  }
  
  return(list(chain = y, acceptance_rate = aceit / iter))
}

# ----------------------------------------------
# 1. TESTAR VÁRIOS LAMBDAS
# ----------------------------------------------
lambdas <- c(0.5, 1, 2, 3, 5, 10)
resultados <- list()

set.seed(123) # para reprodutibilidade
for (lam in lambdas) {
  cat("\nRodando para lambda =", lam, "...\n")
  resultados[[paste0("exp_", lam)]] <- run_mcmc_exp(lambda = lam, iter = 10000)
}

# Rodar também a Uniforme para comparação
resultados[["uniforme"]] <- run_mcmc_unif(iter = 10000)

# ----------------------------------------------
# 2. VISUALIZAR OS RESULTADOS
# ----------------------------------------------

# a) Taxas de Aceitação
cat("\n========== TAXAS DE ACEITAÇÃO ==========\n")
for (nome in names(resultados)) {
  cat(sprintf("%s: %.2f%%\n", nome, resultados[[nome]]$acceptance_rate * 100))
}

# b) Gráficos de Convergência (Trace) e Autocorrelação
par(mfrow = c(3, 4), mar = c(2, 2, 2, 1))
for (nome in names(resultados)) {
  chain <- resultados[[nome]]$chain
  # Trace plot (primeiras 500 iterações para visualizar bem)
  plot(chain[1:500], type = "l", main = paste("Trace -", nome), 
       xlab = "Iteração", ylab = "Valor", col = "blue")
  # Autocorrelação (até lag 50)
  acf(chain, lag.max = 50, main = paste("ACF -", nome))
}
par(mfrow = c(1, 1))