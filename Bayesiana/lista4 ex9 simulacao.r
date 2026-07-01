# ------------------------------------------------------------
# 1. DEFINIR OS PARÂMETROS DA POSTERIORI (a' e b')
# ------------------------------------------------------------

a_linha <- 4   # shape da Gamma posterior
b_linha <- 2   # rate da Gamma posterior


# ------------------------------------------------------------
# 2. SIMULAR UMA AMOSTRA GRANDE DE lambda DA POSTERIORI
# ------------------------------------------------------------

S <- 100000

# R usa rate, então é direto
lambda_sim <- rgamma(
  n = S,
  shape = a_linha,
  rate = b_linha
)


# ------------------------------------------------------------
# 3. CRIAR O VETOR DE x (GRADE)
# ------------------------------------------------------------

x_grid <- seq(0, 5, by = 0.01)


# ------------------------------------------------------------
# 4. MÉDIA DE MONTE CARLO
#    E[lambda exp(-lambda x)]
# ------------------------------------------------------------

pred_mc <- sapply(
  x_grid,
  function(x) {
    mean(lambda_sim * exp(-lambda_sim * x))
  }
)


# ------------------------------------------------------------
# 5. DENSIDADE ANALÍTICA (LOMAX)
#    f(x)= a*b^a / (b+x)^(a+1)
# ------------------------------------------------------------

pred_analitica <- 
  a_linha * b_linha^a_linha /
  (b_linha + x_grid)^(a_linha + 1)



# ------------------------------------------------------------
# 6. GRÁFICO
# ------------------------------------------------------------

plot(
  x_grid,
  pred_mc,
  type = "l",
  lwd = 2,
  xlab = "x (tempo de vida do novo produto)",
  ylab = "Densidade preditiva f(x | dados)",
  main = "Monte Carlo vs Distribuição Lomax"
)

lines(
  x_grid,
  pred_analitica,
  col= "blue",
  lwd = 2,
  lty = 2
)

legend(
  "topright",
  legend = c(
    "Monte Carlo (média exponenciais)",
    "Analítica (Lomax)"
  ),
  lty = c(1,2),
  lwd = 2
)


# ------------------------------------------------------------
# 7. DIFERENÇA MÁXIMA
# ------------------------------------------------------------

diferenca_max <- max(abs(pred_mc - pred_analitica))

cat(
  "Diferença máxima entre curvas:",
  round(diferenca_max, 6),
  "\n"
)
