# ============================================================
#  Análise de Regressão - Resistência à Compressão do Concreto
#  Estratégia: transformações + interações + diagnóstico + MAPE
# ============================================================

library(readxl)
library(lmtest)
library(MASS)       # boxcox, stepAIC
library(car)        # vif, ncvTest, qqPlot
library(nortest)    # ad.test (Anderson-Darling, mais robusto para n > 50)

# ── 0. Leitura e renomeação ────────────────────────────────
dados_brutos <- read_excel("C:/Users/Marcelo/Documents/Atividades 2026_1/Regressao/Regressao Trabalho/Concrete_Data.xls")
colnames(dados_brutos) <- c("Cimento", "Escoria", "CinzaVolante", "Agua",
                            "Superplastificante", "AgregadoGraudo",
                            "AgregadoMiudo", "Idade", "Resistencia")

cat("Dimensão dos dados:", dim(dados_brutos), "\n")
cat("Resumo das variáveis:\n")
print(summary(dados_brutos))

# ── 1. Divisão treino (70%) / validação (30%) ─────────────
set.seed(42)
n_total  <- nrow(dados_brutos)
n_treino <- round(0.70 * n_total)
idx_treino      <- sample(1:n_total, size = n_treino)
dados_treino    <- dados_brutos[ idx_treino, ]
dados_validacao <- dados_brutos[-idx_treino, ]
cat("\nTreino:", nrow(dados_treino), "| Validação:", nrow(dados_validacao), "\n")

# ── 2. Exploração: Box-Cox para Y ─────────────────────────
# Ajuste auxiliar para guiar a transformação em Y
mod_aux <- lm(Resistencia ~ ., data = dados_treino)
bc <- boxcox(mod_aux, lambda = seq(-2, 2, 0.1), plotit = FALSE)
lambda_otimo <- bc$x[which.max(bc$y)]
cat("\nLambda ótimo Box-Cox (Y):", round(lambda_otimo, 3), "\n")

# Lambda ~ 0.5 → raiz quadrada; ~ 0 → log; ~ 1 → sem transformação
# Vamos testar sqrt e log e escolher pelo diagnóstico

# ── 3. Engenharia de variáveis ─────────────────────────────
# Justificativas físicas / estatísticas:
#   - sqrt(Idade): a resistência cresce rápido no início e se estabiliza
#   - log(Cimento): efeito marginal decrescente
#   - Razão Água/Cimento (a/c): principal driver tecnológico do concreto
#   - Interação Cimento × Idade: cura depende da quantidade de cimento
#   - Cinza e Escória têm comportamento análogo ao cimento (pozolanas)

preparar <- function(df) {
  df$sqrtIdade    <- sqrt(df$Idade)
  df$logIdade    <- log(df$Idade)
  df$logCimento   <- log(df$Cimento + 1)
  df$logEscoria   <- log(df$Escoria + 1)        # zeros possíveis → +1
  df$logCinza     <- log(df$CinzaVolante + 1)
  df$logSP        <- log(df$Superplastificante + 1)
  df$razaoAC      <- df$Agua / df$Cimento
  df$CimXIdade    <- df$Cimento * df$sqrtIdade  # interação
  df$logResist    <- log(df$Resistencia)        # resposta transformada
  df$sqrtResist   <- sqrt(df$Resistencia)       # alternativa
  df$Idade_log2  <- df$logIdade^2
  df
}

treino    <- preparar(dados_treino)
validacao <- preparar(dados_validacao)

# ── 4. Modelos candidatos ──────────────────────────────────
# Modelo A: Y log-transformado, preditores originais + sqrt(Idade)
modA <- lm(logResist ~ Cimento + Escoria + CinzaVolante + Agua +
             Superplastificante + AgregadoGraudo + AgregadoMiudo +
             sqrtIdade,
           data = treino)

# Modelo B: Y log-transformado, preditores log-transformados + interação
modB <- lm(logResist ~ logCimento + logEscoria + logCinza + Agua +
             logSP + AgregadoGraudo + AgregadoMiudo +
             sqrtIdade + CimXIdade,
           data = treino)

# Modelo C: Y sqrt-transformado (caso lambda ≈ 0.5)
modC <- lm(sqrtResist ~ logCimento + logEscoria + logCinza + Agua +
             logSP + AgregadoGraudo + AgregadoMiudo +
             sqrtIdade + CimXIdade + razaoAC,
           data = treino)



# ── 5. Seleção automática (stepwise AIC) em cada candidato ─
cat("\n── Seleção stepwise ──\n")
modA_step <- stepAIC(modA, direction = "both", trace = FALSE)
modB_step <- stepAIC(modB, direction = "both", trace = FALSE)
modC_step <- stepAIC(modC, direction = "both", trace = FALSE)

cat("Modelo A (step) — AIC:", AIC(modA_step), "\n")
cat("Modelo B (step) — AIC:", AIC(modB_step), "\n")
cat("Modelo C (step) — AIC:", AIC(modC_step), "\n")

# ── 6. Função de diagnóstico completo ─────────────────────
diagnostico <- function(mod, nome = "Modelo", dados_val, resp_val_orig,
                        transf = c("log", "sqrt", "nenhuma")) {
  transf <- match.arg(transf)
  cat("\n", rep("=", 60), "\n", sep = "")
  cat(" DIAGNÓSTICO:", nome, "\n")
  cat(rep("=", 60), "\n", sep = "")

  res  <- residuals(mod)
  ajus <- fitted(mod)
  n    <- length(res)
  p    <- length(coef(mod))

  # 6a. Normalidade
  sw  <- shapiro.test(res)
  ad  <- ad.test(res)
  cat("\n[Normalidade]\n")
  cat("  Shapiro-Wilk   p =", round(sw$p.value, 4),
      ifelse(sw$p.value > 0.05, "✔ OK", "✘ FALHA"), "\n")
  cat("  Anderson-Darling p =", round(ad$p.value, 4),
      ifelse(ad$p.value > 0.05, "✔ OK", "✘ FALHA"), "\n")

  # 6b. Homocedasticidade
  bp  <- bptest(mod)
  ncv <- ncvTest(mod)
  cat("\n[Homocedasticidade]\n")
  cat("  Breusch-Pagan  p =", round(bp$p.value, 4),
      ifelse(bp$p.value > 0.05, "✔ OK", "✘ FALHA"), "\n")
  cat("  NCV Test       p =", round(ncv$p, 4),
      ifelse(ncv$p > 0.05, "✔ OK", "✘ FALHA"), "\n")

  # 6c. Autocorrelação
  dw <- dwtest(mod)
  cat("\n[Autocorrelação]\n")
  cat("  Durbin-Watson  p =", round(dw$p.value, 4),
      ifelse(dw$p.value > 0.05, "✔ OK", "✘ FALHA"), "\n")

  # 6d. Multicolinearidade
  vifs <- vif(mod)
  cat("\n[VIF — Multicolinearidade]\n")
  print(round(vifs, 2))
  if (any(vifs > 10)) cat("  ⚠ VIF > 10 em algum preditor\n")
  else cat("  ✔ Todos VIF ≤ 10\n")

  # 6e. R² ajustado
  s <- summary(mod)
  cat("\n[Ajuste no treino]\n")
  cat("  R² ajustado =", round(s$adj.r.squared, 4), "\n")

  # 6f. MAPE na validação
  pred_transf <- predict(mod, newdata = dados_val)
  if (transf == "log") {
    pred_orig <- exp(pred_transf)
  } else if (transf == "sqrt") {
    pred_orig <- pred_transf^2
  } else {
    pred_orig <- pred_transf
  }
  mape <- mean(abs((resp_val_orig - pred_orig) / resp_val_orig)) * 100
  cat("\n[Validação — MAPE]\n")
  cat("  MAPE =", round(mape, 2), "%",
      ifelse(mape < 15, "✔ META ATINGIDA (<15%)", "✘ Acima de 15%"), "\n")

  invisible(list(shapiro = sw, ad = ad, bp = bp, dw = dw,
                 vif = vifs, mape = mape, r2adj = s$adj.r.squared))
}

# ── 7. Rodar diagnósticos ──────────────────────────────────
diag_A <- diagnostico(modA_step, "Modelo A (log-Y + preditores originais)",
                      validacao, validacao$Resistencia, transf = "log")

diag_B <- diagnostico(modB_step, "Modelo B (log-Y + log-preditores + interação)",
                      validacao, validacao$Resistencia, transf = "log")

diag_C <- diagnostico(modC_step, "Modelo C (sqrt-Y + log-preditores + razão a/c)",
                      validacao, validacao$Resistencia, transf = "sqrt")

# ── 8. Selecionar o melhor modelo ─────────────────────────
mapes <- c(A = diag_A$mape, B = diag_B$mape, C = diag_C$mape)
melhor_nome <- names(which.min(mapes))
cat("\n\n══ MELHOR MODELO: Modelo", melhor_nome,
    "| MAPE =", round(mapes[melhor_nome], 2), "% ══\n\n")

modelo_final <- switch(melhor_nome,
                       A = modA_step,
                       B = modB_step,
                       C = modC_step)
print(summary(modelo_final))

# ── 9. Gráficos de diagnóstico do modelo final ────────────
par(mfrow = c(2, 3))

# 9a. Resíduos vs Ajustados
plot(fitted(modelo_final), residuals(modelo_final),
     main = "Resíduos vs Ajustados",
     xlab = "Valores Ajustados", ylab = "Resíduos",
     pch = 20, col = "#2c7bb6")
abline(h = 0, lty = 2, col = "red")

# 9b. QQ-plot
qqnorm(residuals(modelo_final), main = "QQ-plot dos Resíduos", pch = 20)
qqline(residuals(modelo_final), col = "red")

# 9c. Scale-Location
plot(fitted(modelo_final), sqrt(abs(residuals(modelo_final))),
     main = "Scale-Location",
     xlab = "Valores Ajustados", ylab = "√|Resíduos|",
     pch = 20, col = "#2c7bb6")

# 9d. Histograma dos resíduos
hist(residuals(modelo_final), breaks = 30,
     main = "Histograma dos Resíduos",
     xlab = "Resíduos", col = "#abd9e9", border = "white")
curve(dnorm(x, mean = mean(residuals(modelo_final)),
            sd   = sd(residuals(modelo_final))) * length(residuals(modelo_final)) *
        diff(hist(residuals(modelo_final), plot = FALSE)$breaks[1:2]),
      add = TRUE, col = "red", lwd = 2)

# 9e. Predito vs Observado (validação)
transf_melhor <- switch(melhor_nome, A = "log", B = "log", C = "sqrt")
pred_val <- predict(modelo_final, newdata = validacao)
pred_orig <- if (transf_melhor == "log") exp(pred_val) else pred_val^2
plot(validacao$Resistencia, pred_orig,
     main = "Predito vs Observado (Validação)",
     xlab = "Resistência Observada (MPa)",
     ylab = "Resistência Predita (MPa)",
     pch = 20, col = "#d7191c")
abline(0, 1, lty = 2, col = "blue")

# 9f. Cook's Distance
plot(cooks.distance(modelo_final),
     main = "Distância de Cook",
     ylab = "Cook's D", type = "h", col = "#2c7bb6")
abline(h = 4 / length(residuals(modelo_final)), lty = 2, col = "red")

par(mfrow = c(1, 1))

cat("\n✔ Script concluído. Verifique os gráficos e os resultados impressos acima.\n")