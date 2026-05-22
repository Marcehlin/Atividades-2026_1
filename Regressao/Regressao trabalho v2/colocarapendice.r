library(readxl)
library(lmtest)
library(MASS)      
library(car)        
library(nortest)

dados_brutos <- read_excel("Concrete_Data.xls")
colnames(dados_brutos) <- c("Cimento","Escoria","CinzaVolante","Agua",
                            "Superplastificante","AgregadoGraudo",
                            "AgregadoMiudo","Idade","Resistencia")

set.seed(20260521)

n_total  <- nrow(dados_brutos)
n_treino <- round(0.70 * n_total)
idx_treino      <- sample(1:n_total, size = n_treino)
dados_treino    <- dados_brutos[ idx_treino, ]
dados_validacao <- dados_brutos[-idx_treino, ]
cat("\nTreino:", nrow(dados_treino), "| Validação:", nrow(dados_validacao), "\n")     

treino    <- preparar(dados_treino)
validacao <- preparar(dados_validacao)


### função que automatiza os testes
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

preparar <- function(df) {
  df$sqrtIdade    <- sqrt(df$Idade)
  df$logIdade    <- log(df$Idade)
  df$logCimento   <- log(df$Cimento + 1)
  df$logCinza     <- log(df$CinzaVolante + 1)  # zeros possíveis → +1
  df$logSP        <- log(df$Superplastificante + 1)
  df$razaoAC      <- df$Agua / df$Cimento
  df$CimXIdade    <- df$Cimento * df$Idade  # interação
  df$CimXIdade2    <- (df$Cimento * df$Idade)^2  # interação
  df$sqrtResist   <- sqrt(df$Resistencia)       # alternativa
  df
}


modL <- lm(sqrtResist ~ Cimento + Escoria + CinzaVolante + Agua + 
          Superplastificante + Idade + AgregadoGraudo + AgregadoMiudo + 
          logIdade + CimXIdade,
          data = treino )

# Matriz de correlação (dados brutos)
library(corrplot)
vars_explicativas <- treino[, c("Cimento", "Escoria", "CinzaVolante", "Agua",
                                      "Superplastificante", "AgregadoGraudo",
                                      "AgregadoMiudo", "Idade", "logIdade", "CimXIdade")]

cor_matrix <- cor(vars_explicativas)
print("Matriz de Correlação:")
round(cor_matrix, 2)

# Visualizar matriz de correlação
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         addCoef.col = "white",
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200))

par(mfrow = c(4, 3))
plot(dados_treino$Cimento, dados_treino$Resistencia,
     main = "Cimento", xlab = "Cimento (kg/m³)", ylab = "Resistência (MPa)",
     pch = 16, col = rgb(0.2, 0.4, 0.8, 0.5), cex = 0.7)

plot(dados_treino$Escoria, dados_treino$Resistencia,
     main = "Escória", xlab = "Escória (kg/m³)", ylab = "",
     pch = 16, col = rgb(0.2, 0.4, 0.8, 0.5), cex = 0.7)

plot(dados_treino$CinzaVolante, dados_treino$Resistencia,
     main = "Cinza Volante", xlab = "Cinza Volante (kg/m³)", ylab = "",
     pch = 16, col = rgb(0.2, 0.4, 0.8, 0.5), cex = 0.7)

plot(dados_treino$Agua, dados_treino$Resistencia,
     main = "Água", xlab = "Água (kg/m³)", ylab = "Resistência (MPa)",
     pch = 16, col = rgb(0.2, 0.4, 0.8, 0.5), cex = 0.7)

plot(dados_treino$Superplastificante, dados_treino$Resistencia,
     main = "Superplastificante", xlab = "Superplast. (kg/m³)", ylab = "",
     pch = 16, col = rgb(0.2, 0.4, 0.8, 0.5), cex = 0.7)

plot(dados_treino$AgregadoGraudo, dados_treino$Resistencia,
     main = "Agregado Graúdo", xlab = "Ag. Graúdo (kg/m³)", ylab = "",
     pch = 16, col = rgb(0.2, 0.4, 0.8, 0.5), cex = 0.7)

plot(dados_treino$AgregadoMiudo, dados_treino$Resistencia,
     main = "Agregado Miúdo", xlab = "Ag. Miúdo (kg/m³)", ylab = "Resistência (MPa)",
     pch = 16, col = rgb(0.2, 0.4, 0.8, 0.5), cex = 0.7)

plot(dados_treino$Idade, dados_treino$Resistencia,
     main = "Idade (sem transformação)", xlab = "Idade (dias)", ylab = "",
     pch = 16, col = rgb(0.8, 0.2, 0.2, 0.5), cex = 0.7)
# curva completamente não linear

plot(log(dados_treino$Idade), dados_treino$Resistencia,
     main = "Idade (log)", xlab = "log(Idade)", ylab = "",
     pch = 16, col = rgb(0.2, 0.8, 0.2, 0.5), cex = 0.7)
# Nota: Muito mais linear!

plot((treino$CimXIdade2), dados_treino$Resistencia,
     main = "Cimento*Idade", xlab = "cimento * idade", ylab = "",
     pch = 16, col = rgb(0.2, 0.8, 0.2, 0.5), cex = 0.7)

par(mfrow = c(1, 1))

modelo_step_BIC <- stepAIC(modL, direction = "both",
                           k = log(nrow(treino)), trace = FALSE)
# Modelo selecionado por BIC:")

formula(modelo_step_BIC)
round(coef(modelo_step_BIC), 4)

modL_step <- stepAIC(modL, direction = "both", trace = FALSE)
diag_L_step <- diagnostico(modL_step, "Modelo L",
                      validacao, validacao$Resistencia, transf = "sqrt")

library(lmtest)
residuos <- residuals(modL_step)
ajustados <- fitted(modL_step)

par(mfrow = c(2, 1))

plot(modL_step, which = 2)
plot(ajustados, residuos,
     xlab = "Valores Ajustados", ylab = "Resíduos",
     main = "Resíduos vs Ajustados")
abline(h = 0, col = "red")

par(mfrow = c(1, 1))
res_std <- rstandard(modL_step)
outliers_idx <- which(abs(res_std) > 3)
length(outliers_idx)
# Índices dos outliers:
outliers_idx

outliers_idx <- which(abs(res_std) > 3)
if(length(outliers_idx) > 0) {
  cat("Possíveis outliers (resíduo studentizado > |3|):", outliers_idx, "\n")
  print(dados_treino[outliers_idx, ])
} else {
  cat("Nenhum outlier grave detectado.\n")
}

plot(fitted(modL_step), res_std,
     xlab = "Valores ajustados", ylab = "Resíduos studentizados",
     main = "Resíduos studentizados vs Ajustados")
abline(h = c(-3, 0, 3), lty = c(2,1,2), col = c("red","gray","red"))

h <- hatvalues(modL_step)
limiar_h <- 2 * length(coef(modL_step)) / nrow(treino)   # 2p/n
plot(h, type = "h", col = ifelse(h > limiar_h, "red", "black"),
     main = "Alavancagem (os Hii)", ylab = "h_ii")
abline(h = limiar_h, lty = 2, col = "blue")

sum(h > limiar_h)

n <- nrow(treino)
p <- length(coef(modL_step))

#D-COOK

#se eu remover essa observação, o modelo inteiro muda muito?
# mede influência global.
cooks_d <- cooks.distance(modL_step)
cook_limite_formal <- qf(0.5, df1 = p, df2 = n - p)
cook_influentes_formais <- which(cooks_d > cook_limite_formal)

# Limite de Cook Formal 
round(cook_limite_formal, 6)

# Número de pontos influentes 
length(cook_influentes_formais)

# Nenhuma observação apresentou distância de Cook acima do limite formal,
# sugerindo ausência de pontos altamente influentes no ajuste global
# da regressão.

# grafico
plot(cooks_d, type = "h", col = ifelse(cooks_d > cook_limite_formal, "red", "black"),
     main = "Distância de Cook", ylab = "Cook's D")
abline(h = cook_limite_formal, lty = 2, col = "blue")

#mede impacto no fitted
dffits_vals <- dffits(modL_step)
dffits_limite <- 2  # amostras grandes

abs_dffits <- abs(dffits_vals)
n_dffits <- sum(abs_dffits > dffits_limite)
round(dffits_limite, 4)
n_dffits

plot(dffits_vals, type = "h", col = ifelse(abs(dffits_vals) > dffits_limite, "red", "black"),
     main = "DFFITS")
abline(h = c(-dffits_limite, dffits_limite), lty = 2, col = "blue")
# Nenhuma observação apresentou DFFITS acima do limite adotado,
# indicando ausência de pontos com forte impacto nas predições do modelo.

# DETECÇÃO DE PONTOS INFLUENTES

#DFBETAS
#essa observação altera muito algum coeficiente β?
dfbetas_vals <- dfbetas(modL_step)
dfbetas_limite <- 2/sqrt(n)  

n_dfbetas_total <- sum(rowSums(abs(dfbetas_vals) > dfbetas_limite) > 0)
round(dfbetas_limite, 4)
n_dfbetas_total

# O critério DFBETAS identificou 143 observações potencialmente
# influentes nos coeficientes.
#o limite 2/sqrt(n) torna-se bastante sensível em
# amostras grandes, resultando na detecção de pequenas alterações
# locais nos coeficientes.
#o limite ficou 0.0745, qualquer observação que altere um coeficiente em mais de 
#0.074 desvios padrão será marcada, muito sensivel.


# Testando com o critério alternativo ∣DFBETAS∣>1
# essa observação altera o coeficiente em mais de 1 erro padrão?
# critério complementar mais conservador.
dfbetas_limite2 <- 1  
n_dfbetas_total2 <- sum(rowSums(abs(dfbetas_vals) > dfbetas_limite2) > 0)
round(dfbetas_limite2, 4)
n_dfbetas_total2
# Nenhuma observação apresentou DFBETAS acima do limite adotado, nenhuma
#altera o coeficiente em mais de 1 erro padrao
summary(modL_step)
anova_tipo3 <- Anova(modL_step, type = "III")
print(anova_tipo3)

predicoes_trans<- predict(modL_step, newdata = validacao)
predicoes <- predicoes_trans^2
validacao$Pred_Resistencia <- predicoes

print(validacao[,c("Resistencia","Pred_Resistencia")], n = 15)

# erros
erro_teste <- validacao$Resistencia - validacao$Pred_Resistencia

# RMSE
RMSE_teste <- sqrt(mean(erro_teste^2))

# MAE
mae_teste <- mean(abs(erro_teste))

#MAPE
mape <- mean(abs(erro_teste) / validacao$Resistencia)

# R² preditivo (correlação ao quadrado)
r2_pred <- cor(validacao$Resistencia, validacao$Pred_Resistencia)^2

# Exibir resultados
cat("RMSE_teste:", round(RMSE_teste, 4), "\n",
    "MAE_teste:", round(mae_teste, 4), "\n",
    "MAPE:", round(mape,4), "\n",
    "R² preditivo:", round(r2_pred, 4), "\n")

predicoes_treino_trans <- predict(modL_step, newdata = treino)

predicoes_treino <- predicoes_treino_trans^2

treino$Pred_Resistencia <- predicoes_treino
# erros
erro_treino <- treino$Resistencia - treino$Pred_Resistencia

# RMSE
rmse_treino <- sqrt(mean(erro_treino^2))

# MAE
mae_treino <- mean(abs(erro_treino))

# Exibir resultados
cat("RMSE_treino:", round(rmse_treino, 4), "\n",
    "MAE_treino:", round(mae_treino, 4), "\n")
coef(modL_step)
dados_brutos <- preparar(dados_brutos)
modelo_final <- lm(sqrtResist ~ Cimento + Escoria + CinzaVolante + Agua + 
           + Idade + AgregadoGraudo + AgregadoMiudo + 
          logIdade + CimXIdade,
          data = dados_brutos )
coef(modelo_final)
