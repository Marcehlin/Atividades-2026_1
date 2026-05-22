library(readxl)
library(lmtest)
library(MASS)      
library(car)        
library(nortest)

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

dados_brutos <- read_excel("C:/Users/Marcelo/Documents/Atividades 2026_1/Regressao/Regressao Trabalho/Concrete_Data.xls")
colnames(dados_brutos) <- c("Cimento", "Escoria", "CinzaVolante", "Agua",
                            "Superplastificante", "AgregadoGraudo",
                            "AgregadoMiudo", "Idade", "Resistencia")
set.seed(20260521)
set.seed(42)
set.seed(123)
set.seed(12)
n_total  <- nrow(dados_brutos)
n_treino <- round(0.70 * n_total)
idx_treino      <- sample(1:n_total, size = n_treino)
dados_treino    <- dados_brutos[ idx_treino, ]
dados_validacao <- dados_brutos[-idx_treino, ]
cat("\nTreino:", nrow(dados_treino), "| Validação:", nrow(dados_validacao), "\n")


preparar <- function(df) {
  df$sqrtIdade    <- sqrt(df$Idade)
  df$logIdade    <- log(df$Idade)
  df$logCimento   <- log(df$Cimento + 1)
  df$logEscoria   <- log(df$Escoria + 1)        # zeros possíveis → +1
  df$logCinza     <- log(df$CinzaVolante + 1)
  df$logSP        <- log(df$Superplastificante + 1)
  df$razaoAC      <- df$Agua / df$Cimento
  df$CimXIdade    <- df$Cimento * df$Idade  # interação
  df$CimXIdade2    <- (df$Cimento * df$Idade)^2  # interação
  df$logResist    <- log(df$Resistencia)        # resposta transformada
  df$sqrtResist   <- sqrt(df$Resistencia)       # alternativa
  df$log2Idade  <- df$logIdade^2
  df
}
treino    <- preparar(dados_treino)
validacao <- preparar(dados_validacao)

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

modD <-  lm(sqrtResist ~ Cimento + logCimento + Escoria + logEscoria + CinzaVolante + logCinza + Agua +
             logSP + AgregadoGraudo + AgregadoMiudo + Idade +
             sqrtIdade + CimXIdade + razaoAC,
           data = treino)         

modE <- lm(sqrtResist ~ Cimento + Escoria + CinzaVolante + Agua + razaoAC + 
          Superplastificante + AgregadoGraudo + AgregadoMiudo + Idade + 
          logIdade + log2Idade + CimXIdade,
          data = treino )

modF <- lm(sqrtResist ~ Cimento + Escoria + CinzaVolante + Agua + razaoAC + 
          Superplastificante + AgregadoGraudo + AgregadoMiudo + Idade + 
          logIdade + CimXIdade,
          data = treino )

modG <- lm(sqrtResist ~ Cimento + logCimento + Escoria + CinzaVolante + Agua + razaoAC + 
          Superplastificante + AgregadoGraudo + AgregadoMiudo + Idade + 
          logIdade + CimXIdade,
          data = treino )

modH <- lm(sqrtResist ~ Cimento + logCimento + Escoria + logEscoria + CinzaVolante + Agua + razaoAC + 
          Superplastificante + AgregadoGraudo + AgregadoMiudo + Idade + 
          logIdade + CimXIdade,
          data = treino )

modI <- lm(sqrtResist ~ Cimento + logCimento + Escoria + CinzaVolante + logCinza + Agua + razaoAC + 
          Superplastificante + AgregadoGraudo + AgregadoMiudo + Idade + 
          logIdade + CimXIdade,
          data = treino )

modJ <- lm(sqrtResist ~ Cimento + logCimento + Escoria + CinzaVolante + logCinza + Agua + razaoAC + 
          Superplastificante + logSP + AgregadoGraudo + AgregadoMiudo + Idade + 
          logIdade + CimXIdade,
          data = treino )

modK <- lm(sqrtResist ~ Cimento + logCimento + Escoria + CinzaVolante + logCinza + Agua + razaoAC + 
          Superplastificante + logSP + AgregadoGraudo + Idade + 
          logIdade + CimXIdade,
          data = treino )

modL <- lm(sqrtResist ~ Cimento + Escoria + CinzaVolante + Agua + 
          Superplastificante + Idade + 
          logIdade + CimXIdade,
          data = treino )

modL2 <- lm(sqrtResist ~ Cimento + Escoria + CinzaVolante + Agua + 
          Superplastificante + Idade + 
          logIdade + CimXIdade2,
          data = treino )          

diag_A <- diagnostico(modA, "Modelo A (log-Y + preditores originais)",
                      validacao, validacao$Resistencia, transf = "log")

diag_B <- diagnostico(modB, "Modelo B (log-Y + log-preditores + interação)",
                      validacao, validacao$Resistencia, transf = "log")

diag_C <- diagnostico(modC, "Modelo C (sqrt-Y + log-preditores + razão a/c)",
                      validacao, validacao$Resistencia, transf = "sqrt")

diag_D <- diagnostico(modD, "Modelo D",
                      validacao, validacao$Resistencia, transf = "sqrt")

diag_E <- diagnostico(modE, "Modelo E",
                      validacao, validacao$Resistencia, transf = "sqrt")

diag_F <- diagnostico(modF, "Modelo F",
                      validacao, validacao$Resistencia, transf = "sqrt")

diag_G <- diagnostico(modG, "Modelo G",
                      validacao, validacao$Resistencia, transf = "sqrt")

diag_H <- diagnostico(modH, "Modelo H",
                      validacao, validacao$Resistencia, transf = "sqrt")

diag_I <- diagnostico(modI, "Modelo I",
                      validacao, validacao$Resistencia, transf = "sqrt")

diag_J <- diagnostico(modJ, "Modelo J",
                      validacao, validacao$Resistencia, transf = "sqrt")

diag_L <- diagnostico(modL, "Modelo L",
                      validacao, validacao$Resistencia, transf = "sqrt")

diag_L2 <- diagnostico(modL2, "Modelo L2",
                      validacao, validacao$Resistencia, transf = "sqrt")

modE_step <- stepAIC(modE, direction = "both", trace = FALSE)
diag_E_step <- diagnostico(modE_step, "Modelo E",
                      validacao, validacao$Resistencia, transf = "sqrt")

modF_step <- stepAIC(modF, direction = "both", trace = FALSE)
diag_F_step <- diagnostico(modF_step, "Modelo F",
                      validacao, validacao$Resistencia, transf = "sqrt")

modG_step <- stepAIC(modG, direction = "both", trace = FALSE)
diag_G_step <- diagnostico(modG_step, "Modelo G",
                      validacao, validacao$Resistencia, transf = "sqrt")

modH_step <- stepAIC(modH, direction = "both", trace = FALSE)
diag_H_step <- diagnostico(modH_step, "Modelo H",
                      validacao, validacao$Resistencia, transf = "sqrt")

modI_step <- stepAIC(modI, direction = "both", trace = FALSE)
diag_I_step <- diagnostico(modI_step, "Modelo I",
                      validacao, validacao$Resistencia, transf = "sqrt")

modJ_step <- stepAIC(modJ, direction = "both", trace = FALSE)
diag_J_step <- diagnostico(modJ_step, "Modelo J",
                      validacao, validacao$Resistencia, transf = "sqrt")

modL_step <- stepAIC(modL, direction = "both", trace = FALSE)
diag_L_step <- diagnostico(modL_step, "Modelo L",
                      validacao, validacao$Resistencia, transf = "sqrt")

modL2_step <- stepAIC(modL2, direction = "both", trace = FALSE)
diag_L2_step <- diagnostico(modL_step, "Modelo L2",
                      validacao, validacao$Resistencia, transf = "sqrt")                      

modelo_final <- modJ_step
print(summary(modelo_final))

plot(modelo_final)
