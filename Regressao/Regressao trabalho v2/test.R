library(readxl)
library(GGally)
library(car)
library(lmtest)
library(nortest)
library(MASS)
library(leaps)
library(glmnet)
library(corrplot)
library(tseries)
library(moments)

dados_brutos <- read_excel("Concrete_Data.xls")
colnames(dados_brutos) <- c("Cimento","Escoria","CinzaVolante","Agua",
                            "Superplastificante","AgregadoGraudo",
                            "AgregadoMiudo","Idade","Resistencia")

set.seed(22)
set.seed(20260521)
n_total  <- nrow(dados_brutos)
n_treino <- round(0.70 * n_total)
idx_treino    <- sample(1:n_total, size = n_treino)
dados_treino  <- dados_brutos[idx_treino, ]
dados_validacao <- dados_brutos[-idx_treino, ]

dados_treino_t <- as.data.frame(dados_treino)
dados_treino_t$Idade_log      <- log(dados_treino_t$Idade)
dados_treino_t$Cimento_Idade  <- dados_treino_t$Cimento * dados_treino_t$Idade_log

# método Box-Cox que encontra automaticamente o lambda ótimo.

bc <- boxcox(lm(Resistencia ~ Cimento + Escoria + CinzaVolante +
                  Agua + Superplastificante + Idade_log + Cimento_Idade,
                data = dados_treino_t), plotit = TRUE)

lambda_otimo <- bc$x[which.max(bc$y)]
round(lambda_otimo, 4)

# Aplicar transformação Box-Cox com lambda ótimo
dados_treino_t$Y_bc <- (dados_treino_t$Resistencia^lambda_otimo - 1) / lambda_otimo

# Modelo final com Y transformado
modelo_final <- lm(Y_bc ~ Cimento + Escoria + CinzaVolante +
                     Agua + Superplastificante + Idade_log + Cimento_Idade,
                   data = dados_treino_t)

summary(modelo_final)

# Diagnóstico gráfico
par(mfrow = c(2, 2))
plot(modelo_final)
par(mfrow = c(1, 1))


res <- residuals(modelo_final)

bp_test <- bptest(modelo_final)
bp_test

dw_test <- dwtest(modelo_final)
dw_test

ad_test <- ad.test(res)
ad_test


pe_test <- pearson.test(res)
pe_test

sw_test <- shapiro.test(res)
sw_test

li_test <- lillie.test(res)
li_test


# QQ-plot — avaliação visual da normalidade
qqnorm(res,
       main = "QQ-Plot dos Resíduos — Modelo Final",
       pch  = 16, col = "steelblue", cex = 0.6)
qqline(res, col = "red", lwd = 2)

modelo_final <- lm(Y_bc ~ Cimento + Escoria + CinzaVolante +
                     Agua + Superplastificante + Idade_log + Cimento_Idade,
                   data = dados_treino_t)

dados_validacao <- as.data.frame(dados_validacao)
dados_validacao$Idade_log      <- log(dados_validacao$Idade)
dados_validacao$Cimento_Idade  <- dados_validacao$Cimento * dados_validacao$Idade_log




pred_transf <- predict(modelo_final, newdata = dados_validacao)
lambda <- lambda_otimo   # seu valor ótimo
if (abs(lambda) < 1e-6) {
  pred_orig <- exp(pred_transf)
} else {
  pred_orig <- (lambda * pred_transf + 1)^(1/lambda)
}

resp_val_orig <- dados_validacao$Resistencia

mape <- mean(abs((resp_val_orig - pred_orig) / resp_val_orig)) * 100
cat("\n[Validação — MAPE]\n")
cat("  MAPE =", round(mape, 2), "%",
ifelse(mape < 15, "✔ META ATINGIDA (<15%)", "✘ Acima de 15%"), "\n")                   
