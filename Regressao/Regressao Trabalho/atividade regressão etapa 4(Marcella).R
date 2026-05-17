library(readxl)
library(GGally)      
library(car)         
library(lmtest)      
library(nortest)     
library(MASS)        
library(leaps)       
library(glmnet)  
library(corrplot)
library(caret)
install.packages("caret")


dados_brutos <- read_excel("C:/Users/Marcelo/Documents/Atividades 2026_1/Regressao/Regressao Trabalho/Concrete_Data.xls")
colnames(dados_brutos) <- c("Cimento","Escoria","CinzaVolante","Agua",
                            "Superplastificante","AgregadoGraudo",
                            "AgregadoMiudo","Idade","Resistencia")

# Dividindo os dados em 70/30 para validação
set.seed(42)
n_total <- nrow(dados_brutos)
n_treino <- round(0.70 * n_total)

idx_treino <- sample(1:n_total, size = n_treino)
dados_treino <- dados_brutos[idx_treino, ]
dados_validacao <- dados_brutos[-idx_treino, ]

n_total
nrow(dados_treino)
nrow(dados_validacao)


# ETAPA 1 resumida 

str(dados_treino)

#Dados faltantes por coluna:
colSums(is.na(dados_treino))

summary(dados_treino)

#################################################################
# ETAPA 2: Seleção de variaveis 

# Matriz de correlação (dados brutos)
vars_explicativas <- dados_treino[, c("Cimento", "Escoria", "CinzaVolante", "Agua",
                                      "Superplastificante", "AgregadoGraudo",
                                      "AgregadoMiudo", "Idade", "Resistencia")]

cor_matrix <- cor(vars_explicativas)
print("Matriz de Correlação:")
round(cor_matrix, 2)

# Visualizar matriz de correlação
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         addCoef.col = "white",
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200))

# Nenhuma correlação > 0.7 entre preditores, não indica multicolinearidade, porém
# pode haver restrição estrutural nos dados:
# quando X1 pode ser "reconstruído" por combinações de X2 + X3 + X4...


# Modelo completo (base com dados brutos)
modelo_completo <- lm(Resistencia ~ Cimento + Escoria + CinzaVolante +
                        Agua + Superplastificante + AgregadoGraudo +
                        AgregadoMiudo + Idade,
                      data = dados_treino)

round(coef(modelo_completo), 4)


# Seleção por Cp de mallows
reg_sub <- regsubsets(Resistencia ~ Cimento + Escoria + CinzaVolante +
                        Agua + Superplastificante + AgregadoGraudo +
                        AgregadoMiudo + Idade,
                      data = dados_treino, nbest = 1, nvmax = 8)

resumo_sub <- summary(reg_sub)

tabela_mallows <- data.frame(
  "n_vars" = 1:8,
  "Cp" = round(resumo_sub$cp, 3),
  "R2" = round(resumo_sub$rsq, 4),
  "R2_adj" = round(resumo_sub$adjr2, 4),
  "BIC" = round(resumo_sub$bic, 2)
)

# Seleção por Cp de Mallows
tabela_mallows

# Gráfico Cp
par(mar = c(4, 4, 3, 1))
plot(1:8, resumo_sub$cp, type = "b", pch = 16, cex = 1.2,
     xlab = "Número de variáveis", ylab = "Cp de Mallows",
     main = "Seleção por Cp de Mallows (dados brutos)", col = "steelblue", lwd = 2)
abline(a = 1, b = 1, col = "red", lty = 2, lwd = 2)
abline(h = 9, col = "orange", lty = 3, lwd = 1.5)
legend("topright", c("Cp observado", "Reta ideal (Cp = p+1)"),
       col = c("steelblue", "red"), lty = c(1, 2), pch = c(16, NA))


# Seleção por AIC (Stepwise)
modelo_step_AIC <- stepAIC(modelo_completo, direction = "both", trace = FALSE)
#Modelo selecionado por AIC:
formula(modelo_step_AIC)
round(coef(modelo_step_AIC), 4)


# Seleção por BIC (Stepwise)
modelo_step_BIC <- stepAIC(modelo_completo, direction = "both",
                           k = log(nrow(dados_treino)), trace = FALSE)
# Modelo selecionado por BIC:")
formula(modelo_step_BIC)
round(coef(modelo_step_BIC), 4)


# Seleção por Lasso
X_mat <- model.matrix(Resistencia ~ Cimento + Escoria + CinzaVolante +
                        Agua + Superplastificante + AgregadoGraudo +
                        AgregadoMiudo + Idade,
                      data = dados_treino)[, -1]

y_vec <- dados_treino$Resistencia
set.seed(42)
cv_lasso <- cv.glmnet(X_mat, y_vec, alpha = 1)

plot(cv_lasso, main = "Lasso: Validação Cruzada para seleção de λ (dados brutos)")

coef_lasso_min <- coef(cv_lasso, s = "lambda.min")
vars_lasso <- names(which(coef_lasso_min[-1, 1] != 0))
# Variáveis selecionadas por Lasso:
vars_lasso

# Vemos que, com base no método do CP-Mallows e Stepwise BIC eu deveria escolher
# entre 5 a 6 variaveis, o ganho em R^2 não justifica adicionar 2 ou 3 variáveis
# (de 0.626 para 0.631) como mostra no teste Lasso e AIC
# Para seguir com um meio termo, continuaremos com o modelo usando 6 variaveis

resumo_sub$which[6, ]



###############################################################################
# ETAPA 3: Ajuste, diagnosticos e Teste



modelo_final <- lm(Resistencia ~ Cimento + Escoria + CinzaVolante +
                     Agua + Superplastificante + Idade,
                   data = dados_treino)

summary(modelo_final)

coef_modelo <- coef(modelo_final)

round(coef_modelo, 4)

# Interpretação do R2
r2_modelo <- summary(modelo_final)$r.squared
r2_adj_modelo <- summary(modelo_final)$adj.r.squared
r2 <- round(r2_modelo, 4)
r2_ajus <- round(r2_adj_modelo, 4)
r2
r2_ajus


# Diagnóstico gráfico
par(mfrow = c(2, 2))
plot(modelo_final)
par(mfrow = c(1, 1))
# indicativo de heterocedasticidade com afunilamento no grafico Residuals vs Fitted
# Indicio de normalidade através do QQplot, respeitando bem a linha

# Testes formais:
# Normalidade: Anderson-Darling
ad_test <- ad.test(residuals(modelo_final))
ad_test


# Homocedasticidade: Breusch-Pagan
bp_test <- bptest(modelo_final)
bp_test
#nao aceitou hipotese nula de homocedasticidade


# Independência: Durbin-Watson
dw_test <- dwtest(modelo_final)
dw_test
# sem autoceorrelação aparente


# Multicolinearidade: VIF
vif_values <- vif(modelo_final)
# Todos abaixo de 5, nao indicando multicolinearidade

round(vif_values, 3)

# Resíduos não são perfeitamente normais
# heterocedasticidade detectada
# Sem autocorrelação detectada
# Sem multicolinearidade problemática


################################################################################
#Teste F Global

sumario <- summary(modelo_final)
f_stat_global <- sumario$fstatistic[1]
f_pvalue_global <- pf(f_stat_global, sumario$fstatistic[2], sumario$fstatistic[3], lower.tail = FALSE)

#F-statistic:
round(f_stat_global, 4)
#gl numerador:
sumario$fstatistic[2]
#gl denominador:
sumario$fstatistic[3]
#p-value:
format(f_pvalue_global, scientific = TRUE)
# Interpretação: Modelo é significativo globalmente (rejeita H0: todos β=0)")

################################################################################
# Teste F Parcial
anova_tipo3 <- Anova(modelo_final, type = "III")
print(anova_tipo3)

#Cada linha testa a contribuição de UMA variável controlando por TODAS as outras
#Todos os coeficientes são estatisticamente significativos ao nível de 5%
#são as variáveis mais importantes, seguidas por Escória 
#(F = 229.93) e Água (F = 79.46). Superplastificante apresenta 
#significância marginal (F = 5.86, p = 0.0158), indicando 
#contribuição menor mas ainda relevante ao modelo.

################################################################################
# Detecção de outliers
res_std <- rstandard(modelo_final)
outliers_idx <- which(abs(res_std) > 3)
length(outliers_idx)
# Índices dos outliers:
outliers_idx





################################################################################
# Hii (Hat Values)
n <- nrow(dados_treino)
p <- length(coef(modelo_final))

leverage <- hatvalues(modelo_final)
limiar_leverage <- 2 * p / n  #existe tambem o limiar de hii>0.5, com ele mostra 0 outliers

high_leverage <- which(leverage > limiar_leverage)

# Limiar usado:
round(limiar_leverage, 4)
# Número de pontos com alto leverage:
length(high_leverage)

################################################################################
#DFBETAS
dfbetas_vals <- dfbetas(modelo_final)
dfbetas_limite <- 2/sqrt(n)  # geral, se for amostra pequena/moderada = 1

n_dfbetas_total <- sum(rowSums(abs(dfbetas_vals) > dfbetas_limite) > 0)
round(dfbetas_limite, 4)
n_dfbetas_total

################################################################################
# DFFITS
dffits_vals <- dffits(modelo_final)
dffits_limite <- 2  # amostras grandes, seria 1 se fosse moderada ou pequena

abs_dffits <- abs(dffits_vals)
n_dffits <- sum(abs_dffits > dffits_limite)
round(dffits_limite, 4)
n_dffits



##################################################
cooks_d <- cooks.distance(modelo_final)


cook_limite_formal <- qf(0.5, df1 = p, df2 = n - p)

cook_influentes_formais <- which(cooks_d > cook_limite_formal)

# Limite de Cook Formal 
round(cook_limite_formal, 6)

# Número de pontos influentes 
length(cook_influentes_formais)

################################################################################
# ETAPA 4: Validação do Modelo - LOOCV, K-Fold e Treino/Teste
################################################################################



# Função auxiliar para calcular as métricas
calcular_metricas <- function(real, predito, metodo = "") {
  rmse <- sqrt(mean((real - predito)^2))
  mae  <- mean(abs(real - predito))
  mape <- mean(abs((real - predito) / real)) * 100
  
  cat(sprintf("\n--- %s ---\n", metodo))
  cat(sprintf("  RMSE : %.4f\n", rmse))
  cat(sprintf("  MAE  : %.4f\n", mae))
  cat(sprintf("  MAPE : %.2f%%\n", mape))
  
  return(data.frame(Metodo = metodo, RMSE = round(rmse, 4),
                    MAE = round(mae, 4), MAPE = round(mape, 4)))
}

resultados <- list()

# ------------------------------------------------------------------------------
# 1. LOOCV (Leave-One-Out Cross-Validation)
# ------------------------------------------------------------------------------
ctrl_loocv <- trainControl(method = "LOOCV")

modelo_loocv <- train(
  Resistencia ~ Cimento + Escoria + CinzaVolante + Agua + Superplastificante + Idade,
  data      = dados_treino,
  method    = "lm",
  trControl = ctrl_loocv
)

pred_loocv <- modelo_loocv$pred  # predições fora da amostra

resultados$loocv <- calcular_metricas(
  real    = pred_loocv$obs,
  predito = pred_loocv$pred,
  metodo  = "LOOCV"
)

# ------------------------------------------------------------------------------
# 2. K-Fold Cross-Validation (k = 5)
# ------------------------------------------------------------------------------
set.seed(42)
ctrl_kfold <- trainControl(method = "cv", number = 5, savePredictions = "final")

modelo_kfold <- train(
  Resistencia ~ Cimento + Escoria + CinzaVolante + Agua + Superplastificante + Idade,
  data      = dados_treino,
  method    = "lm",
  trControl = ctrl_kfold
)

pred_kfold <- modelo_kfold$pred

resultados$kfold <- calcular_metricas(
  real    = pred_kfold$obs,
  predito = pred_kfold$pred,
  metodo  = "K-Fold (k=5)"
)

# ------------------------------------------------------------------------------
# 3. Validação Treino/Teste (70%/30%) — usando dados_validacao já separado
# ------------------------------------------------------------------------------
pred_teste <- predict(modelo_final, newdata = dados_validacao)

resultados$treino_teste <- calcular_metricas(
  real    = dados_validacao$Resistencia,
  predito = pred_teste,
  metodo  = "Treino/Teste (70/30)"
)

# ------------------------------------------------------------------------------
# Tabela comparativa final
# ------------------------------------------------------------------------------
tabela_final <- do.call(rbind, resultados)
rownames(tabela_final) <- NULL

cat("\n========================================\n")
cat("       TABELA COMPARATIVA DE MÉTRICAS   \n")
cat("========================================\n")
print(tabela_final)
