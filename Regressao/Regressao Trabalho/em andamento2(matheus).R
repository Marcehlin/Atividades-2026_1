library(ggplot2)
library(readxl)
library(GGally)      
library(car)         
library(lmtest)      
library(nortest)     
library(MASS)        
library(leaps)       
library(glmnet)   

dados_brutos <- read_excel("C:/Users/Marcelo/Documents/Atividades 2026_1/Regressao/Regressao Trabalho/Concrete_Data.xls")
colnames(dados_brutos) <- c("Cimento","Escoria","CinzaVolante","Agua",
                            "Superplastificante","AgregadoGraudo",
                            "AgregadoMiudo","Idade","Resistencia")
dados_brutos
# dividindo os dados em 70/30 para a validação
set.seed(42)
n_total <- nrow(dados_brutos)
n_treino <- round(0.70 * n_total)

idx_treino <- sample(1:n_total, size = n_treino)
dados_treino <- dados_brutos[idx_treino, ]
dados_validacao <- dados_brutos[-idx_treino, ]

n_total
nrow(dados_treino)
nrow(dados_validacao)


# Etapa 1 resumida. Exploratória e vendo se há necessidade de transformação

str(dados_treino)
print(colSums(is.na(dados_treino))) # pra confirmar que não há dados faltantes

summary(dados_treino)


# Gráficos de dispersão da resistencia a compressão com cada covariável 
par(mfrow = c(3, 3))
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
par(mfrow = c(1, 1))

# Para decidir as transformaçãoes, tive em mente o seguinte:
# No gráfico de Y com a IDADE ele mostra uma relação logarítmica, sobe bem rapido até
# certo ponto, depois de uns 100 dias a curva começa a cair e ficar horizontal.
# Isso se dá pois, de fato, o concreto tem ganho de resistência rápido
# no início e desacelera com o tempo.
# A transformação sugerida é log(Idade) 

# Agora, em relação ao superplastificante, cerca de 36% dos dados são compostos de zeros
# portanto, temos uma distri. assimétrica com a maioria dos dados concentrados em valores 
# baixos. Sem transformação, os poucos pontos altos teriam um peso consideravel na 
# inclinação da reta do modelo. 
# Portanto, se transformar através de log x+1, o log puxa valores altos para mais perto 
# do centro e da uma "espalhada" nos valores baixos.
t0 <- sum(dados_treino$Superplastificante == 0)
t0
t0/nrow(dados_treino)


# Nas demais variaveis: Gráficos não indicam não-linearidade marcante, nao forma 
# uma linha reta perfeita porem nao há curva, é apenas uma relacoa linear com muito ruido.\n")
# A decisão é de manter a escala original dessas.

# transformações ao conjunto de treino
dados_treino$log_Idade <- log(dados_treino$Idade)
dados_treino$log1p_Superplast <- log1p(dados_treino$Superplastificante)

dados_treino$Idade_quad <- (dados_treino$Idade)^2

# transformações ao conjunto de validação
dados_validacao$log_Idade <- log(dados_validacao$Idade)
dados_validacao$log1p_Superplast <- log1p(dados_validacao$Superplastificante)


# 
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

plot(dados_treino$Superplastificante, dados_treino$Resistencia,
     main = "Antes: Y vs Superplastificante", xlab = "Superplastificante", ylab = "Resistência (MPa)",
     pch = 16, col = rgb(0.8, 0.2, 0.2, 0.5), cex = 0.7)
lines(lowess(dados_treino$Superplastificante, dados_treino$Resistencia), col = "red", lwd = 2)

plot(dados_treino$log1p_Superplast, dados_treino$Resistencia,
     main = "Depois: Y vs log1p_Superplast", xlab = "log1p_Superplast", ylab = "Resistência (MPa)",
     pch = 16, col = rgb(0.2, 0.8, 0.2, 0.5), cex = 0.7)
lines(lowess(dados_treino$log1p_Superplast, dados_treino$Resistencia), col = "green", lwd = 2)


# Etapa 2 - Analise e selecão de variaveis

# Matriz de correlação
vars_explicativas <- dados_treino[, c("Cimento", "Escoria", "CinzaVolante", "Agua",
                                      "log1p_Superplast", "AgregadoGraudo",
                                      "AgregadoMiudo", "log_Idade", "Resistencia")]

cor_matrix <- cor(vars_explicativas)
print(round(cor_matrix, 2))

# Gráfico da matriz de correlação
ggpairs(vars_explicativas,
        upper = list(continuous = wrap("cor", size = 3, color = "blue")),
        lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
        title = "Matriz de dispersão e correlação (conjunto treino, transformado)")

# Informações importantes:
# Idade é disparadamente o preditor mais importante (r = 0,86). 
# Cimento, Escória e Cinza Volante também correlacionam positivamente. 
# Água tem relação negativa (clássica na engenharia.

#Em relação a multicolinearidade:
# Correlações > 0.7 entre preditores (indício de colinearidade)
# Nenhuma correlação está acima de 0.7 entre preditores, portanto, ainda não elimino
# nenhuma covar.

# Modelo completo (para servir de base)
modelo_completo <- lm(Resistencia ~ Cimento + Escoria + CinzaVolante +
                        Agua + log1p_Superplast + AgregadoGraudo +
                        AgregadoMiudo + log_Idade,
                      data = dados_treino)

print(round(coef(modelo_completo), 4))


# Seleção por Cp de Mallows
reg_sub <- regsubsets(Resistencia ~ Cimento + Escoria + CinzaVolante +
                        Agua + log1p_Superplast + AgregadoGraudo +
                        AgregadoMiudo + log_Idade,
                      data = dados_treino, nbest = 1, nvmax = 8)

resumo_sub <- summary(reg_sub)

tabela_mallows <- data.frame(
  "n_vars" = 1:8,
  "Cp" = round(resumo_sub$cp, 3),
  "R2" = round(resumo_sub$rsq, 4),
  "R2_adj" = round(resumo_sub$adjr2, 4),
  "BIC" = round(resumo_sub$bic, 2)
)

print(tabela_mallows)

#Interpretação: Modelo com Cp proximo a (p+1) é preferível
#p = número de preditores. Por exemplo: 8 preditores, Cp ≈ 9 é ideal

par(mar = c(4, 4, 3, 1))
plot(1:8, resumo_sub$cp, type = "b", pch = 16, cex = 1.2,
     xlab = "Número de variáveis", ylab = "Cp de Mallows",
     main = "Seleção por Cp de Mallows", col = "steelblue", lwd = 2)
abline(a = 1, b = 1, col = "red", lty = 2, lwd = 2)
abline(h = 9, col = "orange", lty = 3, lwd = 1.5)  # p+1 para 8 preditores
legend("topright", c("Cp observado", "Reta ideal (Cp = p+1)"),
       col = c("steelblue", "red"), lty = c(1, 2), pch = c(16, NA))


# Seleção por AIC (Stepwise)
modelo_step_AIC <- stepAIC(modelo_completo, direction = "both", trace = FALSE)
print(formula(modelo_step_AIC))
# coeficientes
print(round(coef(modelo_step_AIC), 4))


# Seleção por BIC(Stepwise Bidirecional)
modelo_step_BIC <- stepAIC(modelo_completo, direction = "both",
                           k = log(nrow(dados_treino)), trace = FALSE)
print(formula(modelo_step_BIC))
# coeficientes
print(round(coef(modelo_step_BIC), 4))


#Seleção por Lasso
X_mat <- model.matrix(Resistencia ~ Cimento + Escoria + CinzaVolante +
                        Agua + log1p_Superplast + AgregadoGraudo +
                        AgregadoMiudo + log_Idade,
                      data = dados_treino)[, -1]

y_vec <- dados_treino$Resistencia
set.seed(42)
cv_lasso <- cv.glmnet(X_mat, y_vec, alpha = 1)

plot(cv_lasso, main = "Lasso: Validação Cruzada para seleção de λ")


coef_lasso_min <- coef(cv_lasso, s = "lambda.min")
# Coeficientes Lasso (λ mínimo)
print(as.matrix(coef_lasso_min))
# Variáveis selecionadas (coef ≠ 0)
vars_lasso <- names(which(coef_lasso_min[-1, 1] != 0))
print(vars_lasso)


#Resumo da seleção:
#Sugere modelo com 8 variáveis (todas)
#Usar modelo COMPLETO (8 variáveis)
#Convergência de critérios


# ETAPA 3: Ajuste final, diagnostico e testes

modelo_final <- lm(Resistencia ~ Cimento + Escoria + CinzaVolante +
                     Agua + log1p_Superplast + AgregadoGraudo +
                     AgregadoMiudo + log_Idade,
                   data = dados_treino)

print(summary(modelo_final))

modelo_final_2 <- lm(Resistencia ~ Cimento + Escoria + CinzaVolante +
                     Agua + log1p_Superplast + AgregadoGraudo +
                     AgregadoMiudo + Idade_quad,
                   data = dados_treino)

bp_test <- bptest(modelo_final_2)
bp_test

# Diagnóstico gráfico (informal)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modelo_final, main.title = "Diagnóstico de Resíduos — Modelo Final")

#Residuos vs Ajustados: afunilamento observado
#Q-Q plot: Os pontos acompanham bem a linha com certo desvios nas caudas

# Agora, os testes formais:
ad_test <- ad.test(residuals(modelo_final))
ad_test


# Teste de Homocedasticidade: Breusch-Pagan
bp_test <- bptest(modelo_final)
bp_test


# Teste de Independência: Durbin-Watson
dw_test <- dwtest(modelo_final)
dw_test

# Multicolinearidade: Variance Inflation Factor (VIF)
vif_values <- vif(modelo_final)
vif_values
