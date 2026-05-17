library(readxl)
library(GGally)      
library(car)         
library(lmtest)      
library(nortest)     
library(MASS)        
library(leaps)       
library(glmnet)  
library(corrplot)


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


# ETAPA 1 resumida (inicialmente sem transformações) 

str(dados_treino)

#Dados faltantes por coluna:
colSums(is.na(dados_treino))

summary(dados_treino)

#################################################################
# ETAPA 2: Seleção de variaveis (DADOS sem tranformação inicialmente)

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

################################################################################
# antes de partir pra testes, vamos checar como seria o diagnostico escolhendo 8 covariaveis.
# Alem disso, antes de partir para os testes, será preciso transformar para se obter o 
#pressuposto de homocedasticidade.


# Modelo com 8 variaveis:
modelo_comparação <- lm(Resistencia ~ Cimento + Escoria + CinzaVolante +
                        Agua + Superplastificante + AgregadoGraudo +
                        AgregadoMiudo + Idade,
                      data = dados_treino)

summary(modelo_comparação)


par(mfrow = c(2, 2))
plot(modelo_comparação)
par(mfrow = c(1, 1))

ad_test <- ad.test(residuals(modelo_comparação))
ad_test
# aceita mas com p valor menor

bp_test <- bptest(modelo_comparação)
bp_test
#rejeita igualmente homocedasticidade

dw_test <- dwtest(modelo_comparação)
dw_test
# aceita independencia

vif_values <- vif(modelo_comparação)

round(vif_values, 3)
# VIF's se tornam consideravelmente mais altos



################################################################################
#Agora, trabalharemos com um modelo transformado para o atendimento dos pressupostos.

dados_treino_t <- dados_treino
dados_treino_t$Idade_log <- log(dados_treino_t$Idade)
dados_treino_t$Agua_Cimento <- dados_treino_t$Agua / dados_treino_t$Cimento
dados_treino_t$Cimento_Idade <- dados_treino_t$Cimento * dados_treino_t$Idade_log
dados_treino_t$Idade_log2 <- dados_treino_t$Idade_log^2
dados_treino_t$Y_sqrt <- sqrt(dados_treino_t$Resistencia)

#Etapa 2: Seleção de variaveis, mas agora com dados transformados

modelo_completo_t <- lm(Y_sqrt ~ Cimento + Escoria + CinzaVolante +
                      Agua_Cimento + Superplastificante + AgregadoGraudo +
                      AgregadoMiudo + Idade_log + Idade_log2 + Cimento_Idade,
                    data = dados_treino_t)

# Seleção por Cp de Mallows
reg_sub_t <- regsubsets(Y_sqrt ~ Cimento + Escoria + CinzaVolante +
                          Agua_Cimento + Superplastificante + AgregadoGraudo +
                          AgregadoMiudo + Idade_log + Idade_log2 + Cimento_Idade,
                        data = dados_treino_t, nbest = 1, nvmax = 10)

resumo_sub_t <- summary(reg_sub_t)

tabela_mallows_t <- data.frame(
  "n_vars" = 1:10,
  "Cp" = round(resumo_sub_t$cp, 3),
  "R2" = round(resumo_sub_t$rsq, 4),
  "R2_adj" = round(resumo_sub_t$adjr2, 4),
  "BIC" = round(resumo_sub_t$bic, 2)
)

tabela_mallows_t


# Gráfico Cp
par(mar = c(4, 4, 3, 1))
plot(1:10, resumo_sub_t$cp, type = "b", pch = 16, cex = 1.2,
     xlab = "Número de variáveis", ylab = "Cp de Mallows",
     main = "Seleção Cp (sqrt(Y) + transformações)", col = "steelblue", lwd = 2)
abline(a = 1, b = 1, col = "red", lty = 2, lwd = 2)
legend("topright", c("Cp observado", "Reta ideal (Cp = p+1)"),
       col = c("steelblue", "red"), lty = c(1, 2), pch = c(16, NA))
par(mar = c(5, 4, 4, 2))


# Seleção por AIC
modelo_aic_t <- stepAIC(modelo_completo_t, direction = "both", trace = FALSE)
formula(modelo_aic_t)

# Seleção por BIC
modelo_bic_t <- stepAIC(modelo_completo_t, direction = "both",
                        k = log(nrow(dados_treino_t)), trace = FALSE)
formula(modelo_bic_t)

# Seleção por Lasso
X_mat_t <- model.matrix(Y_sqrt ~ Cimento + Escoria + CinzaVolante +
                          Agua_Cimento + Superplastificante + AgregadoGraudo +
                          AgregadoMiudo + Idade_log + Idade_log2 + Cimento_Idade,
                        data = dados_treino_t)[, -1]

y_vec_t <- dados_treino_t$Y_sqrt
set.seed(42)
cv_lasso_t <- cv.glmnet(X_mat_t, y_vec_t, alpha = 1)

coef_lasso_t <- coef(cv_lasso_t, s = "lambda.min")
vars_lasso_t <- names(which(coef_lasso_t[-1, 1] != 0))

vars_lasso_t



bp_4_poly <- bptest(modelo_completo_t)
ad_4_poly <- ad.test(residuals(modelo_completo_t))
shapiro_4_poly <- shapiro.test(residuals(modelo_completo_t))
bp_4_poly
ad_4_poly
shapiro_4_poly


dw_test <- dwtest(modelo_completo_t)
dw_test
# aceita independencia

vif_values <- vif(modelo_completo_t)
vif_values
