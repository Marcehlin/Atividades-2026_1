library(readxl)
dados_brutos <- read_excel("C:/Users/Marcelo/Documents/Atividades 2026_1/Regressao/Regressao Trabalho/Concrete_Data.xls")
colnames(dados_brutos) <- c("Cimento","Escoria","CinzaVolante","Agua",
                            "Superplastificante","AgregadoGraudo",
                            "AgregadoMiudo","Idade","Resistencia")

set.seed(42)
n_total <- nrow(dados_brutos)
n_treino <- round(0.70 * n_total)

idx_treino <- sample(1:n_total, size = n_treino)
dados_treino <- dados_brutos[idx_treino, ]
dados_validacao <- dados_brutos[-idx_treino, ]      
head(dados_treino)

library(caret)
set.seed(42)
ctrl_kfold <- trainControl(method = "cv", number = 5, savePredictions = "final")

modelo_kfold <- train(
  Resistencia ~ Cimento + Escoria + CinzaVolante + Agua + Superplastificante + Idade,
  data      = dados_brutos,
  method    = "lm",
  trControl = ctrl_kfold
)

print(modelo_kfold)
# Extraia o RMSE médio da CV
rmse_cv <- modelo_kfold$results$RMSE
cat("RMSE médio (5-fold CV):", round(rmse_cv, 4), "\n")
