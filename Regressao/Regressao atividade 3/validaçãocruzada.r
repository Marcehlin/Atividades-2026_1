#x1 = anos de experiencia
X1 <- c(5, 11, 9, 8, 3, 3, 2, 11, 17, 18,
        16, 11, 3, 3, 3, 4, 7, 6, 4, 8,
        3, 4, 5, 6)

#x2 = anos de escolaridade
X2 <- c(16, 10, 13, 14, 8, 14, 10, 9, 17, 18,
        16, 11, 15, 12, 9, 13, 8, 17, 11, 15,
        11, 13, 13, 10)

#x3 = setor
X3 <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0,
        0, 0, 0, 0, 1, 0, 1, 0, 1, 1,
        0, 0, 1, 1)

#x4 = idade
X4 <- c(38, 39, 22, 29, 23, 42, 36, 28, 22, 28,
        28, 38, 42, 32, 23, 38, 39, 34, 39, 32,
        33, 31, 21, 23)

#y = log(salario)
Y <- c(8.874, 8.970, 8.901, 9.139, 7.472, 8.388,
       7.836, 8.599, 9.330, 9.174, 8.470, 8.973,
       8.632, 8.149, 7.392, 8.509, 7.796, 9.060,
       8.156, 8.918, 7.997, 8.196, 8.416, 8.033)
#data frame
dados_brutos <- data.frame(Experiencia = X1, 
                    Escolaridade = X2, 
                    Setor = X3, 
                    Idade = X4, 
                    LogSalario = Y)

# semente para reprodutibilidade
set.seed(20260512)

# número de observações
n <- nrow(dados_brutos)

# proporção para treino (70% a 80% é usual)
prop_treino <- 0.7
n_treino <- round(prop_treino * n)

# índices de treino aleatórios
indices_treino <- sample(1:n, size = n_treino, replace = FALSE)
indices_teste <- setdiff(1:n, indices_treino)

dados_treino <- dados_brutos[indices_treino, ]
dados_teste  <- dados_brutos[indices_teste, ]

set.seed(20260513)
library(caret)
train_control <- trainControl(method = "cv", number = 5)
cv_model <- train(LogSalario ~ Experiencia + Escolaridade, 
                  data = dados_brutos,  # use os dados brutos originais (24 obs.)
                  method = "lm",
                  trControl = train_control)
print(cv_model)
# Extraia o RMSE médio da CV
rmse_cv <- cv_model$results$RMSE
cat("RMSE médio (5-fold CV):", round(rmse_cv, 4), "\n")
