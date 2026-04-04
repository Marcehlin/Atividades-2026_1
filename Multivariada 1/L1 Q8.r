caminhada <- c(120,90,100,110,80,105,95,NA,85,115)
bicicleta <- c(45,35,30,NA,25,38,33,40,28,50)
academia <- c(60,50,45,55,40,NA,48,58,42,150)

dados <- cbind(caminhada,bicicleta,academia)

dados <- data.frame(caminhada, bicicleta, academia)

cor(dados, use = "pairwise.complete.obs")

plot(dados)

#b)
dados_imputados <- dados
for (i in 1:ncol(dados)) {
  media <- mean(dados[, i], na.rm = TRUE)
  dados_imputados[, i][is.na(dados[, i])] <- media
}

cor(dados_imputados)
plot(dados_imputados)
boxplot(dados)

dados <- dados[1:9,]
dados

boxplot(dados)
cor(dados, use = "pairwise.complete.obs")

plot(dados)
