data(iris)

dados <- subset(iris, Species == "setosa")

# Neste exercício, utilizaremos apenas as 50 observações da espécie setosa e três variáveis: tamanho da sépala, largura da sépala e tamanho da pétala

dados <- dados[,c("Sepal.Length","Sepal.Width","Petal.Length")]

# item a
par(mfrow = c(1, 3))

hist(dados$Sepal.Length,
     main = "Tamanho da Sépala",
     xlab = "Valor",
     col = "lightblue")

hist(dados$Sepal.Width,
     main = "Largura da Sépala",
     xlab = "Valor",
     col = "lightgreen")

hist(dados$Petal.Length,
     main = "Tamanho da Pétala",
     xlab = "Valor",
     col = "lightpink")

par(mfrow = c(1, 1))

par(mfrow = c(1, 3))

qqnorm(dados$Sepal.Length)
qqline(dados$Sepal.Length)

qqnorm(dados$Sepal.Width)
qqline(dados$Sepal.Width)

qqnorm(dados$Petal.Length)
qqline(dados$Petal.Length)

par(mfrow = c(1, 1))

# item b

shapiro.test(dados$Sepal.Length)
shapiro.test(dados$Sepal.Width)
shapiro.test(dados$Petal.Length)


library(ICSNP)
HotellingsT2(dados, mu = c(5, 3.4, 1.5))

qf(0.95, df1 = 2,, df2 = 43)
44*2/43*3.21
qf(0.95, df1 = 3,, df2 = 47)
3*49/47 * 2.80

colMeans(dados)
