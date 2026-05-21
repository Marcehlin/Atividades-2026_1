library(ggplot2)
library(car)
library(nortest)

dados <- data.frame(
  A   = rep(c(-1, 1,-1, 1,-1, 1,-1, 1), each = 3),
  B   = rep(c(-1,-1, 1, 1,-1,-1, 1, 1), each = 3),
  C   = rep(c(-1,-1,-1,-1, 1, 1, 1, 1), each = 3),
  trat= rep(c("(1)","a","b","ab","c","ac","bc","abc"), each = 3),
  rep = rep(c("I","II","III"), 8),
  y   = c(22, 31, 25,   # (1)
          32, 43, 29,   # a
          35, 34, 50,   # b
          55, 47, 46,   # ab
          44, 45, 38,   # c
          40, 37, 36,   # ac
          60, 50, 54,   # bc
          39, 41, 47)   # abc
)

str(dados)

# transformar fatores
dados$Af <- factor(dados$A, labels = c("Baixa", "Alta"))
dados$Bf <- factor(dados$B, labels = c("Baixa", "Alta"))
dados$Cf <- factor(dados$C, labels = c("Baixo", "Alto"))

N <- nrow(dados)

#Estatisticas descritivas:
#Media geral:
round(mean(dados$y), 2)
#Desvio padrao:
round(sd(dados$y), 2)
# Min:
min(dados$y)
#Max:
max(dados$y)

#Medias por tratamento:
round(tapply(dados$y, dados$trat, mean), 2)

#Medias por fator A:
round(tapply(dados$y, dados$Af, mean), 2)
#Medias por fator B:
round(tapply(dados$y, dados$Bf, mean), 2)
#Medias por fator C:
round(tapply(dados$y, dados$Cf, mean), 2)

#Olhando as médias por fator:
#Fator A: Baixa = 40,67h vs Alta = 41,00h -> diferença de apenas 0,33h, praticamente nada
#Fator B: Baixa = 35,17h vs Alta = 46,50h->diferença de 11,33h, bem expressiva
#Fator C: Baixo = 37,42h vs Alto = 44,25h -> diferença de 6,83h, moderada
#Já dá pra intuir que B vai ser significativo e A provavelmente não.

# ANOVA Fatorial 2^3 
modelo <- aov(y ~ Af * Bf * Cf, data = dados)
summary(modelo)
#A velocidade de corte (A) sozinha não tem efeito. A geometria da ferramenta (B) 
#é o fator mais importante. O ângulo (C) também importa, mas seu efeito depende 
#do nível de A — por isso a interação AC é significativa.

# pressupostos
res <- residuals(modelo)

# Normalidade
sw  <- shapiro.test(res)
sw

ad  <- ad.test(res)
ad

# Homocedasticidade
dados$grupo <- interaction(dados$Af, dados$Bf, dados$Cf)
lev <- leveneTest(y ~ grupo, data = dados)
lev

# Graficos de diagnostico de residuos
par(mfrow = c(2, 2))
plot(modelo, main = "")
mtext("Diagnostico de Residuos - Modelo Fatorial 2^3",
      side = 3, line = -2, outer = TRUE, cex = 1.1, font = 2)

# Graficos de efeitos principais
par(mfrow = c(1, 3))

# Fator A
medA <- tapply(dados$y, dados$Af, mean)
plot(1:2, medA, type = "b", pch = 19, col = "steelblue", lwd = 2,
     xaxt = "n", xlab = "Velocidade de Corte (A)",
     ylab = "Vida Util Media (h)", ylim = c(30, 55),
     main = "Efeito Principal A")
axis(1, at = 1:2, labels = c("Baixa (-)", "Alta (+)"))
grid()
#Fator A: linha quase horizontal (40,67 vs 41,00h) ->velocidade de corte não altera
#a vida útil quando analisada isoladamente. Confirma p = 0,88 na ANOVA.

# Fator B
medB <- tapply(dados$y, dados$Bf, mean)
plot(1:2, medB, type = "b", pch = 19, col = "tomato", lwd = 2,
     xaxt = "n", xlab = "Geometria da Ferramenta (B)",
     ylab = "Vida Util Media (h)", ylim = c(30, 55),
     main = "Efeito Principal B")
axis(1, at = 1:2, labels = c("Baixa (-)", "Alta (+)"))
grid()
#Fator B: linha com inclinação acentuada subindo de 35,17 para 46,50h -> geometria 
#da ferramenta no nível alto aumenta significativamente a vida útil. O maior efeito 
#isolado do experimento.

# Fator C
medC <- tapply(dados$y, dados$Cf, mean)
plot(1:2, medC, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     xaxt = "n", xlab = "Angulo de Corte (C)",
     ylab = "Vida Util Media (h)", ylim = c(30, 55),
     main = "Efeito Principal C")
axis(1, at = 1:2, labels = c("Baixo (-)", "Alto (+)"))
grid()
#Fator C: inclinação moderada de 37,42 para 44,25h -> ângulo alto tende a aumentar
#a vida útil, mas esse efeito precisa ser interpretado junto com A por causa da interação AC.


# Grafico de interacao AC (significativa)
par(mfrow = c(1, 2))

# AC
medAC <- tapply(dados$y, list(dados$Af, dados$Cf), mean)
plot(1:2, medAC["Baixa",], type="b", pch=19, col="steelblue", lwd=2,
     xaxt="n", ylim=c(30,60),
     xlab="Angulo de Corte (C)", ylab="Vida Util Media (h)",
     main="Interacao AC")
lines(1:2, medAC["Alta",], type="b", pch=17, col="tomato", lwd=2, lty=2)
axis(1, at=1:2, labels=c("Baixo (-)","Alto (+)"))
legend("topright", legend=c("A Baixa (-)","A Alta (+)"),
       col=c("steelblue","tomato"), lty=c(1,2), pch=c(19,17))
grid()
#Interação AC (significativa): as linhas se cruzam, esse é o sinal  de interação forte.
# Com A baixo (velocidade baixa), aumentar o ângulo C de - para + aumenta a vida útil. 
#Com A alto (velocidade alta), aumentar o ângulo C de − para + reduz a vida útil. 
#Isso explica por que A sozinho parece sem efeito, em metade das situações ele ajuda e 
#na outra metade prejudica, e os efeitos se cancelam na média.


# BC
medBC <- tapply(dados$y, list(dados$Bf, dados$Cf), mean)
plot(1:2, medBC["Baixa",], type="b", pch=19, col="steelblue", lwd=2,
     xaxt="n", ylim=c(30,60),
     xlab="Angulo de Corte (C)", ylab="Vida Util Media (h)",
     main="Interacao BC")
lines(1:2, medBC["Alta",], type="b", pch=17, col="tomato", lwd=2, lty=2)
axis(1, at=1:2, labels=c("Baixo (-)","Alto (+)"))
legend("topright", legend=c("B Baixa (-)","B Alta (+)"),
       col=c("steelblue","tomato"), lty=c(1,2), pch=c(19,17))
grid()
#Interação BC (não significativa): as linhas são aproximadamente paralelas -> B e C 
#agem de forma independente, sem interação relevante. Confirma p = 0,22 na ANOVA.

library(emmeans)
library(ggplot2)

dados$trat <- factor(dados$trat)

modelo_trat <- aov(y ~ trat, data = dados)

tukey <- pairs(
  emmeans(modelo_trat, ~ trat),
  adjust = "tukey"
)


library(emmeans)   # se não tiver instalado: install.packages("emmeans")

# Modelo já ajustado: modelo <- aov(y ~ Af * Bf * Cf, data = dados)

# 1. Efeito principal de B (com ajuste de Bonferroni para 3 contrastes)
emm_B <- emmeans(modelo, ~ Bf)
contr_B <- pairs(emm_B, adjust = "bonferroni")
summary(contr_B, infer = c(TRUE, TRUE), level = 0.95)

# 2. Desdobramento da interação AC: efeito de A dentro de cada nível de C
emm_A_C <- emmeans(modelo, ~ Af | Cf)
contr_AC <- pairs(emm_A_C, adjust = "bonferroni")

summary(contr_AC, infer = c(TRUE, TRUE), level = 0.95)


em <- emmeans(modelo, ~ Af | Cf)
pairs(em, adjust = "bonferroni") 
