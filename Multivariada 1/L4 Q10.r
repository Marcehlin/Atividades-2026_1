data(airquality)

dados <- airquality

dados <- dados[,c("Wind","Temp")]

#a

par(mfrow = c(1,2))

hist(dados$Wind, col = "lightpink")
hist(dados$Temp, col = "lightblue")

par(mfrow = c(1, 1))


par(mfrow = c(1, 2))

qqnorm(dados$Wind)
qqline(dados$Wind, col = "lightpink")

qqnorm(dados$Temp)
qqline(dados$Temp, col = "lightblue")

par(mfrow = c(1, 1))


#b
shapiro.test(dados$Wind)
shapiro.test(dados$Temp)

#c
pairs(dados)

#d
media <- colMeans(dados)

library(ICSNP)
HotellingsT2(dados, mu = media)

#f
plot(dados$Wind,
     dados$Temp,
     pch=19,
     xlab="Wind",
     ylab="Temp",
     main="Dispersão Wind x Temp")

library(car)

ellipse(center = colMeans(dados),
        shape = cov(dados),
        radius = sqrt(qchisq(.95,2)),
        draw = TRUE)  