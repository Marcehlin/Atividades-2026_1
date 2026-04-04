library(heplots)
data(package = "heplots")
data("schooldata")

schooldata
notas <- subset(schooldata)[,6:8]

notas[11,]

matplot(t(notas), 
        type = "o", 
        pch = 16, 
        col = rainbow(46), 
        xaxt = "n", 
        xlab = "Disciplinas", 
        ylab = "Notas", 
        main = "")

axis(1, at = 1:3, labels = c("Readings", "Math", "Selfesteem"))

cor(notas)
plot(notas)
