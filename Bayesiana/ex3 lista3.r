#Suponha que θ tenha uma distribui¸c˜ao a posteriori Beta(a, b). 
#Fixe valores de θ e fa¸ca o gr´afico das fun¸c˜oes de perda abaixo 
#para analisar seu comportamento e encontre o estimador de Bayes para θ com as seguintes fun¸c˜oes de perda:

#1
theta <- 0.5
delta<-seq(0,1,by=0.001)
L1 <- (theta^(-1)) * ((delta-theta)^(2))
plot(delta,L1,type = "l")


#2
theta <- 0.2
delta<-seq(0,1,by=0.001)
L2 <- theta^(-1) *(1-theta)^(-1) *(delta-theta)^(2)
plot(delta,L2,type = "l")
#3
b <- 0.2
theta <- 0.5
delta<-seq(0,1,by=0.001)
L3 <- as.numeric(abs(delta - theta) > b)
plot(delta,L3,type = "l")
