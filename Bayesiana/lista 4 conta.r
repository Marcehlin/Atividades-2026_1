#2.2
qgamma(0.025,440.5,8)
qgamma(0.975,440.5,8)

qgamma(0.025,388.5,4)
qgamma(0.975,388.5,4)

440.5/8*2 - 388.5/4

4*440.5/64 +388.5/16

388.5/16

sqrt(4*440.5/64 +388.5/16) * 1.96 - 13

#2.3
# Tamanho da amostra posterior
N <- 100000

# Simular das posteriores
lambda_post <- rgamma(N, shape = 440.5, rate = 8)
mu_post     <- rgamma(N, shape = 388.5, rate = 4)


# Novo parâmetro: delta = 2 lambda - mu
delta <- 2*lambda_post - mu_post


# Histograma
hist(delta,
     breaks = 50,
     main = expression("Distribuição posterior de " * (2*lambda-mu)),
     xlab = expression(delta == 2*lambda-mu))


# Intervalo de credibilidade 95%
IC_delta <- quantile(delta, c(0.025, 0.975))

cat("Intervalo de credibilidade 95% para 2λ - μ:\n")
print(IC_delta)


# Probabilidade de 2λ > μ
cat("P(2λ > μ | Y) = ",
    mean(delta > 0), "\n")


#3.1

soma <- exp(-.5) + exp(-1) + exp(-1.5) + exp(-2) + exp(-2.5)

p1 <- exp(-.5) / soma 
p2 <- exp(-1) / soma 
p3 <- exp(-1.5) / soma 
p4 <- exp(-2) / soma 
p5 <- exp(-2.5) / soma 


#4

4/0.36

dados <- c(2, 3, 0, 0, 1, 0, 2, 0, 3, 0, 1, 2)
sum(dados)
length(dados)

qgamma(0.025,25.11,17.55)
qgamma(0.975,25.11,17.55)

qgamma(c(0.025,0.975),
       shape=25.111,
       rate=17.556)

library(HDInterval)

intervalos <- hdi(qgamma(seq(0,1,length=10000),
           shape=25.111,
           rate=17.556),
    credMass=0.95)       

(25.11-1)/17.55
qgamma(0.5,25.11,17.55)
moda <- (alfa - 1) / beta

exp(-qgamma(0.975,25.11,17.55))
exp(-qgamma(0.025,25.11,17.55))

exp(-intervalos[2])
exp(-intervalos[1])

