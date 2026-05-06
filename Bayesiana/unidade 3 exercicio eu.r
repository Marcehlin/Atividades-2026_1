set.seed(123)
lambda <- 5
n <- 100

amostra <- rexp(n,lambda)
amostra

a <- 1
b <- a/5

media_a_posteriori <- (n+a)/(sum(amostra)+b)
media_a_posteriori