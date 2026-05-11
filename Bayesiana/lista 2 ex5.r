lambda <- seq(0.01, 10, by = 0.0001)
y <- dgamma(lambda,4,1)
plot(lambda, y, type="l", col="red")
