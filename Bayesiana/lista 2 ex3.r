p<-seq(0,1,by=0.001)
fun <- 1 / sqrt(p*(1-p))
plot(p, fun, type = "l")
