sim_direta <- function(n) {
  u <- runif(n)
  x1 <- numeric(n)
  x2 <- numeric(n)
  
  for (i in 1:n) {
    if (u[i] < 0.5) {
      x1[i] <- 0; x2[i] <- 0
    } else if (u[i] < 0.7) {
      x1[i] <- 0; x2[i] <- 1
    } else if (u[i] < 0.95) {
      x1[i] <- 1; x2[i] <- 0
    } else {
      x1[i] <- 1; x2[i] <- 1
    }
  }
  return(data.frame(X1 = x1, X2 = x2))
}

amostra_1 <- sim_direta(1000)

sim_fatoracao <- function(n) {
  u1 <- runif(n)
  u2 <- runif(n)
  
  x1 <- ifelse(u1 < 0.7, 0, 1)
  
  x2 <- numeric(n)
  for (i in 1:n) {
    if (x1[i] == 0) {
      x2[i] <- ifelse(u2[i] < 5/7, 0, 1)
    } else {
      x2[i] <- ifelse(u2[i] < 5/6, 0, 1)
    }
  }
  return(data.frame(X1 = x1, X2 = x2))
}

amostra_2 <- sim_fatoracao(1000)

sim_gibbs <- function(n, burnin = 1000) {
  total <- n + burnin
  x1 <- numeric(total)
  x2 <- numeric(total)
  
  # Inicialização
  x1[1] <- 0
  
  for (t in 2:total) {
    # Amostrar X2 | X1
    if (x1[t-1] == 0) {
      x2[t] <- ifelse(runif(1) < 5/7, 0, 1)
    } else {
      x2[t] <- ifelse(runif(1) < 5/6, 0, 1)
    }
    
    # Amostrar X1 | X2
    if (x2[t] == 0) {
      x1[t] <- ifelse(runif(1) < 2/3, 0, 1)
    } else {
      x1[t] <- ifelse(runif(1) < 4/5, 0, 1)
    }
  }
  
  # Descartar burn-in
  return(data.frame(X1 = x1[(burnin+1):total], 
                    X2 = x2[(burnin+1):total]))
}

amostra_3 <- sim_gibbs(1000)
