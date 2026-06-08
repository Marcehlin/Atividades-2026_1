library(MASS) # Para mvrnorm
library(ggplot2) # Para visualização
library(dplyr) # Para manipulação de dados

# Parâmetros populacionais
mu <- c(1, 2)
Sigma<- matrix(c(1, 0.8, 0.8, 2), nrow = 2)

# Tamanhos amostrais
n_values <- c(5, 10, 50, 200)
n_sim <- 1000

# Gerar as médias amostrais

resultados <- lapply(n_values , function(n) {
    medias <- replicate(n_sim, {
        amostra <- mvrnorm(n, mu = mu , Sigma = Sigma), colMeans(amostra) 
        })
        df <- as.data.frame(t(medias))
        df$n <- as.factor(n)
        names(df)[1:2]<- c("X1", "X2")
        return(df)
        }) %>% bind_rows ()
