library(dplyr)
library(ggplot2)
library(caret)

set.seed(123) #para o treino e teste

dados <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", stringsAsFactors = FALSE)
head(dados)

dim(dados)#linha/coluna
str(dados)#tipos de variaveis

dados$Churn <- ifelse(dados$Churn == "Yes", 1, 0)

table(dados$Churn)
prop.table(table(dados$Churn))

dados$customerID <- NULL

sum(is.na(dados$TotalCharges))

dados[is.na(dados$TotalCharges), c("tenure", "MonthlyCharges", "TotalCharges")]

dados$TotalCharges[is.na(dados$TotalCharges)] <- 0

sum(is.na(dados))

dados <- dados %>% mutate(across(where(is.character), as.factor))

dados$SeniorCitizen <- factor(dados$SeniorCitizen, labels = c("No", "Yes"))

str(dados)



ggplot(dados, aes(x = factor(Churn, labels = c("Nao cancelou", "Cancelou")))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuicao da variavel resposta (Churn)",
       x = "", y = "Numero de clientes")

summary(dados[, c("tenure", "MonthlyCharges", "TotalCharges")])


ggplot(dados, aes(x = factor(Churn, labels = c("Nao cancelou", "Cancelou")),
                  y = tenure)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Tempo de contrato (meses) por status de churn",
       x = "", y = "Tenure (meses)")


ggplot(dados, aes(x = factor(Churn, labels = c("Nao cancelou", "Cancelou")),
                  y = MonthlyCharges)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Mensalidade por status de churn",
       x = "", y = "Mensalidade (MonthlyCharges)")



prop.table(table(dados$Contract, dados$Churn), margin = 1)

ggplot(dados, aes(x = Contract, fill = factor(Churn, labels = c("Nao cancelou", "Cancelou")))) +
  geom_bar(position = "fill") +
  labs(title = "Proporcao de churn por tipo de contrato",
       x = "Tipo de contrato", y = "Proporcao", fill = "Churn")


ggplot(dados, aes(x = PaymentMethod, fill = factor(Churn, labels = c("Nao cancelou", "Cancelou")))) +
  geom_bar(position = "fill") +
  labs(title = "Proporcao de churn por metodo de pagamento",
       x = "Metodo de pagamento", y = "Proporcao", fill = "Churn") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))




indices_treino <- createDataPartition(dados$Churn, p = 0.7, list = FALSE)

treino <- dados[indices_treino, ]
teste  <- dados[-indices_treino, ]

nrow(treino)
nrow(teste)


modelo_completo <- glm(Churn ~ ., data = treino, family = binomial(link = "logit"))
modelo_completo <- update(modelo_completo, . ~ . - TotalCharges)

summary(modelo_completo)

library(MASS)

modelo_final <- stepAIC(modelo_completo, direction = "both", trace = FALSE)

summary(modelo_final)

par(mfrow = c(2, 2)) 
plot(modelo_final)
par(mfrow = c(1, 1))

# Razoes de chance (odds ratios) e intervalos de confianca de 95%
odds_ratios <- exp(cbind(OR = coef(modelo_final), confint(modelo_final)))

round(odds_ratios, 3)

# Teste da razao de verossimilhancas entre o modelo completo e o modelo final (reduzido)
anova(modelo_final, modelo_completo, test = "Chisq")

# Comparacao do AIC dos dois modelos
AIC(modelo_completo, modelo_final)