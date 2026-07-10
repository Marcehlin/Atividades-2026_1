library(dplyr)
library(ggplot2)
library(caret)

set.seed(123) #para o treino e teste


#Descrição do problema e dos objetivos da análise:
# O problema deste trabalho e o de "churn" de clientes: uma empresa
# de telecomunicacoes quer entender quais fatores estao associados
# ao cancelamento do servico por parte de um cliente, e construir um
# modelo capaz de estimar a probabilidade de um cliente cancelar.
#
# Objetivo da analise:
# - Identificar quais caracteristicas do cliente e do contrato estao
#   associadas a maior ou menor chance de cancelamento (churn).
# - Construir um modelo de regressao logistica que, a partir dessas
#   caracteristicas, estime a probabilidade de churn de um cliente.
# - Avaliar a qualidade preditiva desse modelo em dados que ele nao
#   viu durante o ajuste (amostra de teste).
#
# Motivacao pratica: prever quais clientes tem maior risco de
# cancelar permite que a empresa direcione acoes de retencao
# (descontos, contato proativo e etc) para esses clientes.



# Descrição do conjunto e dos dados, identificando a var. resp., as covariaveis...
dados <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", stringsAsFactors = FALSE)
head(dados)

dim(dados)#linha/coluna
str(dados)#tipos de variaveis

# Variavel resposta: Churn. Indica se o cliente cancelou o
# servico ("Yes") ou nao ("No").  precisamos transformar em uma
# variavel binaria numerica.
dados$Churn <- ifelse(dados$Churn == "Yes", 1, 0)

# para ver o balanceamento das classes, ou seja, quantos clientes
# cancelaram e quantos nao cancelaram.
table(dados$Churn)
prop.table(table(dados$Churn))

# couna apenas de identificação, sem valor para analise
dados$customerID <- NULL

# Covariaveis utilizadas na analise:
# - gender: sexo do cliente (Male/Female)
# - SeniorCitizen: se o cliente é idoso (0/1)
# - Partner: se o cliente tem conjuge (Yes/No)
# - Dependents: se o cliente tem dependentes (Yes/No)
# - tenure: numero de meses que o cliente permanece na empresa
# - PhoneService, MultipleLines, InternetService, OnlineSecurity,
#   OnlineBackup, DeviceProtection, TechSupport, StreamingTV,
#   StreamingMovies: servicos contratados pelo cliente
# - Contract: tipo de contrato (mensal, um ano, dois anos)
# - PaperlessBilling: se a fatura e digital(Yes/no)
# - PaymentMethod: forma de pagamento
# - MonthlyCharges: valor cobrado mensalmente
# - TotalCharges: valor total ja cobrado do cliente


# quantas linhas ficaram com valor faltante (NA) em totalcharges
sum(is.na(dados$TotalCharges))

# Todas elas correspondem a
#clientes com tenure = 0, ou seja, clientes que acabaram de entrar
# e ainda nao completaram um mes de cobranca. Faz sentido que o
# total cobrado seja "vazio" nesse casoe nao um erro no banco.
dados[is.na(dados$TotalCharges), c("tenure", "MonthlyCharges", "TotalCharges")]

# Em vez de remover essas linhas, escolhi  imputar o valor 0.
# O motivo e que, aqui, o valor faltante nao e "desconhecido", ele
# e logicamente implicado pela propria variavel tenure. Um cliente
# com zero meses de casa ainda nao foi cobrado, entao o total
# cobrado dele ate o momento e necessariamente zero. 
dados$TotalCharges[is.na(dados$TotalCharges)] <- 0

# nenhum outro NA no banco:
sum(is.na(dados))

# transfnormando as variaveis de texto (character) em fator, que é
# o formato que o R usa para representar variaveis categoricas.
dados <- dados %>% mutate(across(where(is.character), as.factor))

# SeniorCitizen esta guardada como 0/1 numerico, mas na pratica é
# uma variavel categorica (idoso ou nao)
dados$SeniorCitizen <- factor(dados$SeniorCitizen, labels = c("No", "Yes"))

#resultado final da estrutura do banco tratado:
str(dados)


# Exploratória:

# grafico de barras mostrando a proporcao de clientes que
# cancelaram (1)e que nao cancelaram (0) o servico.
ggplot(dados, aes(x = factor(Churn, labels = c("Nao cancelou", "Cancelou")))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuicao da variavel resposta (Churn)",
       x = "", y = "Numero de clientes")


# Resumo estatistico (minimo, maximo, media, quartis) das tres
# variaveis numericas do banco.
summary(dados[, c("tenure", "MonthlyCharges", "TotalCharges")])

# Boxplot comparando a distribuicao do tempo de contrato (tenure)
# entre quem cancelou e quem nao cancelou. O que faz sentido éque
# clientes mais antigos cancelem menos.
ggplot(dados, aes(x = factor(Churn, labels = c("Nao cancelou", "Cancelou")),
                  y = tenure)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Tempo de contrato (meses) por status de churn",
       x = "", y = "Tenure (meses)")

# Boxplot comparando o valor da mensalidade entre quem cancelou e
# quem nao cancelou. Indica que as mensalidades mais altas
# estejam associadas a maior chance de cancelamento.
ggplot(dados, aes(x = factor(Churn, labels = c("Nao cancelou", "Cancelou")),
                  y = MonthlyCharges)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Mensalidade por status de churn",
       x = "", y = "Mensalidade (MonthlyCharges)")


# Tabela cruzada entre tipo de contrato e churn, em proporcao por
# linha. Isso mostra, dentro de cada tipo de contrato, qual fracao
# dos clientes cancelou.
prop.table(table(dados$Contract, dados$Churn), margin = 1)

# O mesmo grafico em forma visual
ggplot(dados, aes(x = Contract, fill = factor(Churn, labels = c("Nao cancelou", "Cancelou")))) +
  geom_bar(position = "fill") +
  labs(title = "Proporcao de churn por tipo de contrato",
       x = "Tipo de contrato", y = "Proporcao", fill = "Churn")

# Mesma ideia para o metodo de pagamento, que tambem costuma ser
# bastante associado ao cancelamento 
ggplot(dados, aes(x = PaymentMethod, fill = factor(Churn, labels = c("Nao cancelou", "Cancelou")))) +
  geom_bar(position = "fill") +
  labs(title = "Proporcao de churn por metodo de pagamento",
       x = "Metodo de pagamento", y = "Proporcao", fill = "Churn") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#pode-se reprtir a mesma lógicapara as outras covariaveis porém ficaria muito extenso
# e desnecessário

# Divisão da amostra:
# Aqui fiz uma divisao estratificada (e nao uma amostragem aleatoria
# simples) pois a variavel resposta é desbalanceada ( 73% "Nao cancelou" e 
# 27% "Cancelou", como visto no item 2). A
# funcao createDataPartition, serve para que essa
# mesma proporcao de churn seja mantida tanto na amostra de
# treino quanto na de teste. Sem isso, uma divisao
# aleatoria simples poderia concentrar mais casos de
# churn em um dos dois conjuntos.

# Proporcao adotada: 70% para treino e 30% para teste. 
indices_treino <- createDataPartition(dados$Churn, p = 0.7, list = FALSE)

treino <- dados[indices_treino, ]
teste  <- dados[-indices_treino, ]

# Tamanho de cada amostra.
nrow(treino)
nrow(teste)


# Por agora em diante só  "treino" e "teste" sao os bancos que usaremos
# no resto do trabalho, o ajuste do modelo usa o "treino", e a avaliacao com curva ROC e
# matriz de confusao usa o "teste".
