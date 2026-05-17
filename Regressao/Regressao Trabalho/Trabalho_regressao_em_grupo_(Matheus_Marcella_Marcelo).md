# Trabalho/Atividade 5 em grupo de Regressão

## Integrantes do Grupo:
- Matheus Brito
- Marcella Cobra
- Marcelo Huang

---

Enunciado: "*Encontre um conjunto de dados com, no minimo, 8 covariaveis. Ajuste um modelo de regressão nos seus dados. Use todas as etapas de análise. Escreva os resultados em um Relatorio em PDF. Prepare uma apresentação do seu Relatorio.*"

---
## Conjunto de dados escolhido: 

Resistência à compressão do Concreto  vs. Idade + Componentes (disponível em [UC Irvine: Concrete Compressive Strength](https://archive.ics.uci.edu/dataset/165/concrete+compressive+strength))

## Etapa 0:  Contextualizando

Nesta atividade, será realizada uma análise de um conjunto de dados dito na sala de aula, usando um modelo de regressão linear $Y = X\beta + \varepsilon$.


A partir desses dados, serão conduzidos: Diagnóstico/Análise de resíduos; Detecção de outliers e pontos influentes; Testes estatísticas; e validação do modelo.

Estamos interessados em como os componentes e a idade impacta na resistência à compressão do concreto

O conjunto de dados original é composto por 8 covariáveis e 1 variável resposta:

* $X_1$: Cimento (quantitativa - kg/$m^3$)
* $X_2$: Escória (quantitativa - kg/$m^3$)
* $X_3$: Cinza Volante (quantitativa - kg/$m^3$)
* $X_4$: Água (quantitativa - kg/$m^3$)
* $X_5$: Superplastificante (quantitativa - kg/$m^3$)
* $X_6$: Agregado Graudo (quantitativa - kg/$m^3$)
* $X_7$: Agregado Miudo (quantitativa - kg/$m^3$)
* $X_8$: Idade = o tempo decorrido desde o momento em que a água entra em contato com o cimento (quantitativa - dia)
* $Y$: Resistência a compressão (quantitativa - MPa)


---
Para validação do modelo, o conjunto de dados (total de 1030 observações) foi dividido aleatóriamente com semente 42 para reprodutividade em duas partes: teste (309 observações), treino (721 observações)


*Nessa atividade, as análises serão realizadas no software **R**, utilizando funções da linguagem, nativas e/ou de pacotes externas*

## Etapa 1: 

---
*Pegar os dados brutos e começa a verificar inconveniências.*

Nesse contexto seria verificar a consistência entre as covariáveis.

* Corrigir
* Transformar 
* Remover

*Todas ações feitas devem ser documentadas e justificadas.*

---

#### 1.1 Carregando os dados:

##### Dados do Experimento do treino


| Cimento | Escoria | CinzaVolante | Agua | Superplastificante | AgregadoGraudo | AgregadoMiudo | Idade | Resistencia |
|----------|----------|---------------|-------|---------------------|-----------------|----------------|--------|--------------|
| 220.80 | 147.20 | 0.00  | 185.70 | 0.00  | 1055.00 | 744.30 | 28  | 25.745034 |
| 249.10 | 0.00  | 98.75 | 158.11 | 12.80 | 987.76  | 889.01 | 14  | 28.682202 |
| 275.00 | 0.00  | 0.00  | 183.00 | 0.00  | 1088.00 | 808.00 | 7   | 14.203206 |
| 237.50 | 237.50 | 0.00 | 228.00 | 0.00  | 932.00  | 594.00 | 7   | 26.258004 |
| 139.60 | 209.40 | 0.00 | 192.00 | 0.00  | 1047.00 | 806.90 | 180 | 44.207822 |
| 277.19 | 97.82 | 24.46 | 160.70 | 11.19 | 1061.70 | 782.46 | 14  | 47.711739 |
| ... | ... | ... | ... | ... |... |... |... |... |

(721 x 9)

---

#### 1.2 Verificando inconsistências:

Não aparenta ter inconsistências nos dados, uma vez que os componentes de um concreto são manipuláveis/controláveis.

---
#### 1.3 Análise gráfica para linearidade:

Este passo é um pouco desnecessário pois iriamos usar modelo de regressão linear de qualquer jeito.

![Gráfico de dispersão das covariaveis com a resposta](1_3_grafico_disp_covarivel_x_resposta.png)


---
#### 1.4 Análise descritiva das covariáveis 

| Variável | Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max. |
|-----------|------|----------|---------|------|----------|------|
| Cimento | 102.0 | 192.0 | 266.0 | 280.4 | 350.0 | 540.0 |
| Escoria | 0.00 | 0.00 | 42.08 | 78.34 | 145.00 | 359.40 |
| CinzaVolante | 0.00 | 0.00 | 0.00 | 52.71 | 118.27 | 200.00 |
| Agua | 121.8 | 164.9 | 185.7 | 181.8 | 193.0 | 246.9 |
| Superplastificante | 0.000 | 0.000 | 6.470 | 6.298 | 10.160 | 32.200 |
| AgregadoGraudo | 801.0 | 932.0 | 968.0 | 972.4 | 1028.4 | 1134.3 |
| AgregadoMiudo | 594.0 | 724.3 | 778.5 | 771.0 | 821.0 | 992.6 |
| Idade | 1.00 | 7.00 | 28.00 | 45.83 | 56.00 | 365.00 |
| Resistencia | 2.332 | 24.065 | 34.770 | 36.090 | 45.940 | 82.599 |

---

#### 1.5 Conclusão da Etapa 1

* A base de treino tem 721 observações.

* Não aparenta ter inconsistências nos dados

---
## Etapa 2:

---
*Para a base da nossa análise, deve se verificar as seguintes coisas:*

* Redução de dimensão das covariáveis (Parcimonia)
* Matriz de correlação
* Multicolinearidade, usando critério de VIF (Variance Inflation Factor) como medida
* Seleção de variáveis

*Qual o ponto dessa etapa? Preparar as covariáveis para o ajuste. *

***Covariáveis IMPORTANTES/RELEVANTES***

---


#### 2.1 Corrplot das variáveis

![Corrplot das variaveis](2_1_corrplot.png)

A matriz de correlação não revela correlações muito fortes entre as variáveis preditoras (todas as correlações entre pares de preditores são menores que 0,7 em valor absoluto). Isso sugere **ausência de multicolinearidade** problemática, indicando que as covariáveis são aproximadamente linearmente independentes entre si. Porém, pode haver restrição estrutural nos dados: quando $X_1$ pode ser "reconstruído" por combinações de $X_2 + X_3 + X_4...$


---
#### 2.2 Diagnóstico de multicolinearidade usando VIF (Variance Inflation Factor) como medida

Com o modelo full(completo) $Y = X\beta + \varepsilon$, pode-se calcular os VIF's

| | Cimento | Escoria | CinzaVolante | Agua | Superplastificante | AgregadoGraudo | AgregadoMiudo | Idade |
|----|----------|----------|---------------|-------|---------------------|-----------------|----------------|--------|
|**VIF**  | 7.733 | 7.801 | 6.518  | 7.073 | 7.073  | 2.953 | 5.102 | 7.119  |

Os Valores do Fator de Inflação da Variância (VIF) são todos inferiores a 2. Esse resultado confirma a **ausência de multicolinearidade**; não há evidência de que alguma preditora seja combinação linear das demais.

---
#### 2.3 Seleção de variáveis

* Stepwise (BIC): a saída em R fica do jeito abaixo


```text

Resistencia ~ Cimento + Escoria + CinzaVolante + Agua + Idade

(Intercept): 34.5938 Cimento: 0.1113 Escoria: 0.0945 CinzaVolante: 0.0814 Agua: -0.2559 Idade: 0.1119

```

O método stepwise (BIC) partiu do modelo nulo e 
adicionou/removeu termos até encontrar o modelo com menor BIC. O modelo selecionado contém 
**Cimento, Escoria,  CinzaVolante,  Agua, Idade**. As covariáves Superplastificante, AgregadoGraudo e AgregadoMiudo foram excluídas.
* Cp de Mallows:
```text
Selection Algorithm: exhaustive
         Cimento Escoria CinzaVolante Agua Superplastificante AgregadoGraudo
1  ( 1 ) "*"     " "     " "          " "  " "                " "           
2  ( 1 ) "*"     " "     " "          " "  "*"                " "           
3  ( 1 ) "*"     " "     " "          " "  "*"                " "           
4  ( 1 ) "*"     "*"     " "          "*"  " "                " "           
5  ( 1 ) "*"     "*"     "*"          "*"  " "                " "           
6  ( 1 ) "*"     "*"     "*"          "*"  "*"                " "           
7  ( 1 ) "*"     "*"     "*"          "*"  "*"                "*"           
8  ( 1 ) "*"     "*"     "*"          "*"  "*"                "*"           
...
5  ( 1 ) " "           "*"  
6  ( 1 ) " "           "*"  
7  ( 1 ) " "           "*"  
8  ( 1 ) "*"           "*" 
......
```

|# de covariável | 1  | 2  | 3  | 4  |5  |6  |7  |8  |  
| :--- | :---: | :---: | :---: | :---: |:---: |:---: |:---: |:---: |
| **Cp de Mallows** |723.826  | 500.483 |  271.826  | 114.138 |13.184 |9.307|10.798|9.000|


O modelo com menor Cp é o de 8 variáveis, mas o de 6 (Resistencia ~ Cimento + Escoria + CinzaVolante + Agua + Superplastificante + Idade) é próximo também.

![gráfico dos valores de cp de mallows conforme aumenta # de covariaveis.png](2_3_cp_mallows.png)


---
* LASSO: 

```text
9 x 1 sparse Matrix of class "dgCMatrix"
                     lambda.min
(Intercept)        -26.91760181
Cimento              0.12112049
Escoria              0.10713827
CinzaVolante         0.09109392
Agua                -0.14727340
Superplastificante   0.29303701
AgregadoGraudo       0.01968407
AgregadoMiudo        0.02137302
Idade                0.11263026
```
O LASSO reteve todas as covariáveis, e atribui valores baixos de $\lambda$ para AgregadiGraudo e AgregadoMiudo.

---
Então pelas resultados de Step(BIC) e Cp de Mallow e parcimonia, o modelo terá 6 covariáveis (Resistencia ~ Cimento + Escoria + CinzaVolante + Agua + Superplastificante + Idade).
#### 2.4 Conclusão da Etapa 2

* A Matriz de Correlação não mostrou correlações elevadas entre preditoras, mas com o modelo full (completo) alguns valores de VIFs são razoavelmente elevados sugere um certo nível de multicolinearidade. 

* Na seleção de variáveis, o método Stepwise(BIC) reteve Cimento, Escoria, CinzaVolante, Agua, e Idade; o Cp de Mallows reteve as mesmas covariáveis mais a covariável Superplastificante . 

* Em respeito aos métodos usados (Stepwise, Cp e LASSO), selecionamos para a etapa seguinte **o modelo com Resistencia ~ Cimento + Escoria + CinzaVolante + Agua + Superplastificante + Idade**.”

---
## Etapa 3:

1. Ajustar o modelo
2. Fazer Diagnóstico/ análise de resíduo para verificar os pressupostos:
* Lineraridade (gráfico de resíduos vs. valores ajustados)
* Homocedasticidade (o mesmo gráfico + teste de Breusch-Pagan ou outros testes)
* Normalidade dos erros (QQ-plot + teste de Shapiro-Wilk ou outros)
* Independência (gráfico de resíduos vs. ordem de coleta, nesse caso não temos a ordem)

3. Se passar pelo crivo da análise de resíduo --> prossiga
4. Inclua a parte de detecção de outliers e pontos influentes. Opções: $H_{ii}$; DF-Betas; DF-Fits; D-Cook
5. Após análise de resíduo faça testes.

---
#### 3.1 Ajuste do modelo

Pela etapa 2, o modelo é da forma

$Y$ = $\text{Resistência à compressão} = {\beta}_0 + {\beta}_1 \text{Cimento}  + {\beta}_2 \text{Escoria} + {\beta}_3 \text{CinzaVolante}+ {\beta}_4 \text{Agua} + {\beta}_5 \text{Superplastificante} + {\beta}_6 {Idade} + \varepsilon $ 

onde $\varepsilon_{ij}$ são iid e seguem distribuição normal com média zero, e variancia $\sigma^2$

```
Call:
lm(formula = Resistencia ~ Cimento + Escoria + CinzaVolante + 
    Agua + Superplastificante + Idade, data = dados_treino)
Residuals:
    Min      1Q  Median      3Q     Max 
-29.277  -6.316   0.273   6.450  34.538 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)        28.885497   4.935315   5.853 7.37e-09 ***
Cimento             0.106108   0.005244  20.232  < 2e-16 ***
Escoria             0.088490   0.005836  15.163  < 2e-16 ***
CinzaVolante        0.070523   0.009198   7.667 5.76e-14 ***
Agua               -0.218949   0.024562  -8.914  < 2e-16 ***
Superplastificante  0.238821   0.098670   2.420   0.0158 *  
Idade               0.111404   0.006350  17.544  < 2e-16 ***

... ...
```

* Coeficientes estimados: $\hat{\beta_0} = 28.88$, $\hat{\beta_1} = 0.10$, $\hat{\beta_2} = 0.09$, $\hat{\beta_3}$ = 0.07, $\hat{\beta_4}$ = -0.21, $\hat{\beta_5}$ = 0.24, $\hat{\beta_6}$ = 0.11

Interpretação: 

${\beta_0}$: Não tem interpretação para $\beta_0$ pois aparentemente 0 não está no range das covariáveis (se não tem componentes, não tem concreto, consequentemente não tem resistência à compressão).

$\hat{\beta_1}$: Para cada kg de Cimento adicional em 1 $m^3$ de concreto, estima-se um aumento de 4% em média na resistência do concreto , mantendo as outras covariáveis fixas;

A mesma interpretação pode ser feita com $\hat{\beta_2}$, $\hat{\beta_3}$, $\hat{\beta_4}$, $\hat{\beta_5}$, uma vez que são componentes do concreto também.

$\hat{\beta_6}$: Para cada dia adicional na Idade do concreto (range = [1,180]), estima-se um aumento de 11% em média na resistência do concreto , mantendo as outras covariáveis fixas;

#### 3.1.1 VIFs do modelo reduzido

| | Cimento | Escoria | CinzaVolante | Agua | Superplastificante |  Idade |
|----|----------|----------|---------------|-------|---------------------|-----------------|
|**VIF**  | 2.012 | 1.75 | 2.348  | 1.958 | 2.455  | 1.124 | 


---

#### 3.2 Diagnóstico/ Análise de resíduos

##### 3.2.1 Independência

Como não temos a ordem da coleta, assume-se a independência.

(Claro, supondo que o índice dos dados sejam a ordem, podemos fazer um teste de Durbin-Watson, que daria um valor-p de 0.56.)

```
        Durbin-Watson test

data:  modelo_final
DW = 2.0125, p-value = 0.5691
alternative hypothesis: true autocorrelation is greater than 0

```



##### 3.2.2 Gráficos

Abaixo temos um QQ-plote um gráfico de resíduos vs valores ajustados.

![QQplot e Gráfico de resíduos vs valores ajustados](3_2_QQplot_residuo_vs_ajustadoe.png)

O gráfico de resíduos vs valores ajustados, aparenta ter alguma estrutura de funil, isso pode indicar que talvez uma trasformação seja necessária. Agora vamos prosseguir para os testes de pressupostos.

---
##### 3.2.3 Testes de normalidade e homocedasticidade

```
	Shapiro-Wilk normality test

data:  residuos
W = 0.99671, p-value = 0.1453

	studentized Breusch-Pagan test

data:  modelo
BP = 104.01, df = 6, p-value < 2.2e-16
```

Teste de Shapiro-Wilk deu um valor-p de 0.14, não é pequeno, suposição da normalidade não é violada.

Teste de Breusch-Pagan deu um valor-p extremamente pequeno, suposição da homocedasticidade é violada ---> problema.


##### 3.2.3.1 Problema da homocedasticidade

O resultado do teste de Breusch-Pagan indica que uma transformação pode ser necessária (ou até interações).

Foram feitas as seguintes possibilidades:

* $\log(\text{Resistência à compressão}) =  {\beta}_0 + {\beta}_1 \text{Cimento}  + {\beta}_2 \text{Escoria} + {\beta}_3 \text{CinzaVolante}+ {\beta}_4 \text{Agua} + {\beta}_5 \text{Superplastificante} + {\beta}_6 {Idade} + \varepsilon $ 

* $(\text{Resistência à compressão})^2 =  {\beta}_0 + {\beta}_1 \text{Cimento}  + {\beta}_2 \text{Escoria} + {\beta}_3 \text{CinzaVolante}+ {\beta}_4 \text{Agua} + {\beta}_5 \text{Superplastificante} + {\beta}_6 {Idade} + \varepsilon $ 

* $\sqrt{(\text{Resistência à compressão})} =  {\beta}_0 + {\beta}_1 \text{Cimento}  + {\beta}_2 \text{Escoria} + {\beta}_3 \text{CinzaVolante}+ {\beta}_4 \text{Agua} + {\beta}_5 \text{Superplastificante} + {\beta}_6 {Idade} + \varepsilon $ 

* $\frac{1}{(\text{Resistência à compressão})} =  {\beta}_0 + {\beta}_1 \text{Cimento}  + {\beta}_2 \text{Escoria} + {\beta}_3 \text{CinzaVolante}+ {\beta}_4 \text{Agua} + {\beta}_5 \text{Superplastificante} + {\beta}_6 {Idade} + \varepsilon $ 

As quatro transformações usuais acima não foram úteis para satisfazer o pressuposto de homocedasticidade. Mas conseguimos achar uma que é assim:

* $\sqrt{(\text{Resistência à compressão})} =  {\beta}_0 + {\beta}_1 \text{Cimento}  + {\beta}_2 \text{Escoria} + {\beta}_3 \text{CinzaVolante}+ {\beta}_4 \text{Agua} + {\beta}_5 \text{Superplastificante} + {\beta}_6 \log{\text{(Idade)}} + {\beta}_7 (\log{\text{Idade}})^2 + \varepsilon $ 

Essa transformação atende os pressupostos, porém, pelo princípio da parcimonia e manter o valor pedagógico desse trabalho, decidimos não fazer a análise com o modelo transformado por enquanto. Agora vamos fingir que a homocedasticidade do modelo original é satisfeita e prosseguir.

#### 3.3 Análise de outliers e pontos influentes

##### 3.3.1 Resíduos studentized e alavancagem (os $H_{ii}$)

![Gráfico de residuos studentized.png](3_3_1_residuo_studentized.png)



```
Outliers (resíduo studentizado > |3|): 219 
# A tibble: 1 × 13
  Cimento Escoria CinzaVolante  Agua Superplastificante AgregadoGraudo
    <dbl>   <dbl>        <dbl> <dbl>              <dbl>          <dbl>
1     315     137            0   145                5.9           1130

```

Apenas uma observação apresentou resíduo padronizado acima de |3|,
indicando ausência de grande quantidade de outliers extremos.
Isso sugere que o modelo não está sendo fortemente afetado por
observações discrepantes na variável resposta.

![Os hat values ordenados.png](3_3_1_hii_ordem.png)

Foram identificadas 57 observações com leverage ($h_{ii}$) acima do limiar 2p/n,
indicando pontos com combinações incomuns das variáveis explicativas.
Porém, leverage alto não implica necessariamente influência
significativa sobre o ajuste do modelo. 

Mas, se usarmos outro o segundo criterio (limiar = 1/2), temos 0 pontos
##### 3.3.2 Distância de Cook

Distância de Cook mede influência global, serve para identificar observações que "puxam" ou desviam a linha de regressão.

![Distância de Cook](3_3_2_DCook.png)

Usando o limiar $F_{n,n-p,0.5}$, nenhuma observação apresentou distância de Cook acima do limite formal,
sugerindo ausência de pontos altamente influentes no ajuste global
da regressão.


---
##### 3.3.3 DFFITS

DFFITS mede impacto no fitted, serve para identificar observações influentes na regressão linear.

![os DFFits](3_3_3_DFFits.png)


Nenhuma observação apresentou DFFITS acima do limite adotado,
indicando ausência de pontos com forte impacto nas predições do modelo.
##### 3.3.4  DFBETAS

DFBETAS medem a influência de cada observação no coeficiente de uma variável.

Nesse caso os DFBETAS não são muito informativos pois na nossa amostra existe concretos constituídos com ausência absoluta de certos componentes, (alguns não tem Escoria, alguns não tem CinzaVolante, alguns não tem Superplastificante).

Enfim, 
O critério DFBETAS identificou 143 observações potencialmente
influentes nos coeficientes.
o limite $\frac{2}{\sqrt{(n)}}$ torna-se bastante sensível em
amostras grandes, resultando na detecção de pequenas alterações
locais nos coeficientes.
o limite ficou 0.0745, qualquer observação que altere um coeficiente em mais de 
0.074 desvios padrão será marcada, muito sensivel.

Mas, testando com o critério alternativo ($|\text{DFBETAS}|>1$), nenhuma observação apresentou DFBETAS acima do limite adotado, nenhuma
altera o coeficiente em mais de 1 erro padrao


#### 3.4 Testes de significância formais

##### Fazendo um sumário do modelo com comando "summary" do R, tem-se:

```
Call:
lm(formula = Resistencia ~ Cimento + Escoria + CinzaVolante + 
    Agua + Superplastificante + Idade, data = dados_treino)

Residuals:
    Min      1Q  Median      3Q     Max 
-29.277  -6.316   0.273   6.450  34.538 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)        28.885497   4.935315   5.853 7.37e-09 ***
Cimento             0.106108   0.005244  20.232  < 2e-16 ***
Escoria             0.088490   0.005836  15.163  < 2e-16 ***
CinzaVolante        0.070523   0.009198   7.667 5.76e-14 ***
Agua               -0.218949   0.024562  -8.914  < 2e-16 ***
Superplastificante  0.238821   0.098670   2.420   0.0158 *  
Idade               0.111404   0.006350  17.544  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 10.3 on 714 degrees of freedom
Multiple R-squared:  0.6294,	Adjusted R-squared:  0.6263 
F-statistic: 202.1 on 6 and 714 DF,  p-value: < 2.2e-16

```

Interpretação: 

* As covariáveis são significativos globalmente (rejeita H0: todos β=0), pois valor-p do teste F Global é pequeno (menor que 2.2e-16)


##### Teste F Parcial

```
Anova Table (Type III tests)

Response: Resistencia
                   Sum Sq  Df  F value    Pr(>F)    
(Intercept)          3634   1  34.2555 7.366e-09 ***
Cimento             43427   1 409.3525 < 2.2e-16 ***
Escoria             24392   1 229.9290 < 2.2e-16 ***
CinzaVolante         6236   1  58.7851 5.762e-14 ***
Agua                 8430   1  79.4597 < 2.2e-16 ***
Superplastificante    621   1   5.8584   0.01575 *  
Idade               32653   1 307.7967 < 2.2e-16 ***
Residuals           75746 714                       
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

```

* Cada linha testa (t) a contribuição de UMA covariável controlando por TODAS as outras
Todos os coeficientes são estatisticamente significativos ao nível de 5%
são as variáveis mais importantes, seguidas por Escória 
(F = 229.93) e Água (F = 79.46). Superplastificante apresenta 
significância marginal (F = 5.86, p = 0.0158), indicando 
contribuição menor mas ainda relevante ao modelo.

#### 3.5 Conclusão da Etapa 3


- **Ajuste do modelo**  
  - Modelo múltiplo com 6 variáveis: Cimento, Escória, CinzaVolante, Água, Superplastificante, Idade  
  - R² ajustado = 0,626 → 62,6% da variabilidade da resistência é explicada  
  - Teste F global: F(6,714)=202,1; p<2,2e-16 → modelo globalmente significativo  

- **Significância dos coeficientes**  
  - Todos os coeficientes são estatisticamente significativos (p<0,05)  
  - Destaques: Cimento (t=20,23) e Idade (t=17,54) – maior impacto individual  
  - Superplastificante com significância marginal (p=0,0158)  

- **Multicolinearidade**  
  - VIFs entre 1,12 e 2,45 → sem problemas graves de colinearidade  

- **Verificação dos pressupostos**  
  - **Independência** – Teste de Durbin-Watson: p=0,569 → resíduos independentes  
  - **Normalidade** – Shapiro-Wilk: p=0,145 → normalidade aceita  
  - **Homocedasticidade** – Breusch-Pagan: p<2,2e-16 → **violada** (heterocedasticidade presente)  

- **Tentativas de correção da heterocedasticidade**  
  - Transformações testadas: $\log{Y}$, $Y^2$, $\sqrt{Y}$, $\frac{1}{Y}$ –-> não resolveram  
  - Transformação que funcionou: $\sqrt{(\text{Resistência à compressão})} =  {\beta}_0 + {\beta}_1 \text{Cimento}  + {\beta}_2 \text{Escoria} + {\beta}_3 \text{CinzaVolante}+ {\beta}_4 \text{Agua} + {\beta}_5 \text{Superplastificante} + {\beta}_6 \log{\text{(Idade)}} + {\beta}_7 (\log{\text{Idade}})^2 + \varepsilon $   
  - Por parcimônia e objetivo pedagógico, optou-se por manter o modelo original (ciente da violação)  
  - As interações não foram investigadas pois não somos especialistas em concreto

- **Outliers e pontos influentes**  
  - **Resíduo studentizado** – apenas 1 observação com |res| > 3 (observação 219)  
  - **Alavancagem (hii)** – 57 pontos acima de 2p/n, mas nenhum acima do critério alternativo (1/2)  
  - **Distância de Cook** – nenhum ponto acima do limiar $F_{n,n-p,0.5}$ → sem influência global  
  - **DFFITS** – nenhum ponto acima do limite → sem impacto forte nas predições  
  - **DFBETAS** – 143 pontos detectados com critério $\frac{2}{\sqrt{(n)}}$ (muito sensível); com critério $|\text{DFBETAS}|>1$ → nenhum ponto influente nos coeficientes 

---
##### Predição no conjunto de teste.

Usando o modelo ajustado da etapa 3 e o conjunto de dados 

|Índice(Teste)| Resistencia | Pred_Resistencia |
|------------------|--------------|------------------|
|1| 44.3 | 59.7 |
|2| 47.0 | 27.3 |
|3| 47.8 | 21.1 |
|4| 39.4 | 30.2 |
|5| 37.4 | 31.6 |
|6| 38.4 | 55.3 |
|7| 37.7 | 36.9 |
|8| 39.0 | 65.8 |
|9| 53.1 | 39.3 |
|10| 41.5 | 38.6 |
|11| 15.0 | 24.2 |
|12| 40.8 | 47.7 |
|13| 32.8 | 28.5 |
|14| 39.8 | 46.9 |
|15| 46.9 | 36.0 |
| ... | ... | ... |

##### Métricas

```
RMSE_teste: 10.6676 
 MAE_teste: 8.6598 
 MAPE: 0.3324 
 R² preditivo: 0.5745 
```

## Etapa 4:
**Validação** 

Procurar a resposta da seguinte questão: **O modelo é útil para uma nova Base de dados??**


1. Opções de validar o modelo:
* Treino-Teste (nosso caso: 70% treino e 30% teste)
* Validação cruzada (k-fold)
* LOOCV


---

#### 4.1 Divisão em treino e teste

O conjunto de daos original contém 1030 observações. O modelo foi ajustado com as 721 observações separadas aleatóriamente já no início com o set.seed(42) para reprodutividade, o grupo treino é formado por 721 (70%) observações, e o grupo treino é o que sobrou.
##### Predição no conjunto de teste.

Usando o modelo ajustado da etapa 3 e o conjunto de dados 

|Índice(Teste)| Resistencia | Pred_Resistencia |
|------------------|--------------|------------------|
|1| 44.3 | 59.7 |
|2| 47.0 | 27.3 |
|3| 47.8 | 21.1 |
|4| 39.4 | 30.2 |
|5| 37.4 | 31.6 |
|6| 38.4 | 55.3 |
|7| 37.7 | 36.9 |
|8| 39.0 | 65.8 |
|9| 53.1 | 39.3 |
|10| 41.5 | 38.6 |
|11| 15.0 | 24.2 |
|12| 40.8 | 47.7 |
|13| 32.8 | 28.5 |
|14| 39.8 | 46.9 |
|15| 46.9 | 36.0 |
| ... | ... | ... |

##### Métricas

```
RMSE_teste: 10.6676 
 MAE_teste: 8.6598 
 MAPE: 0.3324 
 R² preditivo: 0.5745 
```
#### 4.2 Validação Cruzada (k= 5 folds)

A validação cruzada com 5 partições foi aplicada ao modelo `Resistencia ~ Cimento + Escoria + CinzaVolante + Agua + Superplastificante + Idade` utilizando **todos os 1030 dados brutos** (sem separação prévia treino/teste). Os resultados médios foram:

- **RMSE = 10.46**  
- **R² = 0,61**  
- **MAE = 8.33**


```
> print(modelo_kfold)
Linear Regression 

1030 samples
   6 predictor

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 826, 824, 823, 825, 822 
Resampling results:

  RMSE      Rsquared   MAE     
  10.46889  0.6069782  8.331812

Tuning parameter 'intercept' was held constant at a value of TRUE
> cat("RMSE médio (5-fold CV):", round(rmse_cv, 4), "\n")
RMSE médio (5-fold CV): 10.4689 
```
## 4.3 Conclusão da etapa 4

- **Divisão treino-teste**  
  - Conjunto original: 1030 observações  
  - Divisão aleatória (set.seed(42)): 721 amostras para treino (70%) e 309 para teste (30%)  
  - Modelo ajustado na etapa 3 foi aplicado para predizer os valores do conjunto de teste  

- **Métricas no conjunto de teste**  
  - RMSE = 10,6676  
  - MAE = 8,6598  
  - MAPE = 0,3324 (erro percentual médio de ~33,2%)  
  - R² preditivo = 0,5745 → modelo explica cerca de 57,5% da variabilidade da resistência em dados não vistos  

- **Validação cruzada (5 folds)**  
  - Aplicada sobre todo o conjunto de dados (1030 amostras)  
  - RMSE médio = 10,4689 (muito próximo ao RMSE do teste)  
  - R² médio = 0,6070  
  - MAE médio = 8,3318  

- **Comparação e interpretação**  
  - O RMSE da validação cruzada (10,47) é ligeiramente inferior ao RMSE do teste (10,67), indicando consistência e ausência de overfitting severo  
  - O R² preditivo (0,5745) é um pouco menor que o R² ajustado do modelo de treino (0,6263), o que é esperado – a performance em novos dados tende a ser ligeiramente inferior  
  - O MAPE de 33,2% sugere que, em média, as previsões se desviam ~33% do valor real; pode ser melhorado com transformações ou inclusão de interações (ou trocando modelo)

- **Conclusão final da etapa**  
  - O modelo não apresenta capacidade  preditiva promisora, já que em média as previsões se desviam ~33% do valor real.
  - Não há evidências de overfitting significativo  
  - Tem margem para melhorias (por exemplo: correção da heterocedasticidade, transformações não lineares, etc)  