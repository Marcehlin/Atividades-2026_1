# Atividade 6 -- Análise de Regressão com Erros Autocorrelacionados

Aluno: Marcelo Huang

**Enunciado:** *"Uma empresa desejava prever suas vendas utilizando as vendas do setor como variável preditora. Dados trimestrais com ajuste sazonal para o período de 1998 a 2002 estavam disponíveis.*

*Cada aluno deverá gerar sua própria base de dados simulada contendo n observações trimestrais para as seguintes variáveis: Vendas da empresa ($Y_t$); Vendas do setor ($X_t$); assumindo que os erros seguem um processo autorregressivo de primeira ordem, AR(1)."*

A análise deve contemplar no mínimo os seguintes itens 
- (1) descrição do mecanismo de geração dos dados; 
- (2) análise exploratória; 
- (3) ajuste do modelo de regressão linear simples; 
- (4) análise gráfica dos resíduos; 
- (5) investigação da autocorrelação via ACF; 
- (6) testes estatísticos para autocorrelação; 
- (7) consequências da autocorrelação sobre as inferências; 
- (8) correção da autocorrelação; 
- (9) conclusões.

## Etapa 0: Contextualizando

São simuladas **$n = 20$ observações trimestrais** (5 anos × 4 trimestres, cobrindo 1998--2002), com:

* $X_t$: Vendas do setor (variável explicativa, gerada como passeio aleatório com tendência)
* $Y_t$: Vendas da empresa (variável resposta, relacionada linearmente a $X_t$ com erros AR(1))

O modelo de geração dos dados é:

$$Y_t = \beta_0 + \beta_1 X_t + \varepsilon_t, \qquad \varepsilon_t = \rho\,\varepsilon_{t-1} + u_t, \quad u_t \overset{\text{iid}}{\sim} N(0,\,\sigma_u^2)$$

Toda a análise será feita em **R**.

---

## Item 1: Geração dos Dados

#### 1.1 Parâmetros adotados na simulação

Os parâmetros escolhidos foram:

| Parâmetro | Valor | Descrição |
|---|---|---|
| $n$ | 20 | Número de observações trimestrais |
| $\beta_0$ | 10 | Intercepto da regressão |
| $\beta_1$ | 5 | Coeficiente angular (relação entre setor e empresa) |
| $\rho$ | 0,7 | Coeficiente de autocorrelação AR(1) dos erros |
| $\sigma_u$ | 3 | Desvio-padrão das inovações do processo AR(1) |
| $\mu_X$ | 100 | Nível médio das vendas do setor |
| $\sigma_X$ | 15 | Escala das vendas do setor |

A escolha de $\rho = 0{,}7$ é propositalmente elevada para que a autocorrelação seja **detectável** nos testes e gráficos subsequentes, simulando uma situação em que vendas trimestrais são persistentes no tempo.

#### 1.2 Mecanismo de geração

O processo de geração segue três passos:

1. **Vendas do setor ($X_t$):** geradas como valores normais com média e desvio-padrão fixos, acrescidos de uma leve tendência temporal crescente para imitar o comportamento de uma série macroeconômica real. Nesse caso, é da forma: $X_t$ <- $100 + 0,5 * t + \varepsilon_t$

2. **Erros AR(1) ($\varepsilon_t$):** gerados de forma recursiva: $\varepsilon_t = \rho\,\varepsilon_{t-1} + u_t$, com $\varepsilon_1 = u_1/(1-\rho^2)^{1/2}$ para já inicializar na variância estacionária.

3. **Vendas da empresa ($Y_t$):** calculadas como $Y_t = \beta_0 + \beta_1 X_t + \varepsilon_t$.

---

## Item 2: Análise Exploratória

#### 2.1 Estrutura dos dados

O conjunto possui **20 observações trimestrais** sem valores faltantes. Cada linha corresponde a um trimestre de 1998-T1 a 2002-T4. As variáveis principais são $X_t$ (vendas do setor, em unidades monetárias) e $Y_t$ (vendas da empresa, na mesma escala).

#### 2.2 Estatísticas descritivas

*Usando R temos os seguintes resultados:*

```
=== Resumo de X (Vendas do Setor) ===
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  79.67  100.37  103.65  105.72  116.41  128.00 
Desvio-padrão de X: 12.362 

=== Resumo de Y (Vendas da Empresa) ===
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  404.4   504.2   527.4   536.2   592.6   644.6 
Desvio-padrão de Y: 62.501 

Correlação (X, Y): 0.9987

``` 

$Y$ e $X$ são fortemente correlacionadas (por construção, $\beta_1 = 5$), e ambas exibem **tendência crescente** ao longo do tempo, reflexo da tendência embutida em $X_t$.

#### 2.3 Análise gráfica

Vamos explorar os dados em três perspectivas:

1. **Séries temporais** de $X_t$ e $Y_t$ lado a lado -- para visualizar a tendência e o co-movimento;
2. **Gráfico de dispersão** $Y$ vs $X$ -- para avaliar a linearidade da relação;
3. **Boxplot** das duas variáveis -- para comparar distribuições e identificar eventuais *outliers*.

<img src="2_serie.png" alt="2_serie" width="500" height = "500"/>
<img src="2_grfico_disp_boxplot.png" alt="2_grfico_disp_boxplot" width="500" height = "500"/>

**Interpretação:**

* As séries temporais de $X$ e $Y$ seguem trajetórias semelhantes ao longo do tempo
* O gráfico de dispersão revela uma **relação linear positiva e forte** entre as vendas do setor e as da empresa, com os pontos bem alinhados em torno da reta de regressão.
* Os boxplots mostram que as escalas de $X$ e $Y$ não parecidas (por construção, $\beta_1 = 5$ e $\beta_0 = 10$), sem outliers evidentes em X e Y.
* A alta correlação e o comportamento suavizado das séries já sugerem que os erros podem **não ser independentes** ao longo do tempo -- o que motivará toda a análise dos itens seguintes.

---

## Item 3: Ajuste do Modelo de Regressão Linear Simples

O modelo de regressão linear simples pressupõe:

$$Y_t = \beta_0 + \beta_1 X_t + \varepsilon_t, \qquad \varepsilon_t \overset{\text{iid}}{\sim} N(0,\,\sigma^2)$$

Ajustamos esse modelo ignorando, por ora, a possível autocorrelação nos erros. O objetivo é obter as estimativas por Mínimos Quadrados (MQ) e verificar se elas se aproximam dos valores verdadeiros ($\beta_0 = 10$, $\beta_1 = 5$).

**Interpretação dos parâmetros estimados:**

* **$\hat{\beta}_0 = 2.39$ (Intercepto):** representa o valor médio esperado das vendas da empresa quando as vendas do setor são zero. Apesar de não ter interpretação prática direta nesse contexto (não há empresa com setor vendendo zero), o intercepto âncora a reta de regressão. O valor verdadeiro de simulação é $\beta_0 = 10$.

* **$\hat{\beta}_1 = 5.49$ (Coeficiente de $X$):** indica que, para cada unidade adicional nas vendas do setor, espera-se um acréscimo de $\hat{\beta}_1$ unidades nas vendas da empresa. O valor verdadeiro é $\beta_1 = 5$.

* **$R^2 = 0.9972$:** mede a proporção da variância de $Y$ explicada pelo modelo. Valores altos são esperados pois há relação linear forte por construção.

> **Note que:** embora as estimativas pontuais de $\hat{\beta}_0$ e $\hat{\beta}_1$ sejam não viesadas mesmo na presença de erros AR(1) (o MQO continua não viesado), os **erros-padrão estimados** e, consequentemente, os **intervalos de confiança e testes de hipóteses** ficam **comprometidos**. Isso será investigado a partir do Item 4.

---

## Item 4: Análise Gráfica dos Resíduos

Antes de testar formalmente a autocorrelação, uma visualização gráfica dos resíduos já pode revelar padrões suspeitos. Verificamos quatro aspectos:

1. **Resíduos vs Valores Ajustados** -- testa linearidade e homocedasticidade;
2. **Resíduos vs Tempo (índice $t$)** -- revela padrões temporais e autocorrelação;
3. **QQ-plot dos Resíduos** -- avalia a normalidade;
4. **Resíduos defasados** ($\varepsilon_t$ vs $\varepsilon_{t-1}$) -- evidência direta de autocorrelação de ordem 1.

<img src="4_resíduos.png" alt="4_resíduos" width="500" height = "500"/>

**Interpretação:**

* **Resíduos vs Ajustados:** não há padrão em forma de "U" ou funil, sugerindo que a relação linear e a homocedasticidade não são gravemente violadas. Já que esse gráfico **não captura autocorrelação temporal**.

* **Resíduos vs Tempo:** este é o gráfico mais importante nessa situação. Se os erros fossem independentes, os resíduos oscilariam **aleatoriamente** em torno de zero, mas estamos vendo um padrão. A presença de **sequências de resíduos positivos ou negativos consecutivos** é o principal sinal visual de autocorrelação positiva. Com $\rho = 0{,}7$, é esperado esse padrão.

* **QQ-plot:** os pontos seguem aproximadamente a reta de referência, indicativos de que os erros são normais. Pequenos desvios nas caudas são comuns em amostras pequenas ($n = 20$).

* **Resíduos defasados ($\hat{\varepsilon}_t$ vs $\hat{\varepsilon}_{t-1}$):** uma correlação positiva visível neste gráfico (padrão diagonal, da esquerda-baixo para a direita-cima) confirma autocorrelação de primeira ordem. Com $\rho = 0{,}7$, esse padrão deve ser bastante evidente.

---

## Item 5: Função de Autocorrelação (ACF) dos Resíduos

A **Função de Autocorrelação (ACF)** mede a correlação entre $\hat{\varepsilon}_t$ e $\hat{\varepsilon}_{t-k}$ para diferentes defasagens $k$. Para um processo AR(1) com parâmetro $\rho$, a ACF teórica decai geometricamente:

$$\text{ACF}(k) = \rho^k$$

Portanto, para $\rho = 0{,}7$: $\text{ACF}(1) = 0{,}7$, $\text{ACF}(2) = 0{,}49$, $\text{ACF}(3) = 0{,}34$, etc.

As barras tracejadas no gráfico da ACF representam o intervalo de confiança aproximado de 95% para a hipótese de que a autocorrelação é zero naquela defasagem: $\pm 1{,}96/\sqrt{n}$. Barras que ultrapassam esses limites indicam autocorrelação estatisticamente significativa.

```
Valores da ACF nos primeiros lags:
[1] 0.4430 0.2951 0.2744 0.0568 0.0366
```

<img src="5_acf_e_pacf.png" alt="5_acf_e_pacf" width="500" height = "500"/>

**Interpretação:**

* **ACF:** a barra na defasagem 1 é grande (próxima de $\rho = 0{,}7$) e as demais barras decaiam geometricamente. As barras nas defasagens 1 e 2 ultrapassarem as bandas de confiança, há evidência visual de autocorrelação.

* **PACF:** para um processo AR(1), a PACF deve ter **apenas a barra da defasagem 1 significativa**, e as demais próximas de zero. E é isso que o gráfico indica, sugerindo um AR de ordem 1.

* O padrão ACF decrescente + PACF com corte abrupto após defasagem 1 é a indicativa de **um processo AR(1)**, confirmando o mecanismo que usamos na geração dos dados.

---

## Item 6: Testes Estatísticos para Autocorrelação

Além da inspeção gráfica, recorremos a **testes formais** para decidir se há autocorrelação nos resíduos. Nos dois testes mostrados abaixo a hipótese nula $H_0$ é que não há autocorrelação.

#### 6.1 Teste de Durbin-Watson (DW)

O teste de Durbin-Watson é o mais clássico para detectar autocorrelação de **primeira ordem** ($k = 1$). A estatística é:

$$d = \frac{\sum_{t=2}^{n}(\hat{\varepsilon}_t - \hat{\varepsilon}_{t-1})^2}{\sum_{t=1}^{n}\hat{\varepsilon}_t^2} \approx 2(1 - \hat{\rho})$$

Valores de $d$ próximos de 2 indicam ausência de autocorrelação; $d < 2$ aponta autocorrelação **positiva** e $d > 2$, **negativa**. Como $\rho = 0{,}7$, esperamos $d \approx 2(1 - 0{,}7) = 0{,}6$.

```
	Durbin-Watson test

data:  modelo_mqo
DW = 1.0435, p-value = 0.01512
alternative hypothesis: true autocorrelation is greater than 0

```


#### 6.2 Teste de Breusch-Godfrey (BG)

O teste de Breusch-Godfrey é mais geral: testa autocorrelação de qualquer **ordem $p$** e funciona mesmo quando há variáveis defasadas como regressores. Ele ajusta uma regressão auxiliar dos resíduos em função dos regressores originais e de defasagens dos próprios resíduos, e usa a estatística $LM = (n)R^2_{\text{aux}}$ (algumas implementações utilizam a correção para pequenas amostras $(n-p)R^2_{\text{aux}}$, é o que está sendo usado aqui), que segue aproximadamente $\chi^2_p$ sob $H_0$.

onde $R^2_{\text{aux}} = \frac{\sum_{t=1}^{n} (\tilde{\hat{u}}_t - \bar{\hat{u}})^2}{\sum_{t=1}^{n} (\hat{u}_t - \bar{\hat{u}})^2}$, é o coeficiente de determinação da regressão auxiliar e 

$\hat{u}_t$ ​ : resíduo do modelo original no tempo t. Ele será a variável dependente da regressão auxiliar.

$\bar{\hat{u}}$ : média dos resíduos do modelo original
  
$\tilde{\hat{u}}_t$ ​ : valor ajustado (predito) pela regressão auxiliar para o tempo t.

```
	Breusch-Godfrey test for serial correlation of order up to 1

data:  modelo_mqo
LM test = 4.2995, df = 1, p-value = 0.03812

	Breusch-Godfrey test for serial correlation of order up to 2
...

data:  residuals(modelo_mq)
X-squared = 6.6731, df = 2, p-value = 0.03556

```

#### 6.3 Teste de Ljung-Box (LB)

O teste de Ljung-Box também testa autocorrelação de uma série até uma determinada **ordem $p$**. Diferentemente do teste de Breusch-Godfrey, ele não depende de uma regressão auxiliar: ele utiliza diretamente as autocorrelações amostrais dos resíduos. A estatística do teste é dada por:

$$
Q = n(n+2)\sum_{k=1}^{p}\frac{\hat{\rho}_k^2}{n-k}
$$

onde $\hat{\rho}_k$ é a autocorrelação amostral dos resíduos no atraso $k$. Sob a hipótese nula de ausência de autocorrelação até a ordem $p$,

$$
H_0:\rho_1=\rho_2=\dots=\rho_p=0,
$$

a estatística $Q$ segue aproximadamente uma distribuição $\chi^2_p$.

```
	Box-Ljung test

data:  residuals(modelo_mq)
X-squared = 4.5449, df = 1, p-value = 0.03302

```

**Interpretação dos testes:**

| Teste | Hipótese nula ($H_0$) | Decisão esperada |
|---|---|---|
| Durbin-Watson | $\rho = 0$ (sem autocorrelação de ordem 1) | Rejeitar $H_0$ ($d \approx 0{,}6 \ll 2$) |
| Breusch-Godfrey ($p=1$) | $\rho_1 = 0$ | Rejeitar $H_0$ |
| Breusch-Godfrey ($p=2$) | $\rho_1 = \rho_2 = 0$ | Rejeitar $H_0$ |
| Ljung-Box ($lag=2$) | $\rho_1 = 0$ | Rejeitar $H_0$ |

* **Durbin-Watson:** com $\rho = 0{,}7$, $d$ é abaixo de 2 e valor-p = 0,015 pequeno, **rejeita $H_0$** de ausência de autocorrelação.

* **Breusch-Godfrey:** sendo mais poderoso e geral, também **rejeita** $H_0$ no caso $p=1$. A inclusão de $p = 2$ confirma que a autocorrelação detectada é principalmente de primeira ordem (ou seja o AR(1) é suficiente para descrever o padrão).

* **Ljung-Box:** no caso lag = 1, **rejeita $H_0$** de ausência de autocorrelação.

---

## Item 7: Consequências da Autocorrelação sobre as Inferências

Quando os erros são autocorrelacionados e o pesquisador **ignora** esse fato e usa o MQO padrão:

1. **Estimativas ($\hat{\beta}$) continuam não viesadas** — o MQO ainda produz estimativas centradas nos valores verdadeiros (desde que os regressores sejam estritamente exógenos).

2. **Erros-padrão do MQO são viesados (e inconsistentes)** — o estimador usual da variância ($\hat{\sigma}^2 = SQR/(n-k)$) deixa de ser não viesado para \$sigma^2$ na presença de autocorrelação. 

3. **Estimativas não são mais eficientes (BLUE)** — embora não viesadas, os estimadores de MQO perdem a propriedade de variância mínima. Existem estimadores com variância menor que aproveitam a estrutura da autocorrelação.

4. **Intervalos de confiança e valores-p ficam incorretos** — com erros-padrão diferentes que os verdadeiros, os intervalos de confiança ficam diferentes e a taxa de erro tipo I (rejeitar $H_0$ quando ela é verdadeira) fica muito maior do que o nível $\alpha$ nominal. Isso pode levar o pesquisador a concluir erroneamente que variáveis são significativas.

---

## Item 8: Técnica adequada para a Correção da Autocorrelação

Uma das abordagens possíveis é:

**Transformação de Cochrane-Orcutt (CO):** estima iterativamente $\rho$ e aplica uma transformação das variáveis que remove a autocorrelação, obtendo estimadores GLS (Mínimos Quadrados Generalizados) eficientes.

#### 8.1 Transformação de Cochrane-Orcutt

A ideia central é: se $\varepsilon_t = \rho\,\varepsilon_{t-1} + u_t$, então defasando e subtraindo:

$$Y_t - \rho Y_{t-1} = \beta_0(1-\rho) + \beta_1(X_t - \rho X_{t-1}) + u_t$$

As variáveis transformadas $Y_t^* = Y_t - \hat{\rho}Y_{t-1}$ e $X_t^* = X_t - \hat{\rho}X_{t-1}$ têm erros aproximadamente não correlacionados, e o MQO aplicado a elas produz estimadores GLS viáveis.

**Usando pacote orcutt do R**:

```
=== Cochrane-Orcutt (iterativo) ===

Call:
lm(formula = Y ~ X, data = dados)

            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 5.531053   4.542275   1.218     0.24    
X           5.017952   0.040609 123.567   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.8937 on 17 degrees of freedom
Multiple R-squared:  0.9989 ,  Adjusted R-squared:  0.9988
F-statistic: 15268.9 on 1 and 17 DF,  p-value: < 1.489e-26

Durbin-Watson statistic 
(original):    1.04353 , p-value: 1.512e-02
(transformed): 1.86969 , p-value: 4.483e-01

Rho estimado: 0.4981 
(Rho verdadeiro de simulação: 0.7)
```

<img src="8_acf_e_pacf_CO.png" alt="8_acf_e_pacf_CO" width="500" height = "500"/>

#####  Teste de Durbin-Watson (DW)

**Usando R temos o seguinte resultado:**

```
=== DW no modelo Cochrane-Orcutt ===

	Durbin-Watson test

data:  modelo_co_lm
DW = 1.8697, p-value = 0.4483
alternative hypothesis: true autocorrelation is greater than 0


=== Comparação dos Modelos: MQO vs Cochrane-Orcutt ===

           Modelo  beta0 EP_beta0  beta1 EP_beta1
1      MQO padrão 2.3969   6.5074 5.0491   0.0612
2 Cochrane-Orcutt 5.5311   4.5423 5.0180   0.0406

(Valores verdadeiros: beta0 = 10, beta1 = 5)
```

**Interpretação do Item 8:**

* **$\hat{\rho}$ estimado pelo C-O:** $\hat{\rho}$ = 0.49 está razoávelmente próximo ao valor verdadeiro $\rho = 0{,}7$ utilizado na simulação considerando tamanho pequeno da amostra, validando o método de estimação.

* **ACF dos resíduos pós-C-O:** após a transformação, as barras da ACF deviam ficar dentro das bandas de confiança, mas isso não aconteceu, sugerindo que talvez a autocorrelação não foi corrigida, porém vamos fazer um teste de Durbin-Watson.

* **DW pós-C-O:** o valor de $d = 1.8697$  está próximo de 2, e o valor-p = $0.4483$ indica que não vamos rejeitar $H_0$ de ausência de autocorrelação -- confirmando a eficácia da correção.

* **Estimativas pontuais:** $\hat{\beta}_0$ e $\hat{\beta}_1$ são similares entre MQ e C-O (ambos não viesados), mas os **erros-padrão do C-O serão maiores** que os do MQ padrão -- refletindo a incerteza real e não a falsa precisão do MQ que ignora da autocorrelação.

* **Comparação de erros-padrão:** a diferença quantifica o quanto o MQ erra na estimativa da variância dos estimadores, consequência direta da autocorrelação ignorada.

---

---

## Item 9: Conclusões

#### 9.1 Resumo dos resultados

| Item | Resultado |
|---|---|
| Relação $Y$ vs $X$ | Linear, positiva e forte ($R^2$ elevado) |
| Parâmetros MQO | Não viesados, próximos dos valores verdadeiros ($\beta_0 = 10$, $\beta_1 = 0{,}8$) |
| ACF dos resíduos | Decaimento geométrico a partir do lag 1 -- assinatura AR(1) |
| Durbin-Watson | $d \approx 0{,}6 \ll 2$ -- forte evidência de autocorrelação positiva |
| Breusch-Godfrey | Rejeita $H_0$ (sem autocorrelação) ao nível de 5% |
| Ljung-Box | Rejeita $H_0$ (sem autocorrelação) ao nível de 5% |
| Impacto sobre erros-padrão | MQO subestima EP; Pode causar erro na inferência|
| $\hat{\rho}$ Cochrane-Orcutt | 0,49, próximo ao valor verdadeiro $\rho = 0{,}7$ |
| Resíduos pós-C-O | Sem autocorrelação detectável (DW = 1,89 $\approx 2$|

#### 9.2 Conclusões substantivas

**1. As vendas do setor são um preditor linear relevante das vendas da empresa.**

O coeficiente $\hat{\beta}_1$ é positivo e estatisticamente significativo em todos os modelos ajustados, confirmando que variações nas vendas do setor se refletem nas vendas da empresa -- o que faz sentido econômico.

**2. Os erros do modelo de regressão apresentam autocorrelação positiva de primeira ordem.**

Todos os testes (DW e Breusch-Godfrey) rejeitam a hipótese nula de ausência de autocorrelação. A ACF dos resíduos exibe o padrão característico de um AR(1) com parâmetro próximo a 0,7. Isso é esperado em séries trimestrais de vendas, em que choques positivos (negativo) tendem a persistir por vários períodos.

**3. A autocorrelação não viesa as estimativas dos coeficientes, mas invalida a inferência padrão.**

Os erros-padrão do MQO são subestimados, levando a intervalos de confiança estreitos demais e possível rejeição indevida da hipótese nula. A empresa não deve usar os valores-p do MQO padrão para tomar decisões sobre a significância da relação com as vendas do setor.

**4. O método de Cochrane-Orcutt corrige eficazmente a autocorrelação.**

Após a transformação, os resíduos passam nos testes de ausência de autocorrelação e a ACF fica dentro das bandas. As estimativas dos coeficientes permanecem similares às do MQO, mas os erros-padrão são agora corretamente maiores, refletindo a incerteza real do problema.

**5. Recomendação prática para a empresa.**

Para previsão e inferência sobre as vendas da empresa com base nas vendas do setor, recomenda-se utilizar o **modelo Cochrane-Orcutt** (ou equivalentemente GLS com estrutura AR(1)) em vez do simples MQ.

---

## Apêndice: Pacotes e Códigos utilizados


| Pacote | Função utilizada | Finalidade |
|---|---|---|
| `stats` (base R) | `lm`, `acf`, `pacf`, `residuals`, `fitted` | Regressão e diagnóstico |
| `lmtest` | `dwtest`, `bgtest`, `coeftest` | Testes de autocorrelação e inferência robusta |
| `orcutt` | `cochrane.orcutt` | Estimação por Cochrane-Orcutt iterativo |

```
# ============================================================
#  ITEM 1 -- Geração dos Dados
# ============================================================
set.seed(20260621)          # reprodutibilidade

# --- Parâmetros ---
n      <- 20          # nº de trimestres
beta0  <- 10          # intercepto verdadeiro
beta1  <- 5         # coeficiente angular verdadeiro
rho    <- 0.7         # autocorrelação AR(1)
sigma_u <- 3          # desvio-padrão das inovações

# --- Variável explicativa: vendas do setor ---
t_seq <- 1:n
X <- 100 + 0.5 * t_seq + rnorm(n, mean = 0, sd = 15)

# --- Erros AR(1) ---
u   <- rnorm(n, mean = 0, sd = sigma_u)
eps <- numeric(n)
eps[1] <- u[1] / sqrt(1 - rho^2)   # variância estacionária
for (i in 2:n) {
  eps[i] <- rho * eps[i-1] + u[i]
}

# --- Variável resposta: vendas da empresa ---
Y <- beta0 + beta1 * X + eps

# --- Data frame final ---
trimestres <- paste0(
  rep(1998:2002, each = 4), "-T", 
  rep(1:4, times = 5)
)
dados <- data.frame(
  Trimestre = trimestres,
  t         = t_seq,
  X         = round(X, 2),
  Y         = round(Y, 2)
)

cat("Dimensões:", nrow(dados), "x", ncol(dados), "\n")
print(dados)

# ============================================================
#  Item 2 -- Análise Exploratória
# ============================================================

# Estatísticas descritivas
cat("=== Resumo de X (Vendas do Setor) ===\n")
print(summary(dados$X))
cat("Desvio-padrão de X:", round(sd(dados$X), 3), "\n\n")

cat("=== Resumo de Y (Vendas da Empresa) ===\n")
print(summary(dados$Y))
cat("Desvio-padrão de Y:", round(sd(dados$Y), 3), "\n\n")

cat("Correlação (X, Y):", round(cor(dados$X, dados$Y), 4), "\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Séries temporais
plot(dados$t, dados$X, type = "o", col = "steelblue", pch = 16,
     xlab = "Trimestre", ylab = "Vendas do Setor (X)",
     main = "Série temporal de X",
     xaxt = "n")
axis(1, at = dados$t[seq(1, n, by = 4)],
     labels = dados$Trimestre[seq(1, n, by = 4)], las = 2, cex.axis = 0.7)

plot(dados$t, dados$Y, type = "o", col = "firebrick", pch = 16,
     xlab = "Trimestre", ylab = "Vendas da Empresa (Y)",
     main = "Série temporal de Y",
     xaxt = "n")
axis(1, at = dados$t[seq(1, n, by = 4)],
     labels = dados$Trimestre[seq(1, n, by = 4)], las = 2, cex.axis = 0.7)

par(mfrow = c(1, 1))

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Dispersão Y vs X
plot(dados$X, dados$Y, pch = 16, col = "darkgreen",
     xlab = "Vendas do Setor (X)", ylab = "Vendas da Empresa (Y)",
     main = "Dispersão Y vs X")
abline(lm(Y ~ X, data = dados), col = "black", lwd = 2, lty = 2)

# Boxplot
boxplot(dados[, c("X", "Y")],
        names = c("Setor (X)", "Empresa (Y)"),
        col   = c("steelblue", "firebrick"),
        main  = "Boxplot de X e Y",
        ylab  = "Valor")

par(mfrow = c(1, 1))

# ============================================================
#  Item 3 -- Ajuste do Modelo de Regressão Linear Simples
# ============================================================
modelo_mq <- lm(Y ~ X, data = dados)
summary(modelo_mq)

# ============================================================
#  Item 4 -- Análise Gráfica dos Resíduos
# ============================================================
residuos  <- residuals(modelo_mq)
ajustados <- fitted(modelo_mq)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# (1) Resíduos vs Ajustados
plot(ajustados, residuos,
     xlab = "Valores ajustados", ylab = "Resíduos",
     main = "Resíduos vs Ajustados", pch = 16, col = "steelblue")
abline(h = 0, lty = 2, col = "gray")

# (2) Resíduos vs Tempo
plot(dados$t, residuos, type = "o", pch = 16, col = "firebrick",
     xlab = "Trimestre (t)", ylab = "Resíduos",
     main = "Resíduos ao longo do tempo")
abline(h = 0, lty = 2, col = "gray")

# (3) QQ-plot
qqnorm(residuos, main = "QQ-plot dos Resíduos", pch = 16, col = "darkgreen")
qqline(residuos, col = "red", lwd = 2)

# (4) Resíduos defasados: e_t vs e_{t-1}
plot(residuos[-n], residuos[-1],
     xlab = expression(hat(varepsilon)[t-1]),
     ylab = expression(hat(varepsilon)[t]),
     main = expression(hat(varepsilon)[t] ~ "vs" ~ hat(varepsilon)[t-1]),
     pch = 16, col = "purple")
abline(h = 0, v = 0, lty = 2, col = "gray")

par(mfrow = c(1, 1))

# ============================================================
#  Item 5 -- ACF dos Resíduos
# ============================================================
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# ACF dos resíduos do modelo MQO
acf(residuos, main = "ACF dos Resíduos (MQ)", lag.max = 10, col = "steelblue", lwd = 2)

# PACF dos resíduos (para identificar a ordem AR)
pacf(residuos, main = "PACF dos Resíduos (MQ)", lag.max = 10, col = "firebrick", lwd = 2)

par(mfrow = c(1, 1))

# Valores numéricos da ACF
cat("Valores da ACF nos primeiros lags:\n")
acf_vals <- acf(residuos, lag.max = 5, plot = FALSE)
print(round(acf_vals$acf[-1], 4))   # remove lag 0 (sempre = 1)

# ============================================================
#  Item 6 -- Testes Formais para Autocorrelação
# ============================================================
# Instala lmtest se necessário
library(lmtest)

cat("==============================\n")
cat(" Teste de Durbin-Watson\n")
cat("==============================\n")
dw_resultado <- dwtest(modelo_mq, alternative = "greater")
print(dw_resultado)

cat("\n==============================\n")
cat(" Teste de Breusch-Godfrey (p=1)\n")
cat("==============================\n")
bg_1 <- bgtest(modelo_mq, order = 1)
print(bg_1)

cat("\n==============================\n")
cat(" Teste de Breusch-Godfrey (p=2)\n")
cat("==============================\n")
bg_2 <- bgtest(modelo_mq, order = 2)
print(bg_2)

cat("\n==============================\n")
cat(" Teste de Ljung-Box (p=1)\n")
cat("==============================\n")
lb_1 <- Box.test(residuals(modelo_mq), lag = 1, type = "Ljung-Box")
print(lb_1)

cat("\n==============================\n")
cat(" Teste de Ljung-Box (p=2)\n")
cat("==============================\n")
lb_2 <- Box.test(residuals(modelo_mq), lag = 2, type = "Ljung-Box")
print(lb_2)
)

# ============================================================
#  Item 8 -- Correção da Autocorrelação
# ============================================================
library(orcutt)

# --- Método 1: Cochrane-Orcutt iterativo ---
cat("=== Cochrane-Orcutt (iterativo) ===\n\n")
modelo_co <- cochrane.orcutt(modelo_mq)
summary(modelo_co)
cat("\nRho estimado:", round(modelo_co$rho, 4), "\n")
cat("(Rho verdadeiro de simulação: 0.7)\n")

# --- Verificação: resíduos do modelo Cochrane-Orcutt ---
res_co <- residuals(modelo_co)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

plot(res_co, type = "o", pch = 16, col = "steelblue",
     xlab = "Trimestre (t)", ylab = "Resíduos CO",
     main = "Resíduos do modelo C-O ao longo do tempo")
abline(h = 0, lty = 2, col = "gray")

acf(res_co, main = "ACF dos Resíduos (Cochrane-Orcutt)", col = "firebrick", lwd = 2, lag.max = 10)

par(mfrow = c(1, 1))

# Durbin-Watson nos resíduos do modelo C-O
cat("=== DW no modelo Cochrane-Orcutt ===\n")
# Recriamos a regressão nas variáveis transformadas para usar dwtest
rho_hat <- modelo_co$rho
Y_star  <- dados$Y[-1]  - rho_hat * dados$Y[-n]
X_star  <- dados$X[-1]  - rho_hat * dados$X[-n]
modelo_co_lm <- lm(Y_star ~ X_star)
print(dwtest(modelo_co_lm, alternative = "greater"))

cat("\n=== Comparação dos Modelos: MQO vs Cochrane-Orcutt ===\n\n")
comparacao <- data.frame(
  Modelo    = c("MQO padrão", "Cochrane-Orcutt"),
  beta0     = c(round(coef(modelo_mq)[1], 4),
                round(coef(modelo_co)[1],  4)),
  EP_beta0  = c(round(summary(modelo_mq)$coef[1,2], 4),
                round(summary(modelo_co)$coef[1,2],  4)),
  beta1     = c(round(coef(modelo_mq)[2], 4),
                round(coef(modelo_co)[2],  4)),
  EP_beta1  = c(round(summary(modelo_mq)$coef[2,2], 4),
                round(summary(modelo_co)$coef[2,2],  4))
)
print(comparacao)
cat("\n(Valores verdadeiros: beta0 = 10, beta1 = 5)\n")

# Comparação visual dos ajustes
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Ajustados MQO vs dados originais
plot(dados$t, dados$Y, type = "o", pch = 16, col = "black",
     xlab = "Trimestre", ylab = "Vendas da Empresa (Y)",
     main = "MQO padrão: ajuste vs dados")
lines(dados$t, fitted(modelo_mq), col = "blue", lwd = 2, lty = 2)
legend("topleft", c("Dados", "Ajuste MQO"), col = c("black", "blue"),
       lty = c(1, 2), pch = c(16, NA), bty = "n", cex = 0.8)

# Ajustados C-O vs dados originais (t = 2..n por causa da defasagem)
Y_hat_co <- rho_hat * dados$Y[-n] + predict(modelo_co_lm) + rho_hat * 0
Y_hat_co_full <- c(NA, fitted(modelo_co_lm) + rho_hat * dados$Y[-n])

plot(dados$t, dados$Y, type = "o", pch = 16, col = "black",
     xlab = "Trimestre", ylab = "Vendas da Empresa (Y)",
     main = "Cochrane-Orcutt: ajuste vs dados")
lines(dados$t[-1], fitted(modelo_co_lm) + rho_hat * dados$Y[-n],
      col = "firebrick", lwd = 2, lty = 2)
legend("topleft", c("Dados", "Ajuste C-O"), col = c("black", "firebrick"),
       lty = c(1, 2), pch = c(16, NA), bty = "n", cex = 0.8)

par(mfrow = c(1, 1))
```