# Atividade 5 -- Análise de Regressão: Calibração de Instrumentos


**Enunciado:** *"Dois instrumentos foram construídos para medir pressão em um processo industrial. Para cada instrumento foram obtidas leituras em diferentes níveis de pressão real, determinada por um método praticamente exato, porém lento e caro.* 

*O objetivo é verificar se os dois instrumentos podem compartilhar uma única curva de calibração ou se curvas distintas devem ser utilizadas."*

Realize uma análise completa dos dados, incluindo: (1) análise exploratória; (2) ajuste de modelos de regressão apropriados; (3) avaliação da necessidade de termos quadráticos; (4) comparação das curvas de calibração dos dois instrumentos; (5) testes de hipóteses para decidir se uma única curva pode ser usada; (6) análise de resíduos e diagnóstico do modelo final; (7) conclusão prática para a empresa.

## Etapa 0: Contextualizando

Cada instrumento foi exposto a **15 níveis de pressão real** (de 20 a 140, igualmente espaçados), e em cada nível foi registrada a **leitura** fornecida pelo instrumento. Temos, portanto, $2 \times 15 = 30$ observações.

* $X$: Pressão real (variável explicativa, mesma para os dois instrumentos)
* $Y$: Leitura do instrumento (variável resposta)
* Fator: Instrumento (Instrument 1 / Instrument 2)

A pergunta central é: **a relação entre Pressão e Leitura é a mesma para os dois instrumentos, ou cada um precisa de sua própria curva de calibração?**

Toda a análise será feita em **R**.

---

## Etapa 1: Análise Exploratória

#### 1.1 Estrutura dos dados

O conjunto possui **30 observações** (15 por instrumento), sem valores faltantes. A variável `Pressao` é a mesma grade de 15 valores para ambos os instrumentos (desenho balanceado), o que facilita bastante a comparação entre as curvas.

#### 1.2 Estatísticas descritivas

**Usando R (resumo dos dados):**

```
Instrumento    Min     Med      Média   desvio-padrão  Max  Cor(Pressao,Leitura)
Instrument 1  14.84    80       108.09    68.60       228.46  0.988
Instrument 2  25.76    91.62    110.98    71.66       249.32  0.977
```

Os dois instrumentos têm leituras médias e desvios-padrão muito próximos, e ambos apresentam **correlação muito forte e positiva** com a pressão real (acima de 0.97). Isso já sugere que os dois instrumentos respondem de forma parecida à pressão, mas a análise gráfica vai detalhar melhor.

---

#### 1.3 Análise gráfica

O primeiro passo visual é o **gráfico de dispersão** de Leitura vs Pressão, separando os dois instrumentos por cor/símbolo, e o **boxplot** das leituras por instrumento.

![Dispersão Leitura vs Pressão por instrumento](1_dispersao.png)

![BoxPlot](1_boxplot.png)

**Interpretação:**

* O gráfico de dispersão mostra que os pontos dos dois instrumentos ficam **muito próximos** ao longo de toda a faixa de pressão, sugerindo que ambos seguem aproximadamente a mesma tendência.
* A relação entre Pressão e Leitura **não parece perfeitamente linear**: há uma leve curvatura para cima nas pressões mais altas, o que é um indício de que um **termo quadrático** pode ser necessário (será investigado formalmente na Etapa 3).
* O boxplot mostra que a distribuição das leituras dos dois instrumentos é bastante semelhante em posição e dispersão, sem outliers evidentes.

---

## Etapa 2: Ajuste de Modelos de Regressão

Para responder se os instrumentos podem compartilhar uma única curva de calibração, vamos construir um **modelo completo** que permite, em princípio, **curvas completamente distintas** para cada instrumento (intercepto, termo linear e termo quadrático diferentes), e depois testar se essas diferenças são estatisticamente necessárias.

O modelo mais completo é:

$$Y_{ij} = \beta_0  + \beta_1 X_{ij} + \beta_2 X_{ij}^2 + \delta_0 D_i + \delta_1 (D_i X_{ij}) + \delta_2 (D_i X_{ij}^2) + \varepsilon_{ij}$$

onde $D_i$ é uma variável *dummy* que indica o Instrumento 2 ($D_i = 1$ para Instrument 2, $0$ para Instrument 1 ou seja o Intrument 1 é a referência), e $X_{ij}$ é a pressão real.

* Se $\delta_0 = \delta_1 = \delta_2 = 0$, os dois instrumentos compartilham a **mesma curva de calibração**.
* Caso contrário, são necessárias **curvas distintas**.

#### 2.1 Modelo linear simples (sem termo quadrático e dummy)

Antes de incluir o termo quadrático, vamos verificar como fica o ajuste puramente linear, para servir de comparação.

**Usando R (resumo do modelo linear sem termo quadrático):**
```
Call:
lm(formula = Leitura ~ Pressao, data = dados)
...
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -34.22308    5.79896  -5.902 2.39e-06 ***
Pressao       1.79693    0.06578  27.317  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 13.34 on 28 degrees of freedom
Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9625 
F-statistic: 746.2 on 1 and 28 DF,  p-value: < 2.2e-16
```
![Resíduos vs ajustados do modelo linear sem termo quadratico](2_dispersao_residuo_modelo_lin_uni.png)

**Resultado:** O modelo linear simples já apresenta $R^2 \approx 0{,}964$, ou seja, explica boa parte da variação. Porém, ao observar o gráfico de resíduos vs ajustados, percebe-se um **padrão em forma de "U"**, o que pode ser um sinal forte de **falta de um termo quadrático**.

---

## Etapa 3: Avaliação da Necessidade de Termos Quadráticos

Vamos comparar formalmente, via **teste F parcial**, o modelo linear simples com o modelo que inclui $Pressao^2$, primeiro usando **todos os dados juntos** (ignorando o instrumento) e depois **separadamente para cada instrumento**.

#### 3.1 Teste com todos os dados (curva única, ignorando o instrumento)

**Usando R (teste F parcial):**

```
Model 1: Leitura ~ Pressao
Model 2: Leitura ~ Pressao + Pressao2

  Res.Df    RSS     Df  Sum of Sq      F        Pr(>F)
1     28    4984.98
2     27    1031.73  1   3953.25       103.46   9.85e-11 ***
```

O valor-p é **extremamente pequeno** ($p \approx 9{,}85\times10^{-11}$), muito menor que 0.05. Portanto, **rejeitamos $H_0: \beta_2 = 0$**: o termo quadrático $Pressao^2$ é **altamente significativo** e deve ser incluído no modelo. O $R^2$ sobe de $0{,}964$ (linear) para $0{,}993$ (quadrático).

#### 3.2 Teste por instrumento (separadamente)

Para confirmar que essa necessidade de avaliar **dois instrumentos individualmente** (ter duas curvas em vez de uma), repetimos o teste F dentro de cada grupo.

**Usando R (teste F parcial só que para cada instrumento):**

```

<=== Instrument 1 ===>
    Res.Df      RSS         Df      Sum of Sq      F        Pr(>F)
1     13        1579.61
2     12        350.11      1       1229.50        42.14    3.0e-05 ***
R2 linear: 0.976 | R2 quadrático: 0.995

<=== Instrument 2 ===>
    Res.Df      RSS         Df      Sum of Sq      F        Pr(>F)
1     13        3308.47
2     12        408.19      1       2900.28        85.26   8.4e-07 ***
R2 linear: 0.954 | R2 quadrático: 0.994
```

**Conclusão da Etapa 3:** Para **ambos os instrumentos**, o termo quadrático é altamente significativo (valores-p muito menores que 0.05) e melhora substancialmente o $R^2$. A partir daqui, todos os modelos de comparação entre instrumentos devem incluir $Pressao$ **e** $Pressao^2$.

---
## Etapa 4: Comparação das Curvas de Calibração dos Dois Instrumentos

Agora vamos construir o **modelo completo** dito anteriormente, que permite intercepto, termo linear e termo quadrático **diferentes** para cada instrumento, usando uma variável *dummy* `Instrumento` e suas interações com `Pressao` e `Pressao2`.

#### 4.1 Modelo completo (curvas distintas)

**Usando R (resumo do modelo completo):**

```
Call:
lm(formula = Leitura ~ Pressao * Instrumento + Pressao2 * Instrumento, 
    data = dados)
Coefficients:
                                Estimate Std. Error t value Pr(>|t|)
(Intercept)                      4.0025     6.9150   0.579   0.5680
Instrumento(Instrument 2)       18.2980     9.7790   1.871   0.0735
Pressao                          0.5792     0.1946   2.977   0.0066 **
Pressao:Instrumento2            -0.5794     0.2752  -2.106   0.0457 *
Pressao2                         0.0074     0.0012   6.238   2.0e-06 ***
Pressao2:Instrumento2            0.0040     0.0017   2.364   0.0265 *

R-squared:  0.9937
```

Cada coeficiente "$:Instrumento2$" representa a **diferença** entre o Instrumento 2 e o Instrumento 1 (que é a categoria de referência). Note que, individualmente, alguns desses coeficientes de diferença têm valor-p < 0.05 (p.ex. `Pressao:Instrumento2` e `Pressao2:Instrumento2`), o que **isoladamente** sugeriria diferenças entre as curvas. Por isso é importante fazer um **teste F conjunto** (todos os três parâmetros de diferença ao mesmo tempo), em vez de olhar para cada teste $t$ separadamente.

#### 4.2 Modelo reduzido (uma única curva para os dois instrumentos)

**Usando R (resumo do modelo completo só que sem levar em conta os instrumentos):**

```
Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)   13.1516     5.3770   2.446   0.0211 *
Pressao        0.2895     0.1512   1.914   0.0660 .
Pressao2       0.0094     0.0009  10.171   8.2e-11 ***

R-squared:  0.9928
```

O $R^2$ do modelo único ($0{,}9928$) é apenas marginalmente menor do que o do modelo completo ($0{,}9937$), o que é um primeiro indício de que a diferença entre os instrumentos pode não ser relevante.

---
## Etapa 5: Teste de Hipóteses -- Uma Curva ou Duas Curvas?

#### 5.1 Teste F para o conjunto de parâmetros do instrumento

Queremos testar:

$$H_0: \delta_0 = \delta_1 = \delta_2 = 0 \quad \text{(uma única curva é suficiente)}$$
$$H_1: \text{pelo menos um } \delta_k \neq 0 \quad \text{(curvas distintas são necessárias)}$$

Isso é feito comparando o **modelo reduzido** (`modelo_unico`) com o **modelo completo** (`modelo_completo`) via `anova()`, que aqui equivale a um teste F parcial.

**Usando R (teste F parcial comparando os modelos):**

```
Model 1: Leitura ~ Pressao + Pressao2
Model 2: Leitura ~ Pressao * Instrumento + Pressao2 * Instrumento
    Res.Df      RSS     Df      Sum of Sq      F        Pr(>F)
1     27        1031.73
2     24        758.30  3        273.43        2.885   0.0566 .
```

O valor-p é $\approx 0{,}057$, ou seja, **ligeiramente maior que 0,05**. A um nível de significância de 5%, **não rejeitamos $H_0$**: não há evidência estatística suficiente para afirmar que os dois instrumentos precisam de curvas de calibração distintas. O resultado fica no limiar, e na conclusão vamos registrar isso com cautela.

#### 5.2 Teste intermediário (extras)

Para entender melhor de onde vem essa diferença marginal, e valor-p do teste acima deu próximo de 0.05, fica interessante (porém redundante) testar os modelos intermediários, em que ou apenas a curva ou apenas o **intercepto** muda entre instrumentos (curvas "paralelas" mas deslocadas verticalmente).


$$H_0: \delta_1 = \delta_2 = 0 \quad \text{(mesma curvatura)}$$
$$H_1: \text{pelo menos um } \delta_k \neq 0 \quad \text{(curvaturas diferentes)}$$

**Usando R (teste F parcial comparando os modelos):**
(modelo_intercepto é aquele que tem $\delta_0$ mas não tem $\delta_1$ e $\delta_2$)
```
anova(modelo_intercepto, modelo_completo)
  Res.Df    RSS Df Sum of Sq     F  Pr(>F)
1     26 969.06
2     24 758.30  2    210.76 3.335  0.0527 .
```

$$H_0: \delta_0 = 0 \quad \text{(mesmo intercepto)}$$
$$H_1: \delta_0 \neq 0 \quad \text{(intercepto diferente)}$$
(Já sabemos que a $H_0$ de que os $\delta_1$ e $\delta_2$ são 0 não foi rejeitada ao nível de 5%)
```
anova(modelo_unico, modelo_intercepto)
  Res.Df     RSS Df Sum of Sq      F Pr(>F)
1     27 1031.73
2     26  969.06  1     62.67  1.681  0.206
```

* A diferença nos **coeficientes de Pressão e Pressão²** **não é significativa** ($p \approx 0{,}053$), não atingindo 5% de significância.
* A diferença de **intercepto** entre os instrumentos também **não é significativa** ($p \approx 0{,}206$).

#### 5.3 Conclusão da Etapa 5

Combinando os três testes:

1. Curva única vs curvas completamente distintas: $p \approx 0{,}057$ (não rejeita $H_0$);
2. Diferença nos coeficientes de Pressão/Pressão² (dado intercepto comum): $p \approx 0{,}053$ (não rejeita $H_0$, diferença na curvatura não significativa);
3. Diferença apenas no intercepto: $p \approx 0{,}206$ (não rejeita $H_0$, diferença no intercepto não significativa);


**Em todos os casos, ao nível de significância usual de 5%, não há evidência estatisticamente suficiente para rejeitar a hipótese de que os dois instrumentos compartilham a mesma curva**, embora o valor-p do teste principal esteja muito próximo do limiar (0,0566), o que recomenda cautela e, se possível, a coleta de mais dados para reduzir essa incerteza.

Dito disso, seguimos a Etapa 6 (diagnóstico de resíduos) com o **modelo único** (`modelo_unico`), por ser o mais parcimonioso e não ter sido rejeitado pelos testes.

---

## Etapa 6: Análise de Resíduos e Diagnóstico do Modelo Final

O modelo final escolhido é:

$$\text{Leitura} = \beta_0 + \beta_1\,\text{Pressao} + \beta_2\,\text{Pressao}^2 + \varepsilon$$

ajustado com todos os 30 dados (`modelo_unico`). Vamos verificar os pressupostos clássicos do modelo de regressão: **linearidade**, **homocedasticidade**, **normalidade dos erros** e **independência**, além de procurar **outliers e pontos influentes**.

#### 6.1 Linearidade -- resíduos vs valores ajustados

![Resíduos vs Ajustados](6_resid_vs_ajustados.png)

**Interpretação:** Com o termo quadrático incluído, o padrão em "U" que aparecia no modelo puramente linear desaparece. Os resíduos se distribuem de forma razoavelmente aleatória em torno de zero.

#### 6.2 Homocedasticidade -- teste de Breusch-Pagan

**Usando R (teste de homocedasticidade):**

```
	studentized Breusch-Pagan test

data:  modelo_unico
BP = 6.1436, df = 2, p-value = 0.0463
```

O valor-p ($\approx 0{,}046$) é **menor que 0,05**, indicando que **a hipótese de homocedasticidade é levemente rejeitada** ao nível de 5%. Isso é coerente pois à medida que a pressão (e a leitura) aumenta, a variabilidade das leituras também aumenta, um comportamento bastante comum em instrumentos de medição, em que o erro cresce com a magnitude da grandeza medida. Para fins práticos, os erros-padrão dos coeficientes podem estar levemente subestimados. No entanto vamos prosseguir.

#### 6.3 Normalidade dos erros -- QQ-plot e teste de Shapiro-Wilk
![QQ-plot dos resíduos](6_qqplot.png)

**Usando R (teste de normalidade):**

```
	Shapiro-Wilk normality test

data:  residuos
W = 0.9730, p-value = 0.6227
```

* No QQ-plot, os pontos seguem razoavelmente bem a reta de referência, com pequenos desvios nas extremidades, esperados em amostras de tamanho $n=30$.
* O teste de Shapiro-Wilk tem $H_0$: os resíduos seguem distribuição normal. Com valor-p $\approx 0{,}62$ (bem maior que 0,05), **não rejeitamos $H_0$**: a suposição de normalidade dos erros é sustentável.

#### 6.4 Independência

Como o conjunto de dados não fornece informação sobre a ordem temporal de coleta, e os dados são organizados por nível de pressão crescente (provavelmente não é uma série temporal), assumimos que as observações são **independentes**.

#### 6.5 Outliers -- resíduos studentizados
![resíduos studentizado](6_residuos_studentizados.png)

**Usando R para detectar outlier com resíduos studentizados:**

```
   Pressao Leitura Instrumento Pressao2
30   140.0  249.32 Instrument 2  19600

      30
-2.16
```

A observação **30** (Instrumento 2, Pressão = 140, Leitura = 249.32 -- a maior pressão e a maior leitura do conjunto) apresenta resíduo studentizado $\approx -2{,}16$, ligeiramente acima do limiar usual de 2 em valor absoluto. É um **possível outlier**, mas não extremo (próximo do limite).

#### 6.6 Pontos de alta alavancagem ($h_{ii}$)

![gráfico de h_ii](6__hii.png)

**Usando R para ver pontos de alavancagem:**

```
limiar_h = 0.2 # 2p/n

Observações com h_ii > 0.2: as observações 1, 15, 16 e 30
(pressões extremas: 20 e 140, para os dois instrumentos)
h_ii ≈ 0.20 a 0.23
```

Como esperado em uma regressão polinomial, as observações nos **extremos da faixa de pressão** (20 e 140) têm alavancagem ligeiramente maior, pois influenciam mais a forma da curva nas pontas. Os valores ficam pouco acima do limiar $2p/n = 0{,}2$, mas não são extremamente altos.

#### 6.7 Distância de Cook -- influência global

![distancia de cook](6_DCook.png)

**Usando R para ver ponto influente com Distancia de Cook:**

```
limiar_cook ≈ 0.81
max(cook)   ≈ 0.41
which(cook > limiar_cook): nenhuma observação
```

Nenhuma observação tem distância de Cook acima do limiar formal ($F_{p,\,n-p}$ na mediana), embora a observação 30 (a mesma identificada como possível outlier) tenha a maior distância de Cook ($\approx 0{,}41$), aproximadamente metade do limiar. **Não há pontos com influência global excessiva**, mas a observação 30 merece atenção -- pode ser interessante reajustar o modelo sem ela como verificação de robustez.

#### 6.8 Verificação de robustez -- modelo sem a observação 30

```
	        Completo	    Sem_obs30
(Intercept)	13.15156737	    10.745287330
Pressao	    0.28954138	    0.383167155
Pressao2	0.00942118	    0.008705686
R2          0.99251493      0.992563128
```

**Interpretação**: O $R^2$ praticamente não se altera (0,9925 em ambos os casos), confirmando que a observação 30 não compromete o ajuste global do modelo. Já os coeficientes individuais apresentam mudanças mais notáveis: 

- o intercepto cai de 13,15 para 10,75 e 

- o coeficiente de Pressao sobe de 0,290 para 0,383, enquanto 

- o coeficiente Pressao2 varia pouco (de 0,00942 para 0,00871). 

Como a observação 30 está em uma região de alta alavancagem (extremo da faixa de pressão), é esprado que sua remoção afete mais os coeficientes individuais do que a qualidade geral do ajuste. 

Conclui-se que a observação 30, apesar de ser a de maior resíduo studentizado, não compromete as conclusões do modelo.


#### 6.9 Curva final

![Curva](6_9_curva.png)
A curva quadrática única descreve bem o comportamento dos dois instrumentos ao longo de toda a faixa de pressão estudada (20 a 140), com os pontos de ambos os instrumentos dispersos em torno da mesma curva, sem padrão sistemático de um instrumento ficar consistentemente acima ou abaixo do outro.

#### 6.10 Resumo do diagnóstico

| Itens | Verificação | Resultado |
|---|---|---|
| Linearidade | Resíduos vs Ajustados (após incluir Pressao²) | OK -- sem padrão sistemático claro |
| Homocedasticidade | Teste de Breusch-Pagan | $p \approx 0{,}046$ -- leve evidência de heterocedasticidade |
| Normalidade | Shapiro-Wilk | $p \approx 0{,}62$ -- OK |
| Independência | Desenho dos dados | Assumida, razoável |
| Outliers | Resíduos studentizados | Obs. 30 no limiar ($\approx -2{,}16$) |
| Pontos de alta alavancagem | $h_{ii}$ | Obs. nos extremos de pressão (1, 15, 16, 30), pouco acima do limiar |
| Influência global | Distância de Cook | Nenhuma observação acima do limiar formal |

O modelo quadrático único (sem considerar os instrumentos) é adequado. A principal ressalva é a **leve heterocedasticidade**, com variância dos erros crescendo possívelmente com o nível de pressão -- algo a se observar caso o modelo seja usado para pressões fora da faixa estudada (20 a 140).

## Etapa 7: Conclusão Prática para a Empresa

**1. Os dois instrumentos podem compartilhar a mesma curva de calibração.**

Os testes F formais (Etapa 5) não encontraram evidência estatisticamente significativa, ao nível de 5%, de que os dois instrumentos precisem de curvas de calibração distintas (teste principal: $p \approx 0{,}057$). Isso significa que, do ponto de vista estatístico, **um único conjunto de coeficientes de calibração** pode ser usado para converter a leitura de **qualquer um dos dois instrumentos** em pressão real, com erro semelhante.

**2. A relação Leitura x Pressão não é linear -- é necessário o termo quadrático.**

Tanto a análise gráfica quanto os testes F (Etapa 3) mostraram que a curva de calibração tem **curvatura** (a leitura cresce mais que proporcionalmente em relação à pressão real, especialmente em pressões mais altas). O modelo recomendado (com a observação 30) é:

$$\text{Leitura} = 13{,}15 + 0{,}29 \times \text{Pressao} + 0{,}0094 \times \text{Pressao}^2$$

(equivalentemente, invertendo essa relação, a empresa pode estimar a pressão real a partir da leitura observada).

**3. O modelo final é estatisticamente adequado**, com $R^2 \approx 0{,}993$, resíduos aproximadamente normais e sem pontos de influência, embora exista uma indicação leve de heterocedasticidade. 

**4. Recomendação prática:**

* Adotar **uma única curva de calibração quadrática** para os dois instrumentos, simplificando o processo de calibração e manutenção (menos curvas para documentar, calibrar e revalidar).
* Como o valor-p do teste de curvas distintas ($\approx 0{,}057$) ficou **muito próximo do limiar de 5%**, e há indício de heterocedasticidade, seria prudente que a empresa **coletasse leituras adicionais** -- principalmente em pressões mais altas -- para reforçar a confiança na decisão de usar uma curva única e, se necessário, refinar o modelo (por exemplo, considerando variância não constante).
* Em pressões próximas ao limite superior testado (140), recomenda-se atenção redobrada, pois é a região de maior incerteza (maior alavancagem e pode ter maior variabilidade residual).

---

## Apêndice: códigos usados

```
# Pressão real (mesma para os dois instrumentos)
Pressao <- c(20.00, 28.57, 37.14, 45.71, 54.29, 62.86, 71.43, 80.00,
              88.57, 97.14, 105.71, 114.29, 122.86, 131.43, 140.00)

# Leituras do Instrumento 1
Leitura1 <- c(14.84, 24.44, 43.96, 45.00, 56.49, 78.33, 84.30, 88.61,
               107.92, 126.39, 154.61, 168.79, 189.58, 209.57, 228.46)

# Leituras do Instrumento 2
Leitura2 <- c(31.72, 31.64, 25.76, 51.91, 56.27, 65.35, 84.35, 94.64,
               112.82, 131.09, 143.61, 178.90, 196.19, 211.08, 249.32)

# Montando o data frame "long" (uma observação por linha)
dados <- data.frame(
  Pressao     = c(Pressao, Pressao),
  Leitura     = c(Leitura1, Leitura2),
  Instrumento = factor(rep(c("Instrument 1", "Instrument 2"), each = length(Pressao)))
)

str(dados)
head(dados)
tail(dados)


# Resumo descritivo geral
summary(dados)

# Resumo por instrumento
aggregate(Leitura ~ Instrumento, data = dados, summary)

# Desvio-padrão por instrumento
aggregate(Leitura ~ Instrumento, data = dados, sd)

# Correlação Pressao x Leitura, por instrumento
by(dados, dados$Instrumento, function(d) cor(d$Pressao, d$Leitura))

# Gráfico de dispersão: Leitura vs Pressão, por instrumento
plot(dados$Pressao, dados$Leitura,
     col = ifelse(dados$Instrumento == "Instrument 1", "blue", "red"),
     pch = ifelse(dados$Instrumento == "Instrument 1", 16, 17),
     xlab = "Pressão real", ylab = "Leitura",
     main = "Leitura vs Pressão por instrumento")
legend("topleft", legend = levels(dados$Instrumento),
       col = c("blue", "red"), pch = c(16, 17))

# Boxplot da Leitura por instrumento
#boxplot(Leitura ~ Instrumento, data = dados,
#        main = "Boxplot da Leitura por Instrumento",
#        ylab = "Leitura")

# Modelo linear simples, ignorando o instrumento (curva única, sem quadrático)
modelo_lin_unico <- lm(Leitura ~ Pressao, data = dados)
summary(modelo_lin_unico)

# Resíduos vs ajustados do modelo linear simples
plot(fitted(modelo_lin_unico), residuals(modelo_lin_unico),
     xlab = "Valores ajustados", ylab = "Resíduos",
     main = "Resíduos vs Ajustados - modelo linear (sem quadrático)")
abline(h = 0, lty = 2, col = "gray")

# Criando a variável Pressao^2
dados$Pressao2 <- dados$Pressao^2

# Modelo linear (reduzido) e modelo quadrático (completo), ignorando instrumento
modelo_lin   <- lm(Leitura ~ Pressao, data = dados)
modelo_quad  <- lm(Leitura ~ Pressao + Pressao2, data = dados)

summary(modelo_quad)

# Teste F comparando modelo linear vs modelo quadrático
anova(modelo_lin, modelo_quad)
# Ajustando linear e quadrático separadamente para cada instrumento
for (inst in levels(dados$Instrumento)) {
  cat("\n===", inst, "===\n")
  sub <- dados[dados$Instrumento == inst, ]
  
  m_lin  <- lm(Leitura ~ Pressao, data = sub)
  m_quad <- lm(Leitura ~ Pressao + Pressao2, data = sub)
  
  print(anova(m_lin, m_quad))
  cat("R2 linear:", summary(m_lin)$r.squared,
      " | R2 quadrático:", summary(m_quad)$r.squared, "\n")
}

# Modelo completo: tem curva diferente para cada instrumento
# (intercepto, coef. linear e coef. quadrático podem variar com o Instrumento)
modelo_completo <- lm(Leitura ~ Pressao * Instrumento + Pressao2 * Instrumento,
                       data = dados)
summary(modelo_completo)

# Modelo reduzido: ignora o instrumento, uma única curva quadrática para todos os dados
modelo_unico <- lm(Leitura ~ Pressao + Pressao2, data = dados)
summary(modelo_unico)

# Teste F: modelo único vs modelo completo (curvas distintas)
anova(modelo_unico, modelo_completo)

# Modelo intermediário: mesmo formato de curva, intercepto diferente por instrumento
modelo_intercepto <- lm(Leitura ~ Pressao + Pressao2 + Instrumento, data = dados)
summary(modelo_intercepto)

# Teste F: modelo único vs modelo com intercepto diferente
anova(modelo_unico, modelo_intercepto)

# Teste F: modelo com intercepto diferente vs modelo completo
anova(modelo_intercepto, modelo_completo)


#modelo_curva <- lm(Leitura ~ Pressao + Pressao2 + Pressao:Instrumento + Pressao2:Instrumento, data = dados)

#anova(modelo_curva, modelo_completo)

#summary(modelo_curva)

residuos  <- residuals(modelo_unico)
ajustados <- fitted(modelo_unico)

plot(ajustados, residuos,
     xlab = "Valores ajustados", ylab = "Resíduos",
     main = "Resíduos vs Ajustados (Linearidade)")
abline(h = 0, lty = 2, col = "gray")

library(lmtest)
bptest(modelo_unico)

qqnorm(residuos, main = "QQ-plot dos Resíduos")
qqline(residuos, col = "red")

shapiro.test(residuos)

# Resíduos studentizados (jackknife)
res_student <- rstudent(modelo_unico)

plot(ajustados, res_student,
     xlab = "Valores ajustados", ylab = "Resíduos studentizados",
     main = "Resíduos studentizados vs Ajustados")
abline(h = c(-2, 0, 2), lty = c(2, 1, 2), col = c("red", "gray", "red"))

# Observações com |resíduo studentizado| > 2
outliers <- which(abs(res_student) > 2)
dados[outliers, ]
res_student[outliers]

h <- hatvalues(modelo_unico)
p <- length(coef(modelo_unico))   # número de parâmetros (intercepto + 2 coeficientes)
n <- nrow(dados)
limiar_h <- 2 * p / n             # regra 2p/n

plot(h, type = "h", col = ifelse(h > limiar_h, "red", "black"),
     main = "Alavancagem (h_ii)", ylab = "h_ii", xlab = "Observação")
abline(h = limiar_h, lty = 2, col = "blue")

limiar_h
which(h > limiar_h)
h[h > limiar_h]

cook <- cooks.distance(modelo_unico)
limiar_cook <- qf(0.5, p, n - p)   # mediana da F(p, n-p)

plot(cook, type = "h", col = ifelse(cook > limiar_cook, "red", "black"),
     main = "Distância de Cook", ylab = "Cook's D", xlab = "Observação")
abline(h = limiar_cook, lty = 2, col = "blue")

limiar_cook
max(cook)
which(cook > limiar_cook)

# Reajustar o modelo sem a observação 30 (maior pressão do Instrumento 2)
modelo_sem30 <- lm(Leitura ~ Pressao + Pressao2, data = dados[-30, ])

# Comparar coeficientes
cbind(
  Completo = coef(modelo_unico),
  Sem_obs30 = coef(modelo_sem30)
)

# Comparar R^2
c(R2_completo = summary(modelo_unico)$r.squared,
  R2_sem30    = summary(modelo_sem30)$r.squared)

# Gráfico final: dados + curva de calibração ajustada
plot(dados$Pressao, dados$Leitura,
     col = ifelse(dados$Instrumento == "Instrument 1", "blue", "red"),
     pch = ifelse(dados$Instrumento == "Instrument 1", 16, 17),
     xlab = "Pressão real", ylab = "Leitura",
     main = "Curva de calibração única (modelo quadrático)")
legend("topleft", legend = levels(dados$Instrumento),
       col = c("blue", "red"), pch = c(16, 17))

# Curva ajustada
nova_pressao <- seq(min(dados$Pressao), max(dados$Pressao), length.out = 200)
pred <- predict(modelo_unico, newdata = data.frame(Pressao = nova_pressao,
                                                     Pressao2 = nova_pressao^2))
lines(nova_pressao, pred, col = "black", lwd = 2)
```