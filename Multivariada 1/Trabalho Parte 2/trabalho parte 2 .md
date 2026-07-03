### *(a) uma breve descrição do contexto da base de dados;*

A base de dados referente ao trabalho é um arquivo .csv cujo nome é "mindfullness_trabalho" disponibilizado pelo professor.

O arquivo contém 120 observações (uma por trabalhador), e para cada observação são registradas, além do identificador do funcionário, 4 variáveis quantitativas: "estresse_antes_mindfulness", "estresse_depois_mindfulness", "bem_estar_antes_mindfulness" e "bem_estar_depois_mindfulness".

Trata-se de um estudo **pareado**: cada trabalhador é medido duas vezes (antes e depois de participar do programa de mindfulness) em relação a duas grandezas — o nível de estresse percebido e o nível de bem-estar. Assim, Tratamento 1 = antes do programa e Tratamento 2 = depois do programa.

Iremos usar ferramentas estatísticas multivariadas (vetor de médias, matriz de covariâncias/correlações e, posteriormente, teste de hipótese para dados pareados) para avaliar se o programa de mindfulness teve efeito sobre o estresse e o bem-estar dos trabalhadores.

*Não temos informação sobre a unidade de medida exata das escalas de estresse e bem-estar; assume-se que sejam escores de um instrumento (questionário) aplicado à população de trabalhadores da empresa/contexto do estudo.*

### *(b) identificação das variáveis quantitativas que serão utilizadas na análise e o(s) tratamento(s) considerados;*

As variáveis quantitativas originais são:

- "estresse_antes_mindfulness" e "estresse_depois_mindfulness";
- "bem_estar_antes_mindfulness" e "bem_estar_depois_mindfulness".

Por conveniência, vamos chamar:

$X_{11}$: Nível de estresse no Tratamento 1 (antes do mindfulness)

$X_{12}$: Nível de estresse no Tratamento 2 (depois do mindfulness)

$X_{21}$: Nível de bem-estar no Tratamento 1 (antes do mindfulness)

$X_{22}$: Nível de bem-estar no Tratamento 2 (depois do mindfulness)

Como o delineamento é **pareado** (mesmo indivíduo nas duas condições), a análise não deve ser feita sobre os 4 valores separadamente, e sim sobre o vetor de diferenças por indivíduo:

$$d_{1j} = X_{12j} - X_{11j}  \quad \text{(variação de estresse do j-ésimo trabalhador)}$$
$$d_{2j} = X_{22j} - X_{21j} \quad \text{(variação de bem-estar do j-ésimo trabalhador)}$$

de modo que $\boldsymbol{d}_j = (d_{1j}, d_{2j})'$, $j = 1, \dots, 120$, é o vetor de diferenças (Tratamento 2 − Tratamento 1) que será utilizado em toda a análise a seguir.

### *(c) cálculo e interpretação do vetor de médias amostrais $\bar{\boldsymbol{d}}$;*

```
d_estresse-4.21475     d_bem_estar7.41441666666667
```

Interpretação:

- d_barra_1 (estresse) = -4.215 -> em média, a diferença do estresse DEPOIS do programa comparando com o estresse ANTES é -4.215 pontos, ou seja, o mindfulness esteve associado, em média, a uma REDUÇÃO de -4.215 pontos no estresse percebido.

- d_barra_2 (bem-estar) = 7.414 -> em média, o bem-estar DEPOIS do programa é 7.414 pontos MAIOR do que o bem-estar ANTES, ou seja, o mindfulness esteve associado, em média, a uma MELHORA de 7.414 pontos no bem-estar.

### *(d) cálculo e interpretação da matriz de variâncias e covariâncias amostral $\boldsymbol{S}_d$;*

```
	        d_estresse	d_bem_estar
d_estresse	23.4097848	-0.9346822
d_bem_estar	-0.9346822	92.1194602
```


Interpretação:

- Variância de d_estresse = 23.410 (desvio-padrão = 4.838); 

- Variância de d_bem_estar = 92.119 (desvio-padrão = 9.598);

- Covariância entre d_estresse e d_bem_estar = -0.935. O sinal negativo indica que, entre os trabalhadores, uma maior redução de estresse TENDE a vir acompanhada de uma melhora no bem-estar.

Como d_estresse e d_bem_estar são medidos em escalas diferentes, as variâncias não são diretamente comparáveis entre si; para isso, deve-se olhar a matriz de correlações no item (e).

### *(e) cálculo e interpretação da matriz de correlações amostral $\boldsymbol{P}_d$;*


```
            d_estresse	d_bem_estar
d_estresse	1.00000000	-0.02012749
d_bem_estar	-0.02012749	1.00000000
```
Com base na amostra de 120 trabalhadores, a correlação linear amostral entre d_estresse e d_bem_estar é de -0.020, o que sugere uma relação linear negativa fraca entre a redução de estresse e a variação de bem-estar provocadas pelo programa de mindfulness.

### *(f) construção de gráficos adequados para visualizar o comportamento individual e conjunto das variáveis.*

boxplots e dispersão