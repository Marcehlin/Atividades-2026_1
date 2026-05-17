# ============================================================
#  ETAPA 1 — ANÁLISE EXPLORATÓRIA DOS DADOS (Concreto)
# ============================================================

library(readxl)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)

# ── 1. LEITURA E RENOMEAÇÃO ──────────────────────────────────
dados_brutos <- read_excel("C:/Users/Marcelo/Documents/Atividades 2026_1/Regressao/Regressao Trabalho/Concrete_Data.xls")

colnames(dados_brutos) <- c("Cimento", "Escoria", "CinzaVolante", "Agua",
                            "Superplastificante", "AgregadoGraudo",
                            "AgregadoMiudo", "Idade", "Resistencia")

dados <- as.data.frame(dados_brutos)

# ── 2. VISÃO GERAL ───────────────────────────────────────────
cat("=== DIMENSÕES ===\n")
cat("Observações:", nrow(dados), "| Variáveis:", ncol(dados), "\n\n")

cat("=== ESTATÍSTICAS DESCRITIVAS ===\n")
print(summary(dados))

cat("\n=== VALORES AUSENTES ===\n")
print(colSums(is.na(dados)))

cat("\n=== COEFICIENTE DE VARIAÇÃO (%) ===\n")
cv <- sapply(dados, function(x) round(sd(x) / mean(x) * 100, 2))
print(cv)

# ── 3. BOXPLOTS ──────────────────────────────────────────────
# Normaliza os dados para colocar tudo na mesma escala visual
dados_long <- reshape2::melt(dados, variable.name = "Variavel",
                             value.name = "Valor")

# Boxplot geral (todas as variáveis juntas — escala livre)
p_box_all <- ggplot(dados_long, aes(x = Variavel, y = Valor,
                                    fill = Variavel)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1.5,
               alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Variavel, scales = "free", ncol = 3) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(title = "Boxplots — Todas as Variáveis (escala livre)",
       x = NULL, y = "Valor")

print(p_box_all)

# ── 4. MATRIZ DE CORRELAÇÃO ──────────────────────────────────
cor_matrix <- cor(dados, method = "pearson")

cat("\n=== CORRELAÇÕES COM A VARIÁVEL RESPOSTA (Resistencia) ===\n")
cor_resp <- sort(cor_matrix[, "Resistencia"], decreasing = TRUE)
print(round(cor_resp, 4))

# Heatmap da matriz de correlação
cor_melt <- reshape2::melt(cor_matrix)

p_cor <- ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3.2, color = "black") +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9850",
                       midpoint = 0, limits = c(-1, 1),
                       name = "Pearson\nr") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(face = "bold")) +
  labs(title = "Matriz de Correlação de Pearson",
       x = NULL, y = NULL)

print(p_cor)

# ── 5. HISTOGRAMAS + DENSIDADE ───────────────────────────────
vars <- colnames(dados)
plots_hist <- lapply(vars, function(v) {
  ggplot(dados, aes_string(x = v)) +
    geom_histogram(aes(y = ..density..), bins = 30,
                   fill = "#4575b4", color = "white", alpha = 0.8) +
    geom_density(color = "#d73027", linewidth = 1) +
    theme_bw(base_size = 10) +
    labs(title = v, x = NULL, y = "Densidade")
})
grid.arrange(grobs = plots_hist, ncol = 3,
             top = "Distribuição das Variáveis")

# ── 6. DISPERSÃO: PREDITORAS × RESISTÊNCIA ───────────────────
preditoras <- setdiff(colnames(dados), "Resistencia")

plots_disp <- lapply(preditoras, function(v) {
  ggplot(dados, aes_string(x = v, y = "Resistencia")) +
    geom_point(alpha = 0.35, color = "#4575b4", size = 1.5) +
    geom_smooth(method = "loess", se = TRUE,
                color = "#d73027", linewidth = 1) +
    theme_bw(base_size = 10) +
    labs(title = paste(v, "× Resistência"),
         x = v, y = "Resistência (MPa)")
})
grid.arrange(grobs = plots_disp, ncol = 4,
             top = "Dispersão: Variáveis Preditoras × Resistência")


# ── 7. GRÁFICO MULTIVARIADO (Pairs Plot) ─────────────────────
# install.packages("GGally")  # rode apenas uma vez se necessário
library(GGally)

# Cria uma coluna para colorir por faixa de Resistência
dados$FaixaResistencia <- cut(dados$Resistencia,
                              breaks = quantile(dados$Resistencia,
                                                probs = c(0, 0.33, 0.66, 1)),
                              labels = c("Baixa", "Média", "Alta"),
                              include.lowest = TRUE)

# Pairs plot completo com correlações, dispersões e densidades
p_pairs <- ggpairs(
  dados,
  columns      = 1:9,           # apenas as variáveis numéricas
  aes(color = FaixaResistencia, alpha = 0.4),
  upper = list(continuous = wrap("cor", size = 3)),          # correlação no triângulo superior
  lower = list(continuous = wrap("points", size = 0.6)),     # dispersão no inferior
  diag  = list(continuous = wrap("densityDiag", alpha = 0.5)), # densidade na diagonal
  columnLabels = c("Cimento", "Escoria", "CinzaVolante", "Agua",
                   "Superplast.", "Agr.Graudo", "Agr.Miudo",
                   "Idade", "Resistencia")
) +
  scale_color_manual(values = c("Baixa" = "#d73027",
                                "Média" = "#fee08b",
                                "Alta"  = "#1a9850"),
                     name = "Resistência") +
  scale_fill_manual(values = c("Baixa" = "#d73027",
                               "Média" = "#fee08b",
                               "Alta"  = "#1a9850"),
                    name = "Resistência") +
  theme_bw(base_size = 9) +
  theme(strip.text = element_text(size = 7, face = "bold"),
        legend.position = "bottom") +
  labs(title = "Gráfico Multivariado — Pares de Variáveis",
       subtitle = "Colorido por faixa de Resistência (tercis)")

print(p_pairs)

# Remove a coluna auxiliar ao final
dados$FaixaResistencia <- NULL
