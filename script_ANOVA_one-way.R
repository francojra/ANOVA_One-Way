
# ANOVA One-Way ----------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 22/04/25 ---------------------------------------------------------------------------------------------------------------------------

# Introdução -------------------------------------------------------------------------------------------------------------------------------

# O que fazemos para ver diferenças entre três ou mais grupos?: ----------------------------------------------------------------------------

## O que é ANOVA One-Way?

## A ANOVA (Análise de Variância) de uma via é um teste estatístico 
## utilizado para comparar as médias de três ou mais grupos 
## independentes, determinando se há evidências de que pelo menos 
## uma das médias difere significativamente das demais. É uma 
## extensão do teste t para amostras independentes.

## Hipóteses:

## H0: as médias de todos os grupos são iguais;
## H1: pelo menos uma média é diferente.

## Pressupostos:

## 1 - Normalidade dos resíduos.
## 2 - Homogeneidade de variâncias (homocedasticidade).
## 3 - Independência das observações.

# Exemplo prático com o dataset PlantGrowth ------------------------------------------------------------------------------------------------

## Carregar pacotes

library(tidyverse) # Manipulação e visualização de dados

## Carregando e explorando os dados:

# Carregar dados

data(PlantGrowth)
head(PlantGrowth)

# Resumo estatístico

summary(PlantGrowth)

# Visualizar estrutura

glimpse(PlantGrowth)

## Visualização inicial

ggplot(PlantGrowth, aes(x = group, 
                        y = weight, 
                        fill = group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightgreen", 
                               "salmon", 
                               "lightblue"),
                    name = "Grupos",
                    breaks = c("ctrl", "trt1", "trt2"),
                    labels = c("Controle", "Tratamento 1", "Tratamento 2")) +
  scale_x_discrete(labels = c("Controle", "Tratamento 1", "Tratamento 2")) +
  labs(x = "Grupo de Tratamento",
       y = "Peso (g)",
       title = "Peso das Plantas por Grupo") +
  theme_minimal()

# Verificando pressupostos -----------------------------------------------------------------------------------------------------------------

## 1. Normalidade (teste de Shapiro-Wilk por grupo):

tapply(PlantGrowth$weight, PlantGrowth$group, shapiro.test)

## 2. Homogeneidade de Variâncias (teste de Levene ou Bartlett):

car::leveneTest(weight ~ group, data = PlantGrowth)
# Ou
bartlett.test(weight ~ group, data = PlantGrowth)

# Realizando a ANOVA -----------------------------------------------------------------------------------------------------------------------

modelo_anova <- aov(weight ~ group, data = PlantGrowth)
summary(modelo_anova)

## Teste Post-Hoc (se a ANOVA for significativa)

TukeyHSD(modelo_anova)
# Ou usando o pacote 'emmeans'
emmeans::emmeans(modelo_anova, pairwise ~ group)

# Interpretação resultados -----------------------------------------------------------------------------------------------------------------

## Se o p-valor da ANOVA for < 0.05, rejeitamos H0.
## O teste post-hoc identifica quais grupos diferem entre si.

# Conclusão --------------------------------------------------------------------------------------------------------------------------------

## A ANOVA one-way é uma ferramenta poderosa para comparar 
## médias entre grupos. Neste exemplo, vimos como aplicá-la 
## no R, verificar seus pressupostos e interpretar os 
## resultados. Lembre-se de que os pressupostos devem ser 
## atendidos para que a análise seja válida!

## Dica: Para dados não paramétricos, considere o teste de 
## Kruskal-Wallis (kruskal.test()).

