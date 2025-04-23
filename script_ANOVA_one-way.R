
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
       title = "Peso das Plantas por Grupo")
