################################################################################
# MODELAGEM 

################################################################################
# Bibliotecas - Séries Temporais 
library(readr)
library(forecast)
library(dplyr)
library(tidyverse)
library(fable)
library(tsibble)
library(feasts)


################################################################################
# Divisão - Treino e Teste
treino <- window(serie, end=c(2023,12))
teste  <- window(serie, start=c(2024,1))

# Modelo de Holt 
modelo_holt <- holt(treino)

# Modelo de Holt-Winters Aditivo 
modelo_hw_add <- hw(treino, seasonal="additive")

# Modelo de Holt-Winters Multiplicativo
modelo_hw_mult <- hw(treino, seasonal="multiplicative") 

# ETS 
modelo_ets <- ets(treino)
summary(modelo_ets)

# Comparação dos Modelos 
accuracy(modelo_holt, teste)
accuracy(modelo_hw_add, teste)
accuracy(modelo_hw_mult, teste)
accuracy(modelo_ets, teste)
accuracy(previsao, teste)

previsao <- forecast(modelo_ets, h = length(teste)) # para resolver o problema de reconhecer o objeto como ts

