################################################################################
################################################################################
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
################################################################################
################################################################################
# Dados - Importação e Preparação 
dados <- read_delim("ipeadata[04-02-2026-07-46].csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Alterando nome das colunas 
serie <- dados %>% 
  select(-...3) %>% 
  rename(Data = Data,
         Fluxo_De_Caixa = "Previdência Social - fluxo de caixa da previdência - arrecadação líquida - R$ - Ministério da Previdência Social, Boletim Estatístico da Previdência Social (MPS) - MPAS12_ARRLIQ12")

################################################################################
################################################################################
################################################################################
# Transformando em um objeto de série temporal (ts)
# Considerando que os dados são mensais e começam em Fev/2003
serie <- ts(serie$Fluxo_De_Caixa, 
                  start = c(2003, 2), 
                  frequency = 12)

autoplot(serie) +
  ggtitle("Arrecadação Líquida da Previdência Social") +
  xlab("Ano") + ylab("R$ (mil)")
################################################################################
################################################################################
################################################################################
# Ajustar o modelo ARIMA
modelo <- auto.arima(serie)

# Fazer a previsão para os próximos 3 meses
previsao <- forecast(modelo, h = 4)

# Visualizar
plot(previsao, main = "Previsão de Fluxo de Caixa", 
     xlab = "Tempo", ylab = "Valor")

# É perceptivel que há um mudança no comportamento da série no decorrer dos anos
# neste caso, será feito um corte, pegando os ultimos 10 anos. 

################################################################################
################################################################################
################################################################################
# Visualizar a estrutura
head(serie)
str(serie)

# Primeiro vamos transformar o formato decimal em data regular
serie_fluxo <- serie %>%
  mutate(
    # Extrai ano da parte inteira
    ano = floor(Data),
    # Extrai mês da parte decimal (multiplica por 100 para obter mês como inteiro)
    mes = round((Data - ano) * 100),
    # Cria data no formato ano-mês
    Data_ts = yearmonth(paste(ano, mes, sep = "-"))
  ) %>%
  # Seleciona e renomeia colunas
  select(date = Data_ts, Fluxo_De_Caixa) %>%
  # Ordena por data
  arrange(date)

# Converte para tsibble
serie_fluxo_ts <- as_tsibble(serie_fluxo, index = date)

# Verifica a estrutura
print(serie_fluxo_ts)

################################################################################
################################################################################
################################################################################
## Decomposição - Aditiva 
# Modelo Simples, com decomposição clássica
modelo_decomposicao <- serie_fluxo_ts %>%
  model(classical_decomposition(Fluxo_De_Caixa, type = "additive"))

# Extrai os componentes (decomposição)
componentes <- components(modelo_decomposicao)

# Plot
autoplot(componentes) +
  labs(title = "Decomposição clássica aditiva do total de arrecadação")

################################################################################
################################################################################
################################################################################
# Decomposição - Multiplicativa 
serie_fluxo_ts %>%
  model(
    classical_decomposition(Fluxo_De_Caixa, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição classica aditiva do total de arrecadação liquida")

################################################################################
################################################################################
################################################################################
#### Forecast ---- 
## A Série inicia em 2003.02 e vai até 2025.06

# Definindo a base de dados em treinamento e teste
train <- serie_fluxo_ts %>%
  filter_index("2003-02" ~ "2015-06")

test <- serie_fluxo_ts %>%
  filter_index("2015-06" ~ .);test

### verificando se esta correto
head(test)
tail(test)

################################################################################
################################################################################
################################################################################
# Estimando o modelo de suavizacao
Fluxo_Prev_fit <- train %>%
  model(
    # SES (Suavização Exponencial Simples)
    SES = ETS(Fluxo_De_Caixa ~ error("A") + trend("N") + season("N")),
    
    # Holt aditivo
    HoltA = ETS(Fluxo_De_Caixa ~ error("A") + trend("A") + season("N")),
    
    # Holt multiplicativo
    HoltM = ETS(Fluxo_De_Caixa ~ error("M") + trend("A") + season("N")),
    
    # Holt-Winters aditivo
    HW_Add = ETS(Fluxo_De_Caixa ~ error("A") + trend("A") + season("A")),
    
    # Holt-Winters multiplicativo
    HW_Mult = ETS(Fluxo_De_Caixa ~ error("M") + trend("A") + season("M"))
  )
Fluxo_Prev_fit


# GERANDO UMA PREVISAO h = 4 passos 
Fluxo_Prev_fc <- Fluxo_Prev_fit %>% forecast(h = 4)
Fluxo_Prev_fc

# Grafico da serie com as previsoes 
Fluxo_Prev_fc %>%
  autoplot(train, level = NULL) +
  autolayer(
    test,
    colour = "black"
  ) +
  labs(
    y = "Quantidade arrecada - Previdência Social",
    title = "Previsão de arrecadação trimestrais para previdência em 2026"
  ) +
  guides(colour = guide_legend(title = "Forecast"))