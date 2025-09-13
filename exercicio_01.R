##################################################################################
##################################################################################
##################################################################################

# Pacotes

install.packages("fpp3")
install.packages("TSA")
install.packages("forecast")
## Bibliotecas de séries temporais
library(TSA)
library(forecast)
library(fpp3)
library(readr)
library(ggplot2)
library(dplyr)

##################################################################################
##################################################################################
##################################################################################


# Carregando dados
df <- read_csv("DailyDelhiClimateTrain.csv")
head(df)
view(df)

##################################################################################
##################################################################################
##################################################################################

## Transformando a variavel Sales no formato TS e Plotando seus respectivos Gráficos

# Gráfico da Variável Meantemp
y_01 = ts( df$meantemp, start = 2013,end = 2017,  frequency = 4  )

plot.ts( y_01  )

# Gráfico da Variável Humidity 
y_02 = ts( df$humidity, start = 2013,end = 2017,  frequency = 4  )

plot.ts( y_02  )

# Gráfico da Variável Humidity 
y_03 = ts( df$wind_speed, start = 2013,end = 2017,  frequency = 4  )

plot.ts( y_03  )

# Gráfico da Variável Humidity 
y_04 = ts( df$meanpressure, start = 2013,end = 2017,  frequency = 4  )

plot.ts( y_04  )

##################################################################################
##################################################################################
##################################################################################

### Criando a variável Quarter (variável que indica trimestre)
serie <- df %>% 
  mutate(Quarter = yearmonth(date)) %>% 
  as_tsibble(index = Quarter)
serie <- as_tsibble(serie, index =Quarter)

view(serie)
duplicates(serie$Quarter)

# Plot das series - exclindo ambas as colunas de data
serie %>%
  pivot_longer(cols = -c(date, Quarter),  # colunas temporais
               names_to = "variable", 
               values_to = "value") %>%
  ggplot(aes(x = Quarter, y = value, colour = variable)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free_y") +
  labs(title = "Séries Temporais", x = "Trimestre", y = "Valor") +
  theme_minimal()

head(serie)


##################################################################################
##################################################################################
##################################################################################

### Estudando a sazonalidade

serie %>%
  gg_season(Quarter, labels = "both") +
  labs(y = "Tempo Médio",
       title = "Sazonalidade: Meantemp")
# Método recomendado: agregar por média
serie <- df %>% 
  mutate(Quarter = yearmonth(date)) %>%
  group_by(Quarter) %>%
  summarise(
    meantemp = mean(meantemp, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE),
    meanpressure = mean(meanpressure, na.rm = TRUE)
  ) %>%
  as_tsibble(index = Quarter)

# Agora o gráfico deve funcionar
serie %>%
  gg_season(meantemp, labels = "both") +
  labs(y = "Tempo Médio",
       title = "Sazonalidade: Meantemp")  

serie %>% 
  gg_season(humidity,labels = "both") +
  


