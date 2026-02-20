# Previsao
library(forecast)
previsao_final <- forecast(modelo_ets, h=12)

autoplot(previsao_final) +
  autolayer(teste, series="Observado") +
  ggtitle("Previsão da Arrecadação Líquida") +
  xlab("Ano") + ylab("R$ (mil)")
