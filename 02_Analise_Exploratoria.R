#Analise_Exploratoria

summary(serie)
sd(serie)

# Sazonalidade
ggseasonplot(serie)
ggsubseriesplot(serie)

# Decomposição
decomposicao <- decompose(serie, type="multiplicative")
autoplot(decomposicao)
