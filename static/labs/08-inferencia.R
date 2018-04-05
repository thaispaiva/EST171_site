## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 08 - Inferência

## Pacotes
# install.packages("fitdistrplus")  # instalar esse primeiro
# install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/R/", type="source")
require(CASdatasets)
require(fitdistrplus)

## dados seguro contra terceiros Itália
data("itamtplcost")

x = itamtplcost$UltimateCost/10^6
summary(x)
## EMV
fgamEMV = fitdist(x, "gamma", method="mle")
fgamEMV

summary(fgamEMV)

plot(fgamEMV)

