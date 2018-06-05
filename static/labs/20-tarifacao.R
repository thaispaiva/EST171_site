## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 20 - Tarifação

require(CASdatasets)

## Dados de seguros contra terceiros - França
data(freMTPLfreq)
?freMTPLfreq

## categorizando variáveis
freMTPLfreq.f <- freMTPLfreq
freMTPLfreq.f$DriverAge <- cut(freMTPLfreq$DriverAge,c(17,22,26,42,74,Inf))
freMTPLfreq.f$CarAge <- cut(freMTPLfreq$CarAge,c(0,1,4,15,Inf),include.lowest=TRUE)
freMTPLfreq.f$Density <- cut(freMTPLfreq$Density,c(0,40,200,500,4500,Inf),include.lowest=TRUE)

## verificando as classes de cada variável
summary(freMTPLfreq.f)

## Modelo Poisson 1
##

## Modelos de tarifação
## com uma variável categórica

vY <- freMTPLfreq.f$ClaimNb   # selecionando Y
vE <- freMTPLfreq.f$Exposure  # selecionando E
X1 <- freMTPLfreq.f$Gas       # selecionando X1

## Frequência de indenizações por tipo de combustível
tapply(vY, X1, sum)/tapply(vE, X1, sum)

## modelo sem intercepto
df <- data.frame(vY,vE,X1)
regpoislog <- glm(vY~0+X1+offset(log(vE)),data=df,family=poisson(link="log"))
summary(regpoislog)

## exponencial dos coeficientes
##

## modelo com intercepto
regpoislog <- glm(vY~X1+offset(log(vE)),data=df,family=poisson(link="log"))
summary(regpoislog)

## exponencial dos coeficientes
##

## Modelo com covariável contínua

## modelo com idade categórica
##

## modelo com idade contínua
##

