## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 21 - Tarifação (parte 2)

require(CASdatasets)

## carregando os dados da aula passada:

## dados de seguros contra terceiros - frequencia
data(freMTPLfreq)

## categorizando variáveis
freMTPLfreq.f <- freMTPLfreq
freMTPLfreq.f$DriverAge <- cut(freMTPLfreq$DriverAge,c(17,22,26,42,74,Inf))
freMTPLfreq.f$CarAge <- cut(freMTPLfreq$CarAge,c(0,1,4,15,Inf),include.lowest=TRUE)
freMTPLfreq.f$Density <- cut(freMTPLfreq$Density,c(0,40,200,500,4500,Inf),include.lowest=TRUE)


## carregando os dados de severidade
data(freMTPLsev)
tail(freMTPLsev)

## tamanho dos bancos
dim(freMTPLfreq)
dim(freMTPLsev)   # nem todas as apólices tiveram indenizações
sum(freMTPLfreq$ClaimNb>0)

ids = freMTPLsev$PolicyID   # apólices com indenizações
length(ids)
length(unique(ids))

sum(freMTPLfreq$PolicyID %in% ids)

## distribuição da severidade
summary(freMTPLsev$ClaimAmount)

## histogramas
par(mfrow=c(1,2))
hist(freMTPLsev$ClaimAmount)
hist(log(freMTPLsev$ClaimAmount))

## Unificando os bancos
claims <- merge(freMTPLsev, freMTPLfreq)
claims.f <- merge(freMTPLsev, freMTPLfreq.f)


## Regressão Gama (para indenizações menores)
reg.gamma <- glm(ClaimAmount ~ CarAge + Gas, family=Gamma(link="log"),
                 data=claims[claims$ClaimAmount<15000,])
summary(reg.gamma)

## Regressão Log-Normal
reg.logn <- lm(log(ClaimAmount) ~ CarAge + Gas,
               data=claims[claims$ClaimAmount<15000,])
summary(reg.logn)


## Regressão Gama (para todos os valores)
reg.gamma <- glm(ClaimAmount ~ DriverAge,
                 family=Gamma(link="log"), data=claims)
summary(reg.gamma)

## Regressão Log-Normal
reg.logn <- lm(log(ClaimAmount) ~ DriverAge, data=claims)
summary(reg.logn)

