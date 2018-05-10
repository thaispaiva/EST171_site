## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 16 - Mais cálculos de Seguros

require(lifecontingencies)
data("soa08Act")


## Exemplo 1: VPA seguro último sobrevivente
tab.xy = list(soa08Act,soa08Act)

## cálculo direto
Axyzn(tab.xy, x=c(60,70), status="last")

## cálculo indireto
Axn(soa08Act,60) + Axn(soa08Act,70) - Axyzn(tab.xy,x=c(60,70))


## Exemplo 2: VPA seguro vida conjunta

## cálculo direto
##

## cálculo indireto
##


## verifique se a igualdade é verdadeira
##


## Exemplo 3: anuidades reversíveis

## VPA de a_{60|70}
##


## Exemplo 4: Seguro com despesas

## encontre o prêmio bruto


## Exemplo 5: Reserva com despesas

## encontre o prêmio puro
##

## encontre o prêmio bruto
##

## encontre a reserva líquida
##

## encontre a reserva bruta
##

############################################################


## Simulação - amostra de tempos de vida ao nascer K(0)
set.seed(123)
K0 = rLife(n=1000, object=soa08Act, x=0, type="Kx")

## histograma
hist(K0, freq=F, main="Tempos de vida ao nascer")
lines(density(K0), lwd=2, col="blue")

## estatísticas descritivas
summary(K0)

## média empírica
mean(K0)

## média teórica
exn(soa08Act)

## são iguais?
t.test(x=K0, mu=exn(soa08Act))


## Exemplo - simulação de valores presente de seguro

## 1) gerar amostra de tempos de vida ao nascer
set.seed(2018)
##

## 2) calcular valor presente da indenização
##

## 3) encontrar média e variância
##

## média teórica
##

## variância teórica
##


## Exemplo - cálculo do prêmio de acordo com o princípio do percentil

## primeiro, vamos gerar amostras de VPA's de seguros temporários
set.seed(171)
samples = 100000*rLifeContingencies(n=10000, lifecontingency="Axn", object=soa08Act,
                                    x=25, t=40, parallel=TRUE)

## distribuição da amostra
head(samples)
hist(samples)
summary(samples)

## probabilidade de VPA positivo
mean(samples>0)
qxt(soa08Act,25,40)

## Calculando o prêmio (único)

## de acordo com o princípio do percentil
(P.perc = quantile(samples, p=0.95))

## de acordo com o princípio da equivalência
(P.equi = mean(samples))                  # empírico
(P.teo  = 100000*Axn(soa08Act, 25, 40))   # teórico

## histograma dos VPAs
hist(samples, freq=F, main="VPAs de seguros temporários", xlab="Axn")
abline(v=P.perc, lwd=2, col=1)
abline(v=P.equi, lwd=2, col=2)
legend("topright", lwd=2, col=c(1,2), legend=c("P (percentil)", "P (equiv.)"))

## probabilidade de perda positiva
mean(samples>P.perc)
mean(samples>P.equi)


## Exemplo - carteira de aposentados

## primeiro, gerar amostra de VPAs de anuidades
set.seed(1405)
ax65 = rLifeContingencies(100000, lifecontingency="axn", object=soa08Act, x=65, parallel=TRUE)

## distribuição da amostra
head(ax65)
hist(ax65)
summary(ax65)

## média e desvio padrão dos VPAs
muax65 = mean(ax65)
sdax65 = sd(ax65)

## prêmio
(P = qnorm(p=0.99, mean=1000*muax65, sd=sqrt(1000)*sdax65)/1000)

## distribuição da perda total
aux = seq(1000*muax65 - 4*sqrt(1000)*sdax65, 1000*muax65 + 4*sqrt(1000)*sdax65, len=1000)
f = dnorm(aux, mean=1000*muax65, sd=sqrt(1000)*sdax65)
plot(aux, f, type="l", lwd=2, main="Distribuição da perda total")
abline(v=1000*P, col=2, lwd=2)


## Exemplo - variância de seguro crescente

## gerar amostra de VPAs
set.seed(0910)
sampleIAxn = rLifeContingencies(50000, lifecontingency="IAxn", object=soa08Act,
                                x=45, t=20, parallel=TRUE)

## variância amostral
var(sampleIAxn)

## variância teórica
IAxn(soa08Act, 45, 20, power=2) - (IAxn(soa08Act, 45, 20))^2

