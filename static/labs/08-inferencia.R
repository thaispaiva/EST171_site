## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 08 - Inferência

## Pacotes
# install.packages("fitdistrplus")  # instalar esse primeiro
# install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/R/", type="source")
require(CASdatasets)
require(fitdistrplus)
require(actuar)

## dados seguro contra terceiros Itália
data("itamtplcost")

x = itamtplcost$UltimateCost/10^6
summary(x)
## estimador máxima verossimilhança
fgamEMV = fitdist(x, "gamma", method="mle")
fgamEMV

summary(fgamEMV)

plot(fgamEMV)

## estimador método de momentos
fgamEMM = fitdist(x, "gamma", method="mme")

cbind(EMV=fgamEMV$estimate, EMM=fgamEMM$estimate)

## histograma e densidades das distribuições ajustadas
denscomp(list(fgamEMV, fgamEMM), legendtext=c("EMV","EMM"), fitcol=1:2, lwd=2,
         main="Histograma e densidades gama ajustadas")

## histograma e densidade empírica
hist(x, prob=TRUE, ylim=c(0, 1), main="Histograma e densidade empírica")
lines(density(x), lty=5, lwd=2, col=4)

## Dados de perda em incêndios na Dinamarca
data("danishuni")
head(danishuni)

x = danishuni$Loss
hist(x, main="Histograma das indenizações de incêndio")

## ajustando as distribuições
fgam = fitdist(x, "gamma", lower=0) # gama
fpar = fitdist(x, "pareto", start=list(shape=2, scale=2), lower=0) # pareto

# mistura gama e pareto (última aula)
dmixgampar = function(x, prob, nu, lambda, alpha, theta)
  prob*dgamma(x, nu, lambda) + (1-prob)*dpareto(x, alpha, theta)
pmixgampar = function(q, prob, nu, lambda, alpha, theta)
  prob*pgamma(q, nu, lambda) + (1-prob)*ppareto(q, alpha, theta)

fmixgampar = fitdist(x, "mixgampar",
                     start=list(prob=1/2, nu=1, lambda=1, alpha=2, theta=2), lower=0)

## Resultados dos modelos separados (Gama e Pareto) e modelo de mistura
cbind(SINGLE= c(NA, fgam$estimate, fpar$estimate), MIXTURE=fmixgampar$estimate)

## Gráfico das funções de distribuição ajustadas
cdfcomp(list(fgam, fpar, fmixgampar), xlogscale=TRUE, datacol="grey", lwd=2,
        legendtext=c("Gama","Pareto","Gam-Par"), main="Funções de distribuição ajustadas")

## função para calcular o quantil da mistura
qmixgampar = function(p, prob, nu, lambda, alpha, theta){
  L2 = function(q, p) (p - pmixgampar(q, prob, nu, lambda, alpha, theta))^2
  sapply(p, function(p) optimize(L2, c(0, 10^3), p=p)$minimum)
}

## qqplot
qqcomp(list(fgam, fpar, fmixgampar), xlog=TRUE, ylog=TRUE, main="QQ-plot Dados de Incêndio",
       legendtext=c("Gama","Pareto","Gam-Par"), fitpch=c(4,20,1))

## Teste de Adequação de Ajuste
## Exemplo: no. de indenizações em apólices de seguro contra terceiros

data(tplclaimnumber)
summary(tplclaimnumber)

x = tplclaimnumber$claim.number

fpois = fitdist(x, "pois")  # poisson
fnbinom = fitdist(x, "nbinom")  # binomial negativa

## funções para definir a distribuição poisson modificada em zero
dpoisZM <- function(x, prob, lambda)
  prob*(x == 0) + (1-prob)*(x > 0)*dpois(x-1, lambda)
ppoisZM <- function(q, prob, lambda)
  prob*(q >= 0) + (1-prob)*(q > 0)*ppois(q-1, lambda)
qpoisZM <- function(p, prob, lambda)
  ifelse(p <= prob, 0, 1+qpois((p-prob)/(1-prob), lambda))

fpoisZM = fitdist(x, "poisZM", start=list(prob=sum(x == 0)/length(x), lambda=mean(x)),
                  lower=c(0,0), upper=c(1, Inf))  # poisson modificada

gofstat(list(fpois, fnbinom, fpoisZM), chisqbreaks=c(0:4, 9),
        discrete=TRUE, fitnames=c("Poisson","NegBinomial","ZM-Poisson"))
