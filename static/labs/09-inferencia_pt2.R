## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 09 - Inferência

## Dados de peso e altura de 200 indivíduos (Fonte: Davis, 1990)
require(CASdatasets)
data(Davis)
head(Davis,3)

## ajustando altura
X = Davis$height
hist(X, freq=F, main="Histograma da altura", ylim=c(0,0.05))
lines(density(X), lwd=2, lty=4)
## ajustando distribuição normal
require(fitdistrplus)
mod.normal = fitdist(X, "norm")
param = mod.normal$estimate
param

plot(mod.normal)

## Mistura de duas Normais
## log da densidade
logdf = function(x,parameter){
  p = parameter[1]; m1 = parameter[2]; m2 = parameter[3]
  s1 = parameter[4]; s2 = parameter[5]
  return(log(p*dnorm(x,m1,s1)+(1-p)*dnorm(x,m2,s2)))
}
logL = function(parameter) -sum(logdf(X,parameter))

## restrições
Amat = matrix(c(1,-1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1), 4, 5)
bvec = c(0,-1,0,0)
mix1 = constrOptim(c(.5,160,180,10,10), logL, NULL, ui = Amat, ci = bvec)$par
mix1

## outra maneira - algoritmo EM
require(mixtools)
mix2 = normalmixEM(X)
c(mix2$lambda[1], mix2$mu, mix2$sigma)

## histograma com densidades estimadas
aux = seq(min(X),max(X),len=1000)
hist(X, freq=F, main="Histograma da altura", ylim=c(0,0.05))
lines(aux, dnorm(aux, mean=param[1], sd=param[2]), lwd=2, col=2, lty=2)
lines(aux, mix1[1]*dnorm(aux,mix1[2],mix1[4]) + (1-mix1[1])*dnorm(aux,mix1[3],mix1[5]),
      lwd=2, col=4)
lines(density(X), lwd=2, lty=4)
legend("topright", leg=c("Normal","Mistura Normais","Kernel"), lwd=2, lty=c(2,1,4), col=c(2,4,1), bty="n")
## Modelo de mistura por sexo
boxplot(height ~ sex, data=Davis, horizontal=T, names=c("Female","Male"))

## Ajustando a mistura por sexo
sex = Davis$sex
(pM = mean(sex=="M"))
(paramF = fitdistr(X[sex=="F"],"normal")$estimate)
(paramM = fitdistr(X[sex=="M"],"normal")$estimate)
## histograma com densidades estimadas
aux = seq(min(X),max(X),len=1000)
hist(X, freq=F, main="Histograma da altura", ylim=c(0,0.05))
lines(aux, dnorm(aux, mean=param[1], sd=param[2]), lwd=2, col=2, lty=2)
lines(aux, mix1[1]*dnorm(aux,mix1[2],mix1[4]) + (1-mix1[1])*dnorm(aux,mix1[3],mix1[5]),
      lwd=2, col=4)
lines(density(X), lwd=2, lty=4)
lines(aux, pM*dnorm(aux,paramM[1],paramM[2])+(1-pM)*dnorm(aux,paramF[1],paramF[2]), col="green3", lwd=2)
legend("topright", leg=c("Normal","Mistura Normais","Kernel"), lwd=2, lty=c(2,1,4,1), col=c(2,4,1,"green3"), bty="n")
## Regressão Linear - Exemplo
Y = Davis$height
X1 = Davis$sex
X2 = Davis$weight
df = data.frame(Y,X1,X2)

mod.lin = lm(Y~X1+X2)
summary(mod.lin)

new.obs = data.frame(X1=c("M","M","F"),X2=c(100,70,65))
predict(mod.lin, newdata=new.obs)

