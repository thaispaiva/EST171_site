## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 05 - Gráficos no R

## Dados de ciclones tropicais nos EUA de 1899-2006
StormMax = read.table("../datasets/extremedatasince1899.csv", header=T, sep=",")
attach(StormMax)

table( trunc(Yr/10)*10 )[-1]

# gráfico de barra do número de ciclones por década
barplot( table( trunc(Yr/10)*10 )[-1] )

# gráfico de linhas - vel. média por década e região
decade = trunc(Yr/10)*10
interaction.plot(decade, Region, Wmax, type="b", pch=1:5)

## Graficos lado a lado

set.seed(123)
x = rnorm(100)    # gerando amostra
par(mfrow=c(1,2)) # plotar com 1 linha e 2 colunas

## grafico 1: histograma
hist(x, xlab="x", ylab="densidade", main="", col="grey", border="white", probability=T)

# adicionar linhas da densidade (normal com estimadores de momentos)
u = seq(min(x)-1, max(x)+1, by=.01)
lines(u, dnorm(u,mean(x),sd(x)), lty=2)

# densidade estimada com kernel da normal
d = density(x)
lines(d$x, d$y)

# adicionar legenda
legend("topleft", legend=c("Histograma","Densidade Normal", "Densidade Kernel"),
       col=c("grey","black","black"), lwd=c(NA,1,1), lty=c(NA,2,1),
       pch=c(15,NA,NA), bty="n")

## grafico 2: função de distribuição empírica
F.empirica = function(y){
  mean(x<=y)
}

plot(u, Vectorize(F.empirica)(u), type="s", lwd=2, col="grey",
     xlab="x", ylab="Probabilidade acumulada", main="", axes=FALSE)
axis(1); axis(2)  # adicionar eixos
lines(d$x, cumsum(d$y)*diff(d$x)[1])      # f.d. kernel
lines(u, pnorm(u,mean(x),sd(x)), lty=2)   # f.d. normal

# adicionar legenda
legend("topleft", c("F.D. Empírica", "F.D. Normal", "F.D. Kernel"),
       col=c("grey","black","black"), lwd=c(2,1,1), lty=c(1,2,1), bty="n")
## Graficos para classes de objetos
## Voltando aos dados de ciclones...

# selecionando ciclones que ocorreram após 1977 na região 'Basin'
StormMax.Basin = subset(StormMax, (Region=="Basin")&(Yr>1977))
attach(StormMax.Basin)

# boxplot da velocidade por ano
boxplot(Wmax ~ as.factor(Yr), xlab="Ano", ylab="Velocidade (nós)", col="grey")

# regressão linear entre velocidade do vento e ano
model = lm(Wmax ~ Yr)
par(mfrow=c(2,2))
plot(model)

## Graficos 3D

# exemplo normal bivariada
binorm = function(x1,x2,r=0){
  exp( -(x1^2 + x2^2 - 2*r*x1*x2)/( 2*(1-r^2) ) )/( 2*pi*sqrt(1-r^2) )
}

x = y = seq(-2.5, 2.5, by=.25)
z = outer(x, y, function(u,v) binorm(u,v,r=.4))

## para escolher as cores
# install.packages("RColorBrewer")
require(RColorBrewer)
gray.col = gray.colors(n=100, start=0, end=1)

par(mfrow=c(1,2))

# gráfico 1: curva de níveis
image(x, y, z, col=rev(gray.col))
contour(x, y, z, add=TRUE)

# grafico 2: superfície 3D
persp(x, y, z)

persp(x, y, z, theta=210, col=gray.col[45], shade=TRUE)
