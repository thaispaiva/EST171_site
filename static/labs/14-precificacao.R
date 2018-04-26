## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 14 - Precificação de Seguros

require(lifecontingencies)

## Exercício 1 - construir tabela atuarial a partir da tabela do SOA

## carregar valores de l_x
data("soaLt")

## criar tabela atuarial
soaAct = new("actuarialtable", x=soaLt$x, lx=soaLt$Ix, interest=0.06)

class(soaAct)
?`actuarialtable-class`
summary(soaAct)

## convertendo para data.frame
soaAct.df = as(soaAct, "data.frame")
head(soaAct.df)

## Exemplo - Seguro temporário

## 1) usando somatório
## probabilidades
prob.morte = -diff(soaAct.df$lx)[soaAct.df$x%in%36:38]/soaAct.df$lx[soaAct.df$x==36]
prob.morte

## fatores de desconto
disc = (1+0.06)^(-(1:3))
disc

## valor presente atuarial
p1 = 100000*sum(disc*prob.morte)


## 2) usando funções de comutação
p2 = 100000*with(soaAct.df, (Mx[37]-Mx[40])/Dx[37])


## 3) usando função seguro temporário
p3 = 100000*Axn(actuarialtable = soaAct, x=36, n=3)

cbind(p1,p2,p3)

## seguros com benefícios crescentes

(10+1)*Axn(soaAct, 60, 10)

IAxn(soaAct, 60, 10) + DAxn(soaAct, 60, 10)


## relação entre seguros anuais e em m partes
Axn(actuarialtable=soaAct,x=30,k=12)

0.06/real2Nominal(0.06,12)*Axn(actuarialtable=soaAct,x=30)


## Exemplo - Anuidade vitalícia

## 1) usando somatório
sum( (soaAct.df$lx)[soaAct.df$x%in%65:111]/soaAct.df$lx[soaAct.df$x==65]*1.06^(-(0:45)) )

## 2) usando funções de comutação
with(soaAct.df, (Nx[66]/Dx[66]))

## 3) usando funções de comutação
axn(actuarialtable=soaAct,x=65)


## Exercício
## anuidade postecipada
##


## Exercício
## VPA anuidade temporária
##


## Exercício
## VPA anuidade diferida postecipada e mensal
##


## Exercício - Prêmio 1
##


## Exercício - Prêmio 2
##


## Exercício - Prêmio 3
##

