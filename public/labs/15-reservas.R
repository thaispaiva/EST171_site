## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 15 - Reservas

require(lifecontingencies)

## carregar tabela atuarial
data("soa08Act")


## Exemplo 1 - reserva para seguro de vida inteira

## prêmio
## 

## reserva t=10
##


## Exemplo 2 - reserva para seguro de vida temporário

## prêmio
## 

## reserva t=10
##


## Gráfico da reserva com tempo

## 1) Crie uma função V para avaliar a reserva no tempo t

## 2) Avalie a função para t=0,...,30
## Dica: usar a função 'Vectorize'

## 3) Faça o gráfico


## Exemplo 3 - reserva para seguro temporario com duração diferente para prêmios

## prêmio
##

## reserva t=3
##

## reserva t=10
##


## Exemplo 4 - reserva para seguro dotal misto

## prêmio
##

## reserva t=10
##


## Exemplo 5 - reserva para anuidade vitalícia diferida

## prêmio
##

## reserva t=10
##

## reserva t=30
##


## Fórmula recursiva

## função para encontrar solução para fórmula recursiva
recurrent = function(a,b,ufinal){
  s <- rev(cumprod(c(1, b)))
  return( ( rev(cumsum(s[-1]*rev(a))) + s[1]*ufinal )/rev(s[-1]) )
}

## Exemplo - seguro temporário
## encontrando a reserva usando a fórmula recursiva

## prêmio
P = Axn(soa08Act,60,30)/axn(soa08Act,60,30)

## Sequências de a e b
Vecta = Vectorize(function(t) Axn(soa08Act,t,1))(60+0:29) - P
Vectb = Vectorize(function(t) pxt(soa08Act,t,1))(60+0:29)/1.06

## Sequência da reserva com a fórmula recursiva
Vectv = c(recurrent(a=Vecta,b=Vectb,ufinal=0),0)

## gráfico das reservas
plot(0:30, Vectv, type="b", pch=20, main="Reserva de seguro temporário por 30 anos")

