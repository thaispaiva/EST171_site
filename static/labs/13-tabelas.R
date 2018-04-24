## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 13 - Tabelas de Vida

require(lifecontingencies)

## Exercício 1

## criando tabela de vida - 1

tab1 = new("lifetable", x=seq(0,10,1),
           lx=seq(from=1000,to=0,by=-100),name="Sample life table 1")

print(tab1)

class(tab1)
?`lifetable-class`

summary(tab1)
plot(tab1)

## Exercício 2
## criando tabela de vida - 2

tab2 = probs2lifetable(probs=seq(from=0.1,to=1,by=0.1),
                       radix=100000,type="qx",name="Sample life table 2")

head(tab2)
tail(tab2)

## exportando para data.frame
tab2.df = as(tab2, "data.frame")
class(tab2.df)


## Exercício 3

## carregando a tabela de vida ilustrativa do SOA
data("soa08")

slotNames(soa08)
head(soa08@lx)

## prob de (65) morrer em 20 anos
(soa08@lx[soa08@x==65]-soa08@lx[soa08@x==85])/soa08@lx[soa08@x==65]

qxt(soa08, 65,20)

## prob de (25) sobreviver 65 anos
##


## Exercício 4

## esperança de vida em anos completos ao nascer
sum(soa08@lx[soa08@x%in%(1:110)]/soa08@lx[soa08@x==0])

sum(soa08@lx/soa08@lx[soa08@x==0])-1

exn(object=soa08)


## esperança de vida em anos completos entre as idades 50 e 60

sum(soa08@lx[soa08@x%in%(51:60)]/soa08@lx[soa08@x==50])

exn(object=soa08, x=50, n=10, type="curtate")


## Exercício 5

(sum(soa08@lx[soa08@x%in%(81:90)]/soa08@lx[soa08@x==80])+
   sum(soa08@lx[soa08@x%in%(80:89)]/soa08@lx[soa08@x==80]))/2

exn(soa08,80,10,"complete")


## Exercício 6

## probabilidades para idades fracionárias

pxt(object=soa08, x=80.25, t=0.5, fractional="linear")

pxt(object=soa08, x=80.25, t=0.5, fractional="constant force")


## Exercício 7
##

