---
title: "Lista de Exercícios 6"
author: "Thaís Paiva"
date: "21/05/2018"
output:
  html_document: default
  pdf_document: default
  word_document: default
---


## Reservas

### Exercício 1




```r
## prêmio
P = 100000*Axn(soa08Act, x=60)/axn(soa08Act, x=60, n=5)

## reserva t=4
V = 100000*Axn(soa08Act, x=60+4) - P*axn(soa08Act, x=60+4, n=5-4)
```

Para esse seguro, o valor da reserva no tempo $t=4$ é \$34017.86.


### Exercício 2


```r
## prêmio (anual)
P = 100000*Axn(soa08Act, x=60)/axn(soa08Act, x=60, k=4)

## reserva t=10
V = 100000*Axn(soa08Act, x=60+10) - P*axn(soa08Act, x=60+10, k=4)
```

Para esse seguro, o valor da reserva no tempo $t=10$ é \$23418.24.


### Exercício 3


```r
## prêmio
P = 50000*Exn(soa08Act, x=75, n=20)/axn(soa08Act, x=75, n=20)

## reserva t=5
V = 50000*Exn(soa08Act, x=75+5, n=20-5) - P*axn(soa08Act, x=75+5, n=20-5)
```

Para esse seguro, o valor da reserva no tempo $t=5$ é \$889.72.


### Exercício 4


**a)** O código abaixo calcula a reserva para este contrato, considerando os diferentes valores de acordo com o tempo $t$.



```r
## prêmio
P = 100000*AExn(soa08Act, x=60, n=20)/axn(soa08Act, x=60, n=15)

## reserva
V = function(t){
  if(t<=15)
    res = 100000*AExn(soa08Act, x=60+t, n=20-t) - P*axn(soa08Act, x=60+t, n=15-t)
  else
    res = 100000*AExn(soa08Act, x=60+t, n=20-t)
  if(t>=20 | t<0)
    res = 0
  return(res)
}
```


**b)** Vemos a seguir a constituição da reserva para o seguro dotal misto ao longo do tempo. 


<img src="lista-06-resolucao_files/figure-html/unnamed-chunk-6-1.png" width="70%" style="display: block; margin: auto;" />

    
## Seguros: mais cálculos


### Exercício 5


```r
VPA = 1000*12*( axn(soa08Act, x=30, k=12) - axyzn(list(soa08Act, soa08Act), x=c(30,40), k=12) )
P = VPA/(12*axyzn(list(soa08Act,soa08Act),c(30,40),n=30,k=12))
```

**a)** O VPA da anuidade reversível é \$19801.1.

**b)** O prêmio mensal para esse contrato é \$127.06.


### Exercício 6


```r
G = (10000*Axn(soa08Act,30,20)+30*axn(soa08Act,30,20))/(0.85*axn(soa08Act,30,20))
```

O prêmio bruto para esse contrato é \$64.15.


### Exercício 7

Uma amostra de $K_{25}$ pode ser gerada com o seguinte comando:


```r
K25 = rLife(n=10^4, object=soa08Act, x=25, type="Kx")
```

**a)** As estatísticas descritivas e o histograma da amostra de $K_{25}$ podem ser vistos abaixo.


```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   41.00   51.00   49.06   59.00   80.00
```

<img src="lista-06-resolucao_files/figure-html/unnamed-chunk-11-1.png" width="70%" style="display: block; margin: auto;" />


**b)** Podemos estimar a probabilidade de um indivíduo de 25 anos sobreviver até os 60 anos usando a amostra simulada acima. Como vemos abaixo, a probabilidade estimada é bem próxima da probabilidade teórica obtida a partir da tábua de mortalidade.



```r
## probabilidade estimada
mean(K25>35)
```

```
## [1] 0.8401
```

```r
## probabilidade teórica
pxt(soa08Act, x=25, t=35)
```

```
## [1] 0.8560439
```


### Exercício 8


```r
a65 = 1000*rLifeContingencies(n=100000, lifecontingency="axn", object=soa08Act,
                              x=65, parallel=TRUE)
P.perc = quantile(a65, p=0.75)
```

O prêmio para esse contrato obtido de acordo com o princípio do percentil é \$12764.08.

