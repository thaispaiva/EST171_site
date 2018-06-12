## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 22 - Ramo Não-Vida (reservas)

## Dados de pagamentos de indenizações - Seguros contra terceiros U.K.
n <- 7
Claims <- data.frame(originf = factor(rep(2007:2013, n:1)),
                     dev = sequence(n:1),
                     inc.paid = c(3511, 3215, 2266, 1712, 1059,  587,
                                   340, 4001, 3702, 2278, 1180,  956,
                                   629, 4355, 3932, 1946, 1522, 1238,
                                  4295, 3455, 2023, 1320, 4150, 3747,
                                  2320, 5102, 4548, 6283))
Claims

## transformando em triângulo de desenvolvimento
(inc.triangle <- with(Claims, {
  M <- matrix(nrow=n, ncol=n,
              dimnames=list(origin=levels(originf), dev=1:n))
  M[cbind(originf, dev)] <- inc.paid
  M
}))

## triângulo acumulado
(cum.triangle <- t(apply(inc.triangle, 1, cumsum)))

## diagonal - pagamentos acumulados de cada período de origem
(latest.paid <- cum.triangle[row(cum.triangle) == n - col(cum.triangle) + 1])

## pagamentos acumulados
Claims$cum.paid <- cum.triangle[with(Claims, cbind(originf, dev))]
head(Claims)


## Gráfico da sequência de pagamentos individuais e acumulados por origem
op <- par(fig=c(0,0.5,0,1), cex=0.8, oma=c(0,0,0,0))
with(Claims, {
  interaction.plot(x.factor=dev, trace.factor=originf, response=inc.paid,
                   fun=sum, type="b", bty='n', legend=FALSE); axis(1, at=1:n)
  par(fig=c(0.45,1,0,1), new=TRUE, cex=0.8, oma=c(0,0,0,0))
  
  interaction.plot(x.factor=dev, trace.factor=originf, response=cum.paid,
                   fun=sum, type="b", bty='n'); axis(1,at=1:n)
  })
mtext("Incremental and cumulative claims development",
      side=3, outer=TRUE, line=-3, cex = 1.1, font=2)
par(op)


## Gráfico dos pagamentos acumulados em painéis
library(lattice)
xyplot(cum.paid ~ dev | originf, data=Claims, t="b", layout=c(4,2),
       as.table=TRUE, main="Cumulative claims development")


## Cálculo de Reservas
## Métodos determinísticos


## fatores de desenvolvimento
f <- sapply( (n-1):1, function(i) {
  sum(cum.triangle[1:i, n-i+1])/sum(cum.triangle[1:i, n-i]) } )

tail <- 1   # último fator de desenvolvimento
(f <- c(f, tail))

## completando o triângulo
full.triangle <- cum.triangle
for(k in 1:(n-1)){
  full.triangle[(n-k+1):n, k+1] <- full.triangle[(n-k+1):n,k]*f[k]
}
full.triangle

## total de pagamentos previstos
(ultimate.paid <- full.triangle[,n])

## fatores de desenvolvimento totais
(ldf <- rev(cumprod(rev(f))))

## proporção de indenizações estimadas
(dev.pattern <- 1/ldf)

## reserva
(reserve <- sum (latest.paid * (ldf - 1)))

sum(ultimate.paid - latest.paid)

