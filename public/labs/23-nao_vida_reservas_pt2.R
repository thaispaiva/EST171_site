## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 23 - Ramo Não-Vida (reservas) - parte 2

## Dados de pagamentos de indenizações - Seguros contra terceiros U.K.
n <- 7
Claims <- data.frame(originf = factor(rep(2007:2013, n:1)),
                     dev = sequence(n:1),
                     inc.paid = c(3511, 3215, 2266, 1712, 1059,  587,
                                   340, 4001, 3702, 2278, 1180,  956,
                                   629, 4355, 3932, 1946, 1522, 1238,
                                  4295, 3455, 2023, 1320, 4150, 3747,
                                  2320, 5102, 4548, 6283))

## transformando em triângulo de desenvolvimento
inc.triangle <- with(Claims, {
  M <- matrix(nrow=n, ncol=n,
              dimnames=list(origin=levels(originf), dev=1:n))
  M[cbind(originf, dev)] <- inc.paid
  M
})

## triângulo acumulado
cum.triangle <- t(apply(inc.triangle, 1, cumsum))

## pagamentos acumulados
Claims$cum.paid <- cum.triangle[with(Claims, cbind(originf, dev))]

names(Claims)[3:4] <- c("inc.paid.k", "cum.paid.k")
ids <- with(Claims, cbind(originf, dev))

## acrescentando os pagamentos do período k+1
Claims <- within(Claims,{
  cum.paid.kp1 <- cbind(cum.triangle[,-1], NA)[ids]
  inc.paid.kp1 <- cbind(inc.triangle[,-1], NA)[ids]
  devf <- factor(dev)
})

head(Claims)
tail(Claims)

## modelos de regressão Chain Ladder
delta <- 0:2
ATA <- sapply(delta, function(d)
  coef(lm(cum.paid.kp1 ~ 0 + cum.paid.k : devf, weights=1/cum.paid.k^d, data=Claims))
)
dimnames(ATA)[[2]] <- paste("Delta = ", delta)

ATA

## gráfico do resultado dos modelos
require(lattice)
xyplot(cum.paid.kp1 ~ cum.paid.k | devf,
       data=subset(Claims, dev < (n-1)), main="Age-to-age developments", as.table=TRUE,
       scales=list(relation="free"), key=list(columns=2, lines=list(lty=1:4, type="l"),
         text=list(lab=c("lm(y ~ x)","lm(y ~ 0 + x)","lm(y ~ 0 + x, w=1/x)","lm(y ~ 0 + x, w=1/x^2)"))),
       panel=function(x,y,...){panel.xyplot(x,y,...)
         if(length(x)>1){
           panel.abline(lm(y ~ x), lty=1)
           panel.abline(lm(y ~ 0 + x), lty=2)
           panel.abline(lm(y ~ 0 + x, weights=1/x), lty=3)
           panel.abline(lm(y ~ 0 + x, , weights=1/x^2), lty=4)
         }
       })


## Método Mack
require(ChainLadder)
(mack <- MackChainLadder(cum.triangle, weights=1, alpha=1, est.sigma="Mack"))

## gráfico dos valores estimados - modelo Mack
plot(mack, lattice=TRUE, layout=c(4,2))

## gráfico dos resíduos 
plot(mack)


## Modelo de Regressão de Poisson
preg <- glm(inc.paid.k ~ originf + devf,
            data=Claims, family=poisson(link = "log"))

summary(preg)

## criando a matriz para guardar os 
allClaims <- data.frame(origin = sort(rep(2007:2013, n)),
                        dev = rep(1:n,n))
allClaims <- within(allClaims, {
  devf <- factor(dev)
  cal <- origin + dev - 1
  originf <- factor(origin)
})

## completando o triângulo
pred.inc.tri <- t(matrix(predict(preg,type="response",
                                 newdata=allClaims), n, n))

pred.inc.tri

## reserva
sum(predict(preg,type="response", newdata=subset(allClaims, cal > 2013)))

## fatores de desenvolvimento
df <- c(0, coef(preg)[(n+1):(2*n-1)])
sapply(2:7, function(i) sum(exp(df[1:i]))/sum(exp(df[1:(i-1)])))

## testando super-dispersão
require(AER)
dispersiontest(preg)


## Modelo Quasi-Poisson
odpreg <- glm(inc.paid.k ~ originf + devf, data=Claims,
              family=quasipoisson)

summary(odpreg)

## gráfico de resíduos
op <- par(mfrow=c(2,2), oma = c(0, 0, 3, 0))
plot(preg)
par(op)

(odp <- glmReserve(as.triangle(inc.triangle), var.power=1, cum=FALSE))


## Bootstrap Chain-Ladder
set.seed(1)
B <- BootChainLadder(cum.triangle, R=1000, process.distr="od.pois")
B

plot(B)

quantile(B, c(0.75,0.95,0.99,0.995))

ny <- (col(inc.triangle) == (nrow(inc.triangle) - row(inc.triangle) + 2))
paid.ny <- apply(B$IBNR.Triangles, 3,
                 function(x){next.year.paid <- x[col(x) == (nrow(x) - row(x) + 2)]
                             sum(next.year.paid) })
paid.ny.995 <- B$IBNR.Triangles[,,order(paid.ny)[round(B$R*0.995)]]
inc.triangle.ny <- inc.triangle

(inc.triangle.ny[ny] <- paid.ny.995[ny])

