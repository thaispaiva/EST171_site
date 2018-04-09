## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 07 - Principais Distribuições Estatísticas

# distribuições de Pearson
library(PearsonDS)
x <- seq(-1, 6, by=1e-3)
y0 <- dpearson0(x, 2, 1/2)
y1 <- dpearsonI(x, 1.5, 2, 0, 2)
y2 <- dpearsonII(x, 2, 0, 1)
y3 <- dpearsonIII(x, 3, 0, 1/2)
y4 <- dpearsonIV(x, 2.5, 1/3, 1, 2/3)
y5 <- dpearsonV(x, 2.5, -1, 1)
y6 <- dpearsonVI(x, 1/2, 2/3, 2, 1)
y7 <- dpearsonVII(x, 3, 4, 1/2)
plot(x, y0, type="l", ylim=range(y0, y1, y2, y3, y4, y5, y7),
     ylab="f(x)", main="The Pearson distribution system")
lines(x[y1 != 0], y1[y1 != 0], lty=2, lwd=2)
lines(x[y2 != 0], y2[y2 != 0], lty=3, lwd=2)
lines(x[y3 != 0], y3[y3 != 0], lty=4, lwd=2)
lines(x, y4, col="grey", lwd=2)
lines(x, y5, col="grey", lty=2, lwd=2)
lines(x[y6 != 0], y6[y6 != 0], col="grey", lty=3, lwd=2)
lines(x[y7 != 0], y7[y7 != 0], col="grey", lty=4, lwd=2)
legend("topright", leg=paste("Pearson", 0:7), lty=1:4,
       col=c(rep("black", 4), rep("grey", 4)))

# transformações
f <- function(x) dgamma(x,2)
f1 <- function(x) f(x-1)
f2 <- function(x) f(x/2)/2
f3 <- function(x) 2*x*f(x^2)
f4 <- function(x) f(1/x)/x^2
f5 <- function(x) f(exp(x))*exp(x)
f6 <- function(x) f(log(x))/x
x=seq(0,10,by=.025)
plot(x,f(x), ylim=c(0, 1.3), xlim=c(0, 10), main="Theoretial densities",
     lwd=2, type="l", xlab="x", ylab="")
lines(x,f1(x), lty=2, lwd=2)
lines(x,f2(x), lty=3, lwd=2)
lines(x,f3(x), lty=4, lwd=2)
lines(x,f4(x), lty=1, col="grey", lwd=2)
lines(x,f5(x), lty=2, col="grey", lwd=2)
lines(x,f6(x), lty=3, col="grey", lwd=2)
legend("topright", lty=1:4, col=c(rep("black", 4), rep("grey", 3)),
       leg=c("X","X+1","2X", "sqrt(X)", "1/X", "log(X)", "exp(X)"))

## Distribuições mistas

# distribuição beta modificada
dbetaOM = function(x, prob, a, b)
  dbeta(x, a, b)*(1-prob)*(x != 1) + prob*(x == 1)

pbetaOM = function(q, prob, a, b)
  pbeta(q, a, b)*(1-prob) + prob*(q >= 1)

# mistura gama e pareto
library(actuar)   # carregando pacote com dist. pareto

dmixgampar <- function(x, prob, nu, lambda, alpha, theta)
  prob*dgamma(x, nu, lambda) + (1-prob)*dpareto(x, alpha, theta)

pmixgampar <- function(q, prob, nu, lambda, alpha, theta)
  prob*pgamma(q, nu, lambda) + (1-prob)*ppareto(q, alpha, theta)
