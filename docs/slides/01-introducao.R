# fixar a semente
set.seed(1)
# gerar uma amostra de tamanho 200
X = rexp(200, rate=1/100)
head(X)
mean(X)
# equação em função de x=alfa
f = function(x){
  log(x) - digamma(x) - log(mean(X)) + mean(log(X))
}

# encontrar a raiz da equação
alpha = uniroot(f, interval = c(1e-8,1e8))$root
alpha # estimador para alfa

beta = alpha/mean(X) # estimador para beta
beta
# função da distribuição acumulada de S
F = function(x, lambda=100, nmax=1000){
  n = 0:nmax
  return( sum(pgamma(x, n*alpha, beta)*dpois(n, lambda)) )
}
# encontrando o quantil .995
uniroot(function(x) F(x)-.995, c(1e-8,1e8) )$root
