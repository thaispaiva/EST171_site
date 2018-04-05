## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 04 - Outras funções

# Exemplo - função personalizada

f = function(x,p,d){
  n = length(x)
  s = sum( p*x/(1+d)^(1:n) )
  return(s)
}

f

f(x=c(100,200,100), p=c(.4,.5,.3), d=0.05)

qnorm(.95)  # quantil 95% para normal(0,1)

qnorm(.95, mean=1, sd=2)  # quantil 95% para normal(1,4)

h = function(n) if(n<=1) return(1) else return(2*h(n-1)+1)
h(4)

beta = 0
slope = function(X,Y){
  beta = coefficients(lm(Y~X))[2]
  return(as.numeric(beta))
}
attach(cars)
slope(speed,dist)
beta

slope = function(X,Y){
  beta = coefficients(lm(Y~X))[2]
  cat("A inclinação da reta é ",beta,"\n")
  return(as.numeric(beta))
}
slope(speed,dist)

# Funções para cálculo de seguros
require(lifecontingencies)
data("demoFrance")

# carregando a tabela de vida francesa
alive = demoFrance$TV88_90
head(alive)   # l_x
death = -diff(alive)  # d_x
head(death)

# função de sobrevivência de Makeham
sMakeham = function(x,a,b,c){
  ifelse( x<0, 1, exp(-a*x-b/log(c)*(c^x-1)) )
}

# função de probabilidade para idades inteiras
dMakeham = function(x,a,b,c){
  ifelse( x>floor(x), 0, sMakeham(x,a,b,c)-sMakeham(x+1,a,b,c) )
}

death = death[-c(1,107:111)]  # removendo algumas idades
ages = 1:(length(death))

# função da log verossimilhança
loglikMakeham = function(abc){  # abc: vetor com paramêtros
  - sum( log( dMakeham(ages,abc[1],abc[2],abc[3]))*death[ages] )
}

# estimador de máxima verossimilhança
mlEstim = optim( c(1e-5,1e-4,1.1), loglikMakeham)
abcml = mlEstim$par
abcml
# idade esperada de morte
sum( (ages+.5)*death )/sum(death)
integrate( sMakeham, 0, Inf, abcml[1], abcml[2], abcml[3])

