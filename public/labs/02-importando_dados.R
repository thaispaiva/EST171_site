## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 02-Importando Dados
x = exp(1)
x
class(x)
.Machine$double.xmax    # maior numero antes de Inf
2e+307<Inf   # verdadeiro ou falso?
2e+308<Inf   # verdadeiro ou falso?
0/0   # não está definido
pi    # 3,1416...
y = x+1
y
x = c(-1,0,2)
x
y = c(0, 2^x)
y
u = 1:50
u
seq(from=0, to=1, by=.1)
seq(5,2,-1)
seq(5,2,len=9)
c(NULL,x)
y = NULL
for(i in 1:10){
  y = c(y, max(sin(u[1:i])))
}
y
names(x) = c("A","B","C")
x
x[c(3,2)]
x[c("C","B")]
x = c(100,200,300,400,500,600,700,800)
y = c(1,2,3,4)
x+y
y = c(1,2,3)
x+y
set.seed(1)
U = runif(10)
U
U[ U > 0.8 ]
U[ (U>.4) & (U<.6) ]
which( (U>.4) & (U<.6) )
which( !((U>.4) & (U<.5)) )
which( (U<=.4) | (U>=.5) )
y
y==2
M = matrix(U, nrow=5, ncol=4)
M
dim(M)
M[ M[,4] > 0.8, ]
sweep(M, MARGIN=1, STATS=1:nrow(M), FUN="+")
M = matrix(1:8, nrow=4, ncol=3, byrow=FALSE)
M + c(10,20,30,40,50)
A = matrix(0,2,2)
A
B = matrix(1,1,2)
B
rbind(A,B)
cbind(A, t(B))
A = array(1:36 ,c(3,6,2))
A
lista = list(subM = M[1:2,], sequence=U)
lista
names(lista)
lista[[2]]
lista$sequence
file = "../datasets/extremedatasince1899.csv"
StormMax = read.table(file, header=T, sep=",")
head(StormMax, 3)
str(StormMax)   # mostra a estrutura (structure) de um objeto
set.seed(123)
df = data.frame( x1=rnorm(5), x2=sample(1:2,size=5,replace=T), x3=rnorm(5))
df
df [ order(df$x2, -df$x1), ]
