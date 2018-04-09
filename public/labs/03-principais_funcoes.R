## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 03 - Principais Funções e Operações

## caracteres e strings
cidade = "Belo Horizonte, MG"
class(cidade)

nchar(cidade)

substr(cidade, start=1, stop=4)
cidade = paste(cidade, ", Brasil", sep="")
cidade

strsplit(cidade, split=", ")    # retorna uma lista

unlist(strsplit(cidade, split=", "))[3]

cidades = c("Belo Horizonte, MG", "São Paulo, SP", "Brasília, DF")
substr(cidades, nchar(cidades)-1, nchar(cidades))

unlist(strsplit(cidades, ", "))[seq(2,6,by=2)]

n_cid = length(cidades)
cat("Número de cidades: ",n_cid)


## variaveis categoricas

x = c( rep( letters[1:2], each=2), letters[3] )
x

x = factor(x)
x

unclass(x)
x = factor(x, labels=c("Ativo", "Aposentado", "Pensionista"))
x

relevel(x, "Aposentado")

factor(x, ordered=T)

model.matrix(~0+x)

set.seed(1)
U = runif(10)
U

cut(U, breaks=3, labels=c("S","M","L"))

x = cut(U, breaks=c(0,.3,.8,1), labels=c("S","M","L"))
x

table(x)

gl(n=3, k=2, labels=c("S","M","L"), ordered=T)


## datas

datas = as.Date(c("16/10/12", "19/11/12"), format="%d/%m/%y")
class(datas)
datas

diff(datas)

difftime(datas[2], datas[1], units="weeks")

seq.datas = seq(from=datas[1], to=datas[2], by=7)
seq.datas

format(seq.datas, "%b")     # mês em formato abreviado

weekdays(seq.datas)
weekdays(datas[1]+1:4)
months(seq.datas)

as.POSIXct(seq.datas)
substr( as.POSIXct(seq.datas), start=1, stop=4 )
strftime(seq.datas ,"%Y")  # mais rápido


temp = Sys.setlocale("LC_TIME")
temp

Sys.setlocale("LC_TIME", "english" )  # CUIDADO!


weekdays(seq.datas)
months(seq.datas)

Sys.setlocale("LC_TIME", temp)  # voltar para a configuração original


## Exemplo expressões simbólicas
set.seed(123)
df = data.frame( Y=rnorm(50), X1=as.factor(sample(LETTERS[1:4],size=50,replace=TRUE)), 
                 X2=as.factor(sample(1:3,size=50,replace=TRUE)) )
tail(df)

reg = lm( Y ~ X1 + X2, data=df)
model.matrix(reg)[45:50,]

reg = lm( Y ~ X1 + X2 + X1:X2, data=df)
model.matrix(reg)[45:50,]

x = c(1,4,6,6,10,5)
var(x)
sum( (x-mean(x))^2 ) / (length(x) - 1)

m = matrix(x,3,2)
m

mean(m)

apply(m, MARGIN=1, FUN=mean)

g = c("M","F","F","M","M","M")
base = data.frame(x,g)
base

tapply(x, g, mean)

base$status = c("Ativo", "Ativo", "Aposentado", "Pensionista", "Pensionista", "Ativo")
table(base$g, base$status)

addmargins( table(base$g, base$status) )

# loop - Exemplo Processo de Poisson
T = NULL
while( sum(T) <= 10 ){
  T = c( T, rexp(1) )
}
T

cumsum(T)
# Condições lógicas
set.seed(1)
u = runif(1)
u
if(u> 0.5){
  ("maior que 50%")
}else{
  ("menor que 50%")
}


ifelse( u>0.5, ("maior que 50%"),
               ("menor que 50%")  )

u = runif(3)
u
if(u> 0.5){
  ("maior que 50%")
}else{
  ("menor que 50%")
}

ifelse( u>0.5, ("maior que 50%"), ("menor que 50%") )

