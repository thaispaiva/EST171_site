## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 10 - Aprendizagem Estatística

require(CASdatasets)
data(credit)
names(credit)

credit = credit[,-which(names(credit) == "foreign_worker")]

table(credit$class)

## qual o tipo de cada variável?
##

## discretizando as covariáveis contínuas
credit.f = credit
credit.f$age = cut(credit.f$age,c(0,25,Inf))
credit.f$credit_amount = cut(credit.f$credit_amount,c(0,4000,Inf))
credit.f$duration = cut(credit.f$duration,c(0,15,36,Inf))

## distribuição variáveis contínuas
##

## Estatística V de Cramer
cramer = function(i)
  sqrt(chisq.test(table(credit.f[,i],credit.f$class))$statistic/(length(credit.f[,i])))

## p-valor
pv = function(i) chisq.test(table(credit.f[,i],credit.f$class))$p.value

## Calculando V de Cramer para todas as covariáveis
k = ncol(credit.f)-1
CRAMER = data.frame( variable=names(credit)[1:k],
                     cramerv = Vectorize(cramer)(1:k),
                     p.value = Vectorize(pv)(1:k) )
vCRAMER = CRAMER[order(CRAMER[,2], decreasing=TRUE),]

## Plotando os níveis de correlação
par(mar=c(10,4,4,0))
barplot(vCRAMER[,2],names.arg=vCRAMER[,1],las=3)

## comparando a distribuição de idade e duração vs classificação do cliente
aggregate(credit[,c("age","duration")], by=list(class=credit$class), mean)

## classificação de clientes por faixa etária
Q = quantile(credit$age,seq(0,1,by=.1))
Q[1] = Q[1]-1
cut.age = cut(credit$age,Q)
prop = prop.table(table(cut.age,credit$class),1)

## gráfico da classificação por idade
barplot(t(prop))
abline(h=mean(credit$class==0),lty=2)

