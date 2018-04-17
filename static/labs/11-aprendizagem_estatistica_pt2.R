## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 11 - Aprendizagem Estatística

## Pacotes
# install.packages("car")
require(CASdatasets)
require(car)

## carregando os dados
data(credit)
credit = credit[,-which(names(credit) == "foreign_worker")] # removendo variável

## discretizando as covariáveis contínuas
credit.f = credit
credit.f$age = cut(credit.f$age,c(0,25,Inf))
credit.f$credit_amount = cut(credit.f$credit_amount,c(0,4000,Inf))
credit.f$duration = cut(credit.f$duration,c(0,15,36,Inf))

## recodificando algumas variáveis

credit.rcd <- credit.f

credit.rcd$checking_status <- recode(credit.rcd$checking_status, "'A14'='No checking account'; 'A11'='CA < 0 euros'; 'A12'='CA in [0-200 euros['; 'A13'='CA > 200 euros' ")

credit.rcd$credit_history <- recode(credit.rcd$credit_history, "c('A30','A31')='critical account'; c('A32','A33')='existing credits paid back duly till now'; 'A34'='all credits paid back duly'")

credit.rcd$purpose <- recode(credit.rcd$purpose, "'A40'='Car (new)'; 'A41'='Car (used)'; c('A42','A43','A44','A45')='Domestic equipment'; c('A46','A48','A49')='Studies-Business'; 'A47'='Holidays'; else='Else'")

credit.rcd$savings <- recode(credit.rcd$savings, "c('A65','A63','A64')='No savings or > 500 euros'; c('A62','A61')='< 500 euros'")

credit.rcd$employment <- recode(credit.rcd$employment, "c('A71','A72')='unemployed or < 1 year'; 'A73'= 'E [1-4[ years'; c('A74','A75')='> 4 years'")

credit.rcd$personal_status <- recode(credit.rcd$personal_status, "'A91'='male divorced/separated'; 'A92'='female divorced/separated/married'; c('A93','A94')='male single/married/widowed'; 'A95'='female : single'")

credit.rcd$other_parties <- recode(credit.rcd$other_parties, "'A103'='guarantor'; else='none'")

credit.rcd$property_magnitude <- recode(credit.rcd$property_magnitude, "'A121'='Real estate'; 'A124'='No property'; else='Else'")

credit.rcd$other_payment_plans <- recode(credit.rcd$other_payment_plans, "'A143'='None'; else='Banks-Stores'")

credit.rcd$housing <- recode(credit.rcd$housing, "'A152'='Owner';else='Else'")

## Separando Banco de Treinamento
set.seed(123)
index = sort(sample(nrow(credit), 644, replace=F))
table(credit$class[index])

train.db <- credit.rcd[index,]
valid.db <- credit.rcd[-index,]


## Ajustando modelo de regressão logística com Idade e Duração
reg <- glm(class ~ age + duration, data=credit[index,], family=binomial(link="logit"))

summary(reg)

## Ajustando modelo de regressão logística com Histórico
reg <- glm(class ~ credit_history, data=credit, family=binomial(link="logit"))

summary(reg)

## valor previsto por categoria de Histórico
cbind( prop.table(table(credit$credit_history,credit$class),1),
       logit=predict(reg,
                     newdata=data.frame(credit_history=levels(credit$credit_history)),
                     type="response"))

## Ajustando modelo de regressão logística com Histórico e Motivo
reg <- glm(class ~ credit_history*purpose,data=credit.rcd,family=binomial(link="logit"))

## Valores estimados por categoria
attach(credit.rcd)
p.class = matrix( predict(reg, newdata = data.frame(
    credit_history = rep(levels(credit_history),each=length(levels(purpose))),
    purpose = rep(levels(purpose),length(levels(credit_history))) ),
  type="response"), ncol=length(levels(credit_history)), nrow=length(levels(purpose)) )
rownames(p.class) <- levels(purpose)
colnames(p.class) <- levels(credit_history)

p.class

## Razão das chances
p.class/(1-p.class)

## Seleção de Variáveis

## Forward Selection
predictors <- names(credit.rcd) [-grep('class', names(credit.rcd))]
formula <- as.formula(paste("y ~ ", paste(names(credit.rcd[,predictors]), collapse="+")))
logit <- glm(class ~ 1, data=train.db, family=binomial)

for.sel <- step(logit,direction='forward', trace=FALSE, # mudar para trace=TRUE
                k=log(nrow(train.db)), scope=list(upper=formula))

## Backward Selection

logit <- glm(class ~ ., data=train.db[,c("class",predictors)], family=binomial)

back.sel <- step(logit,direction='backward',trace=FALSE, # mudar para trace=TRUE
                 k=log(nrow(train.db)))

