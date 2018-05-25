## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 19 - Portfólios (parte 2)

## Pacotes necessários
# install.packages("robustbase")
require(robustbase)

## Executar o código da aula passada
file = "../labs/18-portfolios.R"
source(file)


## Restrição Retorno Alvo
targetReturn <- function(x, target) {
  list(Aeq = rbind(colMeans(x)), aeq = target)
}

## Restrição Investimento Total
fullInvest <- function(x) {
  list(Aeq = matrix(1, nrow = 1, ncol = ncol(x)), aeq = 1)
}

## Restrição Long Only
longOnly <- function(x) {
  list(A = diag(1, ncol(x)), a = rep(0, ncol(x)))
}

## Dados NASDAQ
file = "../datasets/NASDAQ/nasdaq.csv"
nas = read.csv(file, colClasses = c("Date", rep("numeric",10)))
head(nas)
id = names(nas)[-1]

## calcular retornos
x <- apply(nas[,-1], MAR=2, function(x) x[-1] / x[-length(x)] - 1)
dim(x)

## Restrição de Grupo
GroupBudget <- function() {
  # max 10\% in financial and bank sector
  A1 <- matrix(0, ncol = length(id), nrow = 1)
  colnames(A1) <- id
  A1[1, c("IXBK", "IXF")] <- -1
  a1 <- -0.1
  
  # max 30\% in a single instrument
  A2 <- diag(-1, length(id))
  a2 <- rep(-0.3, length(id))
  
  # at least 10\% in treasury
  A3 <- matrix(0, ncol = length(id), nrow = 1)
  colnames(A3) <- id
  A3[1, c("FVX", "TYX")] <- 1
  a3 <- 0.1
  list(A = rbind(A1, A2, A3), a = c(a1, a2, a3))
}


## Modelo de Média-Variância 
MV_QP <- function(x, target, Sigma = cov(x), ...,
                  cstr = c(fullInvest(x), targetReturn(x, target), longOnly(x), ...),
                  trace = FALSE) {
  
  # quadratic coefficients
  size <- ncol(x)
  c <- rep(0, size)
  Q <- Sigma
  
  # optimization
  sol <- QP_solver(c, Q, cstr, trace)
  
  # extract weights
  weights <- sol$solution
  names(weights) <- colnames(x)
  weights
}

## Exemplo - retorno alvo = média dos retornos
w <- MV_QP(x, mean(x))
round(w,4)
## restrições atendidas?
all.equal( c( t(w)%*%col_means(x) ) , mean(x) )   # retorno alvo
all.equal( sum(w) , 1 )                           # investimento total 
# sum(w) == 1
all(w >= 0)                                       # long only

## gráfico com os pesos de cada ativo
barplot(w, ylim = c(0, 1), las = 2, main = "Test MV Implementation")

## modelo de média-variância
t(w)%*%cov(x)%*%w   # covariância
t(w)%*%col_means(x) # retorno

## modelo de pesos uniformes
u = rep(1/10,10)
t(u)%*%cov(x)%*%u   # covariância
t(u)%*%col_means(x) # retorno

## Exemplo - retorno alvo = retorno médio mínimo
which(col_means(x)==min(col_means(x)))

w <- MV_QP(x, min(col_means(x)))
round(w, 4)

## gráfico com os pesos de cada ativo
barplot(w, ylim = c(0, 1), las = 2,
        main = "Smallest Portfolio Return")

## restrições atendidas?
all.equal( c(t(w)%*%col_means(x)) , min(col_means(x)) )  # retorno alvo
all.equal(sum(w) , 1)                                    # investimento total 
all(round(w,4) >= 0)                                     # long only
# all(w >= 0)

## Exercício - retorno alvo = retorno médio máximo
## Repita a análise do exemplo anterior fixando o retorno alvo
## como o retorno médio individual de maior valor.
## O que acontece com a alocação dos pesos?

## Exemplo - restrições de grupo
w <- MV_QP(x, mean(x), Sigma = covMcd(x)$cov, GroupBudget())

## gráfico com os pesos de cada ativo
barplot(w, ylim = c(0, 1), las = 2,
        main = "MV with Budget Constraints")
## Comparação de pesos
w1 <- MV_QP(x, mean(x), Sigma = cov(x))         # método clássico
w2 <- MV_QP(x, mean(x), Sigma = covMcd(x)$cov)  # método robusto

## Gráficos
par(mfrow=c(1,2))
barplot(w1, ylim = c(0, 1), las = 2, main = "MV with classical cov")
barplot(w2, ylim = c(0, 1), las = 2, main = "MV with robust cov")


## Modelo de Mínima Variância
w <- MV_QP(x, cstr = c(fullInvest(x), longOnly(x)))
## gráfico com pesos
barplot(w, ylim = c(0, 1), las = 2, main = "Minimum Variance Portfolio")

