## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 18 - Alocação de Portfólios

## instalar pacotes
# install.packages("Rglpk")
# install.packages("quadprog")
# install.packages("Rsolnp")


## Otimização linear

## carregar pacote
require(Rglpk)

## Função para executar a otimização linear
LP_solver <- function(c, cstr = list(), trace = FALSE) {
Aeq <- Reduce(rbind, cstr[names(cstr) %in% "Aeq"])
aeq <- Reduce(c, cstr[names(cstr) %in% "aeq"])
A <- Reduce(rbind, cstr[names(cstr) %in% "A"])
a <- Reduce(c, cstr[names(cstr) %in% "a"])

sol <- Rglpk_solve_LP(obj = c, mat = rbind(Aeq, A), 
                      dir = c(rep("==", nrow(Aeq)), rep(">=", nrow(A))),
                      rhs = c(aeq, a), verbose = trace)

status <- sol$status
solution <- if (status) rep(NA, length(c)) else sol$solution
list(solution = solution, status = status)
}


## Otimização quadrática

## carregar pacote
require(quadprog)

## Função para executar a otimização quadrática
QP_solver <- function(c, Q, cstr = list(), trace = FALSE) {
  Aeq <- Reduce(rbind, cstr[names(cstr) %in% "Aeq"])
  aeq <- Reduce(c, cstr[names(cstr) %in% "aeq"])
  A <- Reduce(rbind, cstr[names(cstr) %in% "A"])
  a <- Reduce(c, cstr[names(cstr) %in% "a"])
  
  sol <- try(solve.QP(Dmat = Q,dvec = -2*c, Amat = t(rbind(Aeq, A)), bvec = c(aeq, a),
                      meq = nrow(Aeq)), silent = TRUE)
  if (trace)
    cat(sol)
  if (inherits(sol, "try-error"))
    list(solution = rep(NA, length(c)), status = 1)
  else
    list(solution = sol$solution, status = 0)
}


## Otimização não-linear

## carregar pacote
require(Rsolnp)

NLP_solver <- function(par, f, cstr = list(), trace = FALSE) {
  Aeq <- Reduce(rbind, cstr[names(cstr) %in% "Aeq"])
  aeq <- Reduce(c, cstr[names(cstr) %in% "aeq"])
  A <- Reduce(rbind, cstr[names(cstr) %in% "A"])
  a <- Reduce(c, cstr[names(cstr) %in% "a"])
  heq <- Reduce(c, cstr[names(cstr) %in% "heq"])
  h <- Reduce(c, cstr[names(cstr) %in% "h"])
  leqfun <- c(function(par) c(Aeq %*% par), heq)
  eqfun <- function(par)
    unlist(lapply(leqfun, do.call, args = list(par)))
  eqB <- c(aeq, rep(0, length(heq)))
  
  lineqfun <- c(function(par) c(A %*% par), h)
  ineqfun <- function(par)
    unlist(lapply(lineqfun, do.call, args = list(par)))
  ineqLB <- c(a, rep(0, length(h)))
  ineqUB <- rep(Inf, length(ineqLB))
  
  sol <- solnp(par = par, fun = f, eqfun = eqfun, eqB = eqB,
               ineqfun = ineqfun, ineqLB = ineqLB, ineqUB = ineqUB,
               control = list(trace = trace))
  
  status <- sol$convergence
  solution <- if (status) rep(NA, length(par)) else sol$pars
  list(solution = solution, status = status)
}


## Bases de dados

## Indices de ações européias
data("EuStockMarkets")
head(EuStockMarkets)

## Indice NASDAQ Bank
file = "../datasets/IXIC.csv"
Nas = read.csv(file, colClasses = c("Date", rep("numeric",6)))

head(Nas)

## Retornos

x <- apply(EuStockMarkets, MAR=2,
           function(x) x[-1] / x[-length(x)] - 1)

## Função para calcular o valor do portfólio
pftPerf <- function(x, w, W0 = 1000) {
  W0 * cumprod(c(1, 1 + x %*% w))
}

## Gráfico do valor do portfólio
nc <- ncol(x)
w <- rep(1/nc, nc)  # pesos uniformes
plot(1:nrow(EuStockMarkets), pftPerf(x, w), type = "l",
     main = "Portfolio Cumulated Performance",
     xlab = "Date", ylab = "Wealth")

## Exercício: fazer para os dados da NASDAQ

