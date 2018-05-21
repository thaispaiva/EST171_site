## EST171 - Métodos Computacionais para Análise de Risco
## Código da aula 18 - Alocação de Portfólios

## instalar pacotes
# install.packages("Rglpk")

## carregar pacotes
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

